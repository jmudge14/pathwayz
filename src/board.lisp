;;;; Models a Pathwayz gameboard
;;;; Game concept and rules, credit where due: http://pathwayzgame.com/

(in-package :cl-user)
(defpackage pathwayz.board
  (:use :cl)
  (:import-from :pathwayz.config
                :config)
  (:export :make-board
           :game-won-p
           :move
           :current-player
           :board-contents
           :turn-count
           :toggle-color
           :board
           :legal-moves
           :undo-move
           :pretty-print-board))
(in-package :pathwayz.board)

(defclass board ()
  ((contents :accessor board-contents 
             :initform (make-array '(12 8) :initial-element nil))
   (player :accessor current-player
           :initform :white)
   (won :initform nil)
   (turn-count :accessor turn-count
               :initform 0)
   (last-move :initform nil)))

(defun make-board ()
  (make-instance 'board))

(defparameter directions
    '((-1 . -1) (-1 . 0) (-1 . 1)
      ( 0 . -1)          ( 0 . 1)
      ( 1 . -1) ( 1 . 0) ( 1 . 1))
    "All ordinal directions as coordinate offsets")

(defmacro do-offsets (directions x y &body body)
  (let ((offset (gensym)))
    `(dolist (,offset ,directions)
       (let ((cx (+ ,x (car ,offset)))
             (cy (+ ,y (cdr ,offset))))
         (when (and (>= cx 0)
                    (>= cy 0)
                    (<= cx 11)
                    (<= cy 7))
           ,@body)))))

(defun toggle-color (color)
  (if (eq color :white)
    :black
    :white))

(defmethod toggle-player ((b board))
  (with-slots (player) b
    (setf player (toggle-color player))))

(defmethod move ((b board) x y permanent)
  "Perform a game move. Game moves mean placing a piece, either permanent or not,
   on any empty board position, then if permanent, flipping all surrounding
   non-permanent piece colors"
  (with-slots (contents player won last-move) b
    (when (or (aref contents x y)
              won)
      (return-from move nil))
    (setf (aref contents x y)
          (cons player permanent))
    (if permanent
      (progn
        (setf (aref contents x y)
              (cons (toggle-color player) t))
        (symbol-macrolet ((piece (aref contents cx cy)))
          (do-offsets directions x y
            (when (and piece 
                       (not (cdr piece)))
              (setf piece (cons (toggle-color (car piece)) 
                                nil))))))
      (setf (aref contents x y)
            (cons player nil)))
    (setf won (game-won-p b))
    (unless won (toggle-player b))
    (incf (turn-count b)) 
    (setf last-move (list x y permanent))
    t))

(defmethod undo-move ((b board))
  "Perform a game move. Game moves mean placing a piece, either permanent or not,
   on any empty board position, then if permanent, flipping all surrounding
   non-permanent piece colors"
  (with-slots (contents player won last-move) b
    (let ((x (first last-move))
          (y (second last-move))
          (permanent (third last-move)))
      (setf (aref contents x y) nil) ; clear piece
      (unless won (toggle-player b))
      (decf (turn-count b)) 
      (setf last-move nil) 
      (if permanent
        (symbol-macrolet ((piece (aref contents cx cy)))
          (do-offsets directions x y
                      (when (and piece 
                                 (not (cdr piece)))
                        (setf piece (cons (toggle-color (car piece)) 
                                          nil))))))
      (setf won nil) ; if undoing a move, they can't have won anymore
      ) 
    t))

(defun path-exists-p (board color)
  "Check whether a path exists for a given color from board edge to board edge."
  (with-slots (contents) board
    (let ((filled (make-array '(12 8) :initial-element nil)))
      ; begin with first column squares with matching color
      (dotimes (i 8)
            (when (eq (car (aref contents 0 i)) color)
              (setf (aref filled 0 i) t)))
      ; flood fill matching like color for each option on first column
      (labels ((flood (x y)
                 (when (= x 11)
                   (return-from path-exists-p t))
                 (do-offsets directions x y
                   (when (and (eq color (car (aref contents cx cy)))
                              (not (aref filled cx cy)))
                     (setf (aref filled cx cy) t)
                     (flood cx cy)))))
        (dotimes (i 8)
          (when (aref filled 0 i)
            (flood 0 i)))))))

(defun game-won-p (board)
  "Determine the win state of the game, with ties broken by current player."
  ; preemptive check if we already found a winner
  (with-slots (won) board
    (when won
      (return-from game-won-p won)))
  ; full check for win status
  (let ((b (path-exists-p board :black))
        (w (path-exists-p board :white)))
    (cond ((and b w)
           (current-player board)) ; current player wins ties
          (b :BLACK)
          (w :WHITE)
          (t nil))))

(defun legal-moves (board)
  (with-slots (contents current-player) board
    (let ((result nil))
      (dotimes (x 12)
        (dotimes (y 8)
          (unless (aref contents x y)
            ; (list x y PERM)
            (push (list x y nil) result)
            (push (list x y t) result))))
      result)))

(defun pretty-print-board (board)
  "Display a board nicely, as a grid.
   Colors are represented as Q/@ for white, S/$ for black, regular and permanent respectively."
  (let* ((contents (board-contents board)))
    (dotimes (y 8)
      (dotimes (x 12)
        (let* ((m (aref contents x y))
               (c (car m))
               (p (cdr m)))
          (write-char (case c
                        (:white (if p #\@ #\Q))
                        (:black (if p #\$ #\S))
                        (t #\.)))))
        (terpri))))
