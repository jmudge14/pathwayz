; Code to teach Pathwayz how to play. Yay!!

(in-package :cl-user)
(defpackage pathwayz.learn
  (:use :cl)
  (:import-from :pathwayz.config
                :config)

  (:import-from :pathwayz.sigmoid
                :make-network
                :perceive
                :back-propagate)

  (:import-from :pathwayz.board
                :make-board
                :game-won-p
                :move
                :current-player
                :board-contents
                :turn-count
                :board
                :legal-moves)
  
  (:import-from :cl-utilities
                :copy-array)

  (:export )) ; fill in export later
(in-package :pathwayz.learn)



(defvar *net*)
(setf *net* (make-network '(110 500 20 1) :weight 0.005 :bias 0.001))
(defvar *games-played* 0)
(defvar *board* nil)
(defvar *non-backprop-moves* 0)

; Reusable array to speed up computations
(defvar *inputs* (make-array '(110)))
(defvar *board-places* (make-array '(12 8) :displaced-to *inputs*))
(defvar *next-move-x* (make-array '(12) :displaced-to *inputs* :displaced-index-offset 96))
(defvar *next-move-y* (make-array '(8) :displaced-to *inputs* :displaced-index-offset 108))
(defvar *next-move-piece* (make-array '(1) :displaced-to *inputs* :displaced-index-offset 109))

(defun new-board ()
  "Update the global board with a new one, and relevant variable updates."
  (setf *board* (make-board)
        *non-backprop-moves* (random (cond ((<= *games-played* 100) 10)
                                           ((<= *games-played* 400) 5)
                                           (t 1))))
  (incf *games-played*))

(defun update-inputs ()
  "Update *inputs* for current board positions"
  (let ((contents (board-contents *board*)))
    (dotimes (x 12)
      (dotimes (y 8)
        (let* ((piece (aref contents x y))
               (color (car piece))
               (permanent (cdr piece))
               (piece-value (cond ((not color) 0.0)
                                  (permanent 1.0)
                                  (t 0.5)))
               (board-value (case color
                              (:white piece-value)
                              (:black (- piece-value)))))
          (setf (aref *board-places* x y) board-value))))))

(defun set-next-move (x y permanent)
  "Update input array to the given next move. Use nil nil nil to reset."
  (dotimes (cx 12)
    (setf (aref *next-move-x* cx)
          (if (and x (= cx x)) 1.0 0.0)))
  (dotimes (cy 8)
    (setf (aref *next-move-y* cy)
          (if (and y (= cy y)) 1.0 0.0)))
  (setf (aref *next-move-perm* 0)
        (if permanent 1.0 0.0))
  (setf (aref *next-move-white* 0)
        (case (current-player *board*) (:white 1.0) (:black 0.0)))
  (setf (aref *next-move-black* 0)
        (case (current-player *board*) (:white 0.0) (:black 1.0))))


(defun read-input ()
  (first (perceive *net* *inputs*)))

(defun backprop-input (inputs val &key (eta 0.1))
  (back-propagate *net* inputs (list val) :eta eta))


(defun get-move-scores ()
  "Get all legal moves, sorted in score order according to current player"
  (let* ((moves (legal-moves *board*))
         (move-scores (map 'list (lambda (m)
                                   (let ((x (first m))
                                         (y (second m))
                                         (perm (third m)))
                                     (set-next-move x y perm)
                                     (cons (read-input) m)))
                           moves)))
    (setf move-scores 
          (sort move-scores
                (if (eq (current-player *board*) :black)
                  #'< #'>)
                :key #'first))
    move-scores))

(defun find-best-move ()
  (first (get-move-scores)))

(defun random-shuffle (sequence)
  (map-into sequence #'car
            (sort (map 'vector (lambda (x)
                                 (cons x (random 1d0)))
                       sequence)
                  #'< :key #'cdr)))

(defun train-game (intended-winner random-move-seed)
  (format t "~%New game, no. ~A~%" *games-played*)
  (new-board)
  (let ((played-moves nil)
        (random-move-count (random random-move-seed)))
    ; play some number of untrained moves
    (format t "Initial Random Moves: ~A~%" random-move-count)
    (dotimes (mn random-move-count)
      (let ((m (first (coerce (random-shuffle (legal-moves *board*)) 'list))))
        (push m played-moves)
        (apply #'move *board* m)))
    ;(format t "Computer's first move for player ~A~%" (current-player *board*))
    (format t "Computer's goal: ~A~%" intended-winner)
    ; play until game end - train each move
    (format t "Playing to end of game...~%")
    (update-inputs)
    (loop until (game-won-p *board*) do
          (update-inputs)
          (let* ((move (find-best-move)))
            (unless move 
              (format t "Game was a tie, not training.~%~%")
              (return-from train-game nil))
            ;(format t "Move: ~A ~A's turn~%" move (current-player *board*))
            (push (rest move) played-moves)
            (apply #'move *board* (rest move))))
    (pretty-print-board *board*)
    (unless (eq (game-won-p *board*)
                intended-winner)
      (format t "Game lost by computer. Not training.~%")
      (return-from train-game nil))
    (format t "~A won. Training...~%" (game-won-p *board*))
    (let ((train-score (case (game-won-p *board*)
                             (:white 1.0)
                             (:black 0.0)))
          (train-eta (* 0.05
                        (- 96 (length played-moves))))
          (train-moves (reverse played-moves))
          (*board* (make-board)))
      (dotimes (n random-move-count)
        (apply #'move *board* (pop train-moves)))
      (loop for m in train-moves do
            (update-inputs)
            (apply #'set-next-move m)
            (backprop-input *inputs* train-score :eta train-eta))
      t)))



(defun train-game-old ()
  (format t "~%New game, no. ~A~%" *games-played*)
  (new-board)
  ; play some number of untrained moves
  (let ((random-move-count (random 20)))
    (format t "Initial Random Moves: ~A~%" random-move-count)
    (dotimes (mn random-move-count)
      (let ((m (first (coerce (random-shuffle (legal-moves *board*)) 'list))))
        (apply #'move *board* m))))
  ;(format t "Computer plays ~A~%" (current-player *board*))
  ; play until game end - train each move
  (update-inputs)
  (format t "Playing to end of game...~%")
  (loop until (game-won-p *board*) do
        (update-inputs)
        (let* ((move (find-best-move))
               save-inputs)
          (unless move (return-from train-game-old nil))
          ;(format t "Move: ~A ~A's turn~%" move (current-player *board*))
          (apply #'set-next-move (rest move))
          (setf save-inputs (copy-array *inputs*))
          (apply #'move *board* (rest move))
          (if (game-won-p *board*)
            (cond ((eq (current-player *board*) (game-won-p *board*))
                   (format t "Winning move for current player. Rewarding.")
                   (backprop-input save-inputs
                                   (case (current-player *board*)
                                     (:white 1.0)
                                     (:black 0.0))
                                   :eta 1.5))
                  (t (format t "Made opponent win. Gently encouraging different behavior.")
                     (backprop-input save-inputs
                                     (case (current-player *board*)
                                       (:white 0.0)
                                       (:black 1.0))
                                     :eta 1.5)))
            (progn (update-inputs)
                   (set-next-move nil nil nil)
                   (backprop-input save-inputs 
                                   (read-input) 
                                   :eta 0.1)))))
  (pretty-print-board *board*))


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

(defun training-loop ()
  (loop (loop for i = 10 then (1+ i)
              until (train-game :white i))
        (loop for i = 10 then (1+ i)
              until (train-game :black i))))


(defun play-computer-game (num)
     "Development tool - connect current net to an existing board on the Pathwayz web service"
     (new-board)
     (setf (gethash num pathwayz.web::*games*) *board*)
     (block :a
            (loop (update-inputs) 
                  (loop until (eq (current-player *board*) :black)
                        do (if (game-won-p *board*) (return-from :a nil)))
                  (apply #'move *board* (rest (print (find-best-move)))))))


