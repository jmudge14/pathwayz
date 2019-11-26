; Code to teach Pathwayz how to play. Yay!!

(in-package :cl-user)
(defpackage pathwayz.learn
  (:use :cl :pathwayz.sigmoid :pathwayz.board :alexandria)
  (:import-from :pathwayz.config
                :config)

  (:export )) ; fill in export later
(in-package :pathwayz.learn)

(defun random-move-count ()
  "A nonlinear distribution biased toward low move counts."
  (let ((s (random 100)))
    (cond ((<= s 10) 3)
          ((<= s 15) 4)
          ((<= s 20) 5)
          ((<= s 25) 6)
          ((<= s 30) 7)
          ((<= s 35) 8)
          ((<= s 40) 9)
          ((<= s 50) 10)
          ((<= s 60) 15)
          ((<= s 80) 20)
          ((<= s 90) 25)
          ((<= s 95) 30)
          (t 35))))


(defvar *net*)
(setf *net* (make-network '(290 75 50 25 1)
                          :default-transfer-function 'relu
                          :default-derivative-function 'relu-d
                          :max-random-bias 0.01d0
                          :max-random-weight 0.01d0))
(defvar *games-played* 0)
(defvar *board* nil)

(defun new-board ()
  "Update the global board with a new one, and relevant variable updates."
  (setf *board* (make-board))
  (incf *games-played*))

(defun same-color (color1 color2)
  (if (eq color1 color2) 1.0 0.0))

(defun get-inputs ()
  "Return input values for current board positions.
   Total: 290 inputs (= (+ 2 (* 12 8 3)))"
  (let ((contents (board-contents *board*))
        (result nil))
    (dotimes (x 12)
      (dotimes (y 8)
         (let* ((piece (aref contents x y))
                (color (car piece))
                (permanent (cdr piece)))
           (push (same-color :black color) result) ; black active
           (push (same-color :white color) result) ; white active
           (push (if permanent 1.0 0.0) result)))) ; permanent or not
    ; Marker for current player
    (push (same-color :black (current-player *board*)) result)
    (push (same-color :white (current-player *board*)) result)
    result))


(defun read-input (inputs)
  (first (propagate *net* inputs)))

(defun backprop-input (inputs val)
  (back-propagate *net* (list val) inputs))


(defun get-move-scores ()
  "Get all legal moves, sorted in score order according to current player"
  (let* ((moves (random-shuffle (legal-moves *board*)))
         (move-scores (map 'list (lambda (m)
                                   (let ((x (first m))
                                         (y (second m))
                                         (perm (third m)))
                                     (let ((inputs (prog2 (move *board* x y perm)
                                                          (get-inputs)
                                                          (undo-move *board*))))
                                       (list (read-input inputs)
                                             inputs
                                             m))))
                           moves)))
    (setf move-scores 
          (sort move-scores
                (if (eq (current-player *board*) :black)
                  #'< #'>)
                :key #'first))
    move-scores))

(defun find-best-move ()
  (let ((possible-moves (get-move-scores)))
    (first possible-moves)))

(defun random-shuffle (sequence)
  (map-into sequence #'car
            (sort (map 'vector (lambda (x)
                                 (cons x (random 1d0)))
                       sequence)
                  #'< :key #'cdr)))

(defun training-loop ()
  (loop (setf *print-all-moves* nil) 
        (loop repeat 10 do 
              (train-game)
              (pretty-print-board *board*))
        (setf *print-all-moves* t)
        (train-game)
        (pretty-print-board *board*)))

(defvar *random-win-count* 0)
(defvar *print-all-moves* t)
(defun train-game ()
  (new-board)
  (let ((last-white-move nil)
        (last-black-move nil))
    ; To add some stochasticness to the game, play some random moves first
    (loop repeat (* 2 (random-move-count)) do  ; even so white ends up playing first
          (apply #'move *board* (random-move)))
    
    (when (game-won-p *board*) 
      (format t "Random moves ended game.~%")
      (return-from train-game))

    ; move white - first "real" turn
    (setf last-white-move (list 0.0 '() (random-move)))
    (when *print-all-moves* 
      (format t "Next move: ~A ~A~%" (third last-white-move) (current-player *board*)))
    (apply #'move *board* (third last-white-move))

    (when (game-won-p *board*) 
      (format t "Game ended too soon for training.")
      (return-from train-game))

    ; move black - first "real" turn
    (setf last-black-move (list 0.0 '() (random-move)))
    (when *print-all-moves* 
      (format t "Next move: ~A ~A~%" (third last-black-move) (current-player *board*)))
    (apply #'move *board* (third last-black-move)) 

    (when *print-all-moves* (pretty-print-board *board*))

    ; if the above moves somehow won the game, end here.
    (when (game-won-p *board*)
      (format t "~%**********~%GAME WON ON INITIAL TURNS. NUMBER: ~A**********~%" 
              (incf *random-win-count*)))

    ; Train on moves until game is won
    (loop for color = (current-player *board*) then (current-player *board*)
          until (game-won-p *board*) do
          ; move and train this color
          (let ((current-move (find-best-move)))
              (when (null current-move) ; tie
                (format t "TIE -- buzzing.~%")
                (buzz *net* 100)
                (return-from train-game))
              (when *print-all-moves* 
                (format t "*Next move: ~A ~A~%" (third current-move) (current-player *board*)))
              (apply #'move *board* (third current-move))
              (when *print-all-moves* (pretty-print-board *board*))
              (let ((inp (list (read-input (get-inputs)))))
                (cond ((eq color :white)
                       (back-propagate *net* inp (second last-white-move))
                       (setf last-white-move current-move))
                      (t (back-propagate *net* inp (second last-black-move))
                       (setf last-black-move current-move))))))
    
    ; Train the winning board position
    (format t "Winner: ~A~%" (game-won-p *board*))
    (back-propagate *net* 
                    (list (if (eq :black (game-won-p *board*)) 0.0 1.0))
                    (get-inputs)
                    0.9d0
                    )))


(defun random-move ()
  (first (coerce (random-shuffle (legal-moves *board*)) 'list)))

(defun generate-random-winning-game ()
  (loop 
    (let ((*board* (make-board)))
      (loop for move = (random-move) then (random-move)
            for move-count = 0 then (1+ move-count)
            collect move into moves
            do 
            (unless move
              (return nil))
            (apply #'move *board* move)
            (if (game-won-p *board*)
              (return-from generate-random-winning-game
                           (list move-count
                                 (game-won-p *board*)
                                 moves)))))))

(defun train-last-n-moves (last-n winner movelist)
  "Trains on the last-n moves of movelist, including the winner"
  (let ((*board* (make-board)))
    (terpri)
    ; skip training on all but last-n moves
    (loop until (= last-n (length movelist))
          for move = (pop movelist)
          do (format t ".")
             (apply #'move *board* move))
    ; train winner for remainder of game
    (loop until (null movelist)
          with val = (list (case winner (:black 0.0d0) (:white 1.0d0)))
          for move = (pop movelist)
          do (format t "@")
             (apply #'move *board* move)
             (back-propagate *net* val (get-inputs)))))

(defun n-or-less-games (n maxtries)
  (loop repeat maxtries
        for g = (generate-random-winning-game)
        if (<= (first g) n) collect g))

(defun train-random-games (&optional (batch-size 10000) (game-length 60) (last-n 10) (max-batches 1))
  (loop for i = 1 then (1+ i) 
        until (> i max-batches) do
    (format t "~A Batch of ~A. ~%" i batch-size)
    (loop for g in (n-or-less-games game-length batch-size) do 
          (train-last-n-moves last-n (second g) (third g)))
    ; Conventional game to observe current state
    (terpri)
    (train-game)
    (pretty-print-board *board*)))


#|
(defun play-computer-game (num)
     "Development tool - connect current net to an existing board on the Pathwayz web service"
     (new-board)
     (setf (gethash num pathwayz.web::*games*) *board*)
     (block :a
            (loop (update-inputs) 
                  (loop until (eq (current-player *board*) :black)
                        do (if (game-won-p *board*) (return-from :a nil)))
                  (apply #'move *board* (rest (print (find-best-move)))))))

|#
