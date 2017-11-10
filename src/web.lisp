(in-package :cl-user)
(defpackage pathwayz.web
  (:use :cl
        :caveman2
        :pathwayz.config
        :pathwayz.view
        :pathwayz.db
        :pathwayz.board
        :datafly
        :sxql)
  (:export :*web*))
(in-package :pathwayz.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(defvar *game-count* 0)
(defvar *games* (make-hash-table))

(defun next-game-number ()
  (incf *game-count*)
  *game-count*)


;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

(defroute "/new" ()
  (let* ((game-number (next-game-number))
         (game (make-board)))
    (setf (gethash game-number *games*) game)
    (render #P"newgame.html" 
            `(:game_number ,game-number))))

(defroute "/view/:number/:color_name" (&key number color_name)
  (let* ((color (intern (string-upcase color_name) "KEYWORD"))
         (game-number (parse-integer number))
         (board (gethash game-number *games*)))
    (render #P"viewgame.html"
            `(:game_number ,game-number
              :color ,color
              :board_color ,(current-player board)
              :current_player ,(eq (current-player board) color)
              :game_won ,(game-won-p board)
              :board ,(loop for y from 0 upto 7
                            with contents = (board-contents board)
                            collect (loop for x from 0 upto 11
                                          collect (let ((c (aref contents x y)))
                                                    (list :color (car c) 
                                                          :perm (cdr c) 
                                                          :x x :y y))))
              :turn_number ,(turn-count board)))))

; Cause a player move to happen, then display result.
(defroute "/move/:number/:color_name/:x_str/:y_str/:permanent" 
          (&key number color_name x_str y_str permanent)
  (let* ((game-number (parse-integer number))
         (color (intern (string-upcase color_name) "KEYWORD"))
         (x (parse-integer x_str))
         (y (parse-integer y_str))
         (p (string= permanent "true"))
         (board (gethash game-number *games*)))
    (unless (eq color (current-player board))
      (error "Wrong player can't make move. ~A -- ~A" color (current-player board)))
    (let ((move-result (move board x y p)))
      (unless move-result
        (error "Invalid move, try again."))
      (redirect (format nil "/view/~A/~A" game-number color)))))

(defroute "/turn/:number" (&key number)
  (let* ((game-number (parse-integer number))
         (board (gethash game-number *games*)))
    (list 200 '() (list (format nil "~A" (turn-count board))))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
