(cl:in-package #:gui-sudoku)

(defvar *digit-menu*)
(setq *digit-menu* nil)
(defvar *square*)
(setq *square* nil)

(defvar *automatic* nil)

(defun toggle-digit-menu ()
  (setq *digit-menu* (not *digit-menu*)))

(defmacro with-html-output-br (stream expr)
  `(with-html-output (,@stream nil :indent t)
       ,expr))
     
;;; For now, we can handle only one game at a time.  This game is
;;; stored in the special variable *GAME*. It would be better to
;;; support different sessions, so that when the URI "/sudoku" (without
;;; a coor following), a new session is created. 

;;; Main entry point.  When this function is called, the acceptor goes
;;; into an infinite loop serving requests.  If you still want to be
;;; able to evaluate forms, run this function in its own thread.

(defun start-acceptor (port)
  (setf hunchentoot::*supports-threads-p* nil)
  (start (make-instance 'hunchentoot:easy-acceptor
			      :port port
			      :document-root ".")))

(define-condition quit-condition (condition)
  ())

(defun sudoku (&optional (port 5050))
  (init-sudoku)
  (setq *game* (game-with-new-grid))
  (format *trace-output* "Visit page http://localhost:~A~%" port)
  (handler-case
      (start-acceptor port)
    (quit-condition (c)
      (declare (ignore c)))))

(defgeneric active-p (game))
(defmethod active-p ((game game))
  (not (game-over game)))

(defgeneric play-button-p (player game))
(defmethod play-button-p ((player integer) (game game))
  (not (game-over game)))
  
(defun html-digit-item (coor digit)
  (with-html-output-br (*standard-output*)
    (:li
     (:a
      :href
      (format nil "digit-choice?x=~A&y=~A&digit=~A" (x-coor coor) (y-coor coor) digit)
      (str (format nil "~A" digit))))))

(defgeneric html-digit-input (square))
(defmethod html-digit-input ((square square))
  (let ((coor (coor square)))
    (with-html-output-br (*standard-output*)
      (:form
       :action (format nil "digit-choice?&x=~A&y=~A" (x-coor coor) (y-coor coor))
       :method "post"
       :autocomplete "off"
       (:input
	:type "text"
	:name "digit"
	:size "1")))))

(defgeneric html-digit-menu (square))
(defmethod html-digit-menu ((square square))
  (with-html-output-br (*standard-output*)
    (:nav
     (:ul
      (:li (:a :href "#" "?")
	   (:ul 
	    (loop for digit in (possible-digits square)
		  do (html-digit-item (coor square) digit))))))))

(defgeneric html-protected-digit (digit-string))
(defmethod html-protected-digit ((digit-string string))
  (with-html-output-br (*standard-output*)
    (:div :class "digit"
	  (:div :class "protected" (str digit-string)))))

(defgeneric html-unprotected-digit (digit-string))
(defmethod html-unprotected-digit ((digit-string string))
  (with-html-output-br (*standard-output*)
    (:div :class "digit" (str digit-string))))

(defgeneric html-digit (square))
(defmethod html-digit ((square square))
  (let ((digit-string (format nil "~A" (digit square))))
    (if (protected square)
	(html-protected-digit digit-string)
	(html-unprotected-digit digit-string))))

(defun html-br ()
  (with-html-output-br (*standard-output*)
    (:br)))

(defun html-hr ()
  (with-html-output-br (*standard-output*)
    (:hr)))

(defgeneric html-square (square))
(defmethod html-square ((square square))
  (with-html-output-br (*standard-output*)
    (:div :class "square"
	  (if (assigned-p square)
	      (html-digit square)
	      (if *digit-menu*
		  (html-digit-menu square)
		  (html-digit-input square))))))

(defgeneric html-tdd-square (square))
(defmethod html-tdd-square ((square square))
  (with-html-output-br (*standard-output*)
    (:td :class "tdd" (html-square square))))

(defun html-sqrt-line (squares zone line)
  (with-html-output-br (*standard-output*)
    (:tr
     (loop
       for column from 0 below *sqrt-size*
       do (html-tdd-square (coor-square squares (rcoor-to-coor zone (make-coor line column))))))))

(defun html-zone (squares i j)
  (with-html-output-br (*standard-output*)
    (:td
     (:table
      :class "zone"
      (loop for line from 0 below *sqrt-size*
	    do (html-sqrt-line squares (zcoor-to-zone (make-coor i j)) line))))))

(defgeneric html-zones-line (squares i))
(defmethod html-zones-line ((squares squares) (i integer))
  (with-html-output-br (*standard-output*)
    (:tr
     (loop for j from 0 below *sqrt-size*
	   do (html-zone squares i j)))))

(defgeneric html-squares (squares))
(defmethod html-squares ((squares squares))
  (with-html-output-br (*standard-output*)
    (:div
     :align "center"
     (:table
      :class "squares"
      (loop for i from 0 below *sqrt-size*
	    do (html-zones-line squares i))))))

(defmacro page-template ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent nil)
     (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
            (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
                   (:title (str ,title))
                   (:link :rel "stylesheet" :type "text/css" :href "sudoku.css")
		   ;; (:link :rel  "stylesheet" :href "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css")
		   ;; (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js")
		   ;; (:script :src "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"))
)
            (:body ,@body))))

(defgeneric play (game))
(defmethod play ((game game))
  (game-do game (next-move game)))

(defun place-chosen-digit (digit x y)
  (setq x (parse-integer x))
  (setq y (parse-integer y))
  (game-do *game* (make-square (make-coor x y) (parse-integer digit))))

(defun html-main-menu ()
  (with-html-output-br (*standard-output*)
    (:div
     :align "center"
     (:nav
      (:a :href "/quit" (:button "Quit")) ""
      (:a :href "/restart" (:button "Restart"))
      (:a :href "/new-grid" (:button "New grid")) "")
      )))

(defun html-play-menu ()
  (with-html-output-br (*standard-output*)
    (:div :align "center"
    (:nav
      (:a :href "/play" (:button "Play")) ""
      (:a :href "/toggle-digit-menu" (:button (str (if *digit-menu* "digit-input"  "digit-menu"))))
      ))))

(defun test-nav2 ()
  (with-html-output-br (*standard-output*)
    (:nav
     (:ul
      (:li (:a :href "#" "Play")
	   (:ul
	    (:li (:a :href "#" "One step"))
	    (:li (:a :href "#" "Brute Force "))
	    (:li (:a :href "#" "D3"))))))))

(defun html-entete ()
  (with-html-output-br (*standard-output*)
    (:h2 :align "center" "Let's play Sudoku")))
  
(defun html-game-over ()
  (let ((string 
	 (format
	  nil
	  "Game over: you have ~A!" 
	  (if (solved-p *game*) "won" "lost"))))
    (with-html-output-br (*standard-output*)
      (:h2 :align "center" (str string)))))
  
(defgeneric echo (object))
(defmethod echo ((game game))
  (html-entete)
  (html-main-menu)
  (html-hr)
  (html-squares (game-squares game))
  (html-hr)
  (if(active-p *game*)
     (html-play-menu)
     (html-game-over)))

(hunchentoot:define-easy-handler (game-handler :uri "/sudoku") ()
  (page-template (:title "Sudoku")
    (echo *game*)))

(hunchentoot:define-easy-handler (quit-handler :uri "/quit") ()
  (signal 'quit-condition))

(hunchentoot:define-easy-handler (restart-handler :uri "/restart") ()
  (init-game *game*)
  (redirect "/sudoku"))

(hunchentoot:define-easy-handler (new-game-handler :uri "/new-grid") ()
  (setq *game* (game-with-new-grid))
  (redirect "/sudoku"))

(hunchentoot:define-easy-handler (front-page :uri "/") ()
  (redirect "/restart"))

(hunchentoot:define-easy-handler (play-handler :uri "/play") ()
  (play *game*)
  (redirect "/sudoku"))

(hunchentoot:define-easy-handler
    (digit-choice-handler :uri "/digit-choice")
    (x y digit)
  (place-chosen-digit digit x y)
  (redirect "/sudoku"))

(hunchentoot:define-easy-handler (toggle-digit-menu-handler :uri "/toggle-digit-menu") ()
  (toggle-digit-menu)
  (redirect "/sudoku"))
