;;;concatenation of files

;;;api.lisp
(defvar *sqrt-size* 3 "side of the side of a zone")
(defvar *size* (* *sqrt-size* *sqrt-size*))
(defvar *nb-squares* (* *size* *size*))
(defvar *game* nil "the current instance of a game")
(defvar *new-grids-9* (list "./Grids/9x9-1.sudoku" "./Grids/9x9-2.sudoku" "./Grids/9x9-3.sudoku" "./Grids/9x9-9.sudoku" "./Grids/1.sudoku" "./Grids/2.sudoku" "./Grids/4.sudoku" "./Grids/5.sudoku" "./Grids/6.sudoku"))
(defvar *new-grids-result-9* (list "./Grids/9x9-1-result.sudoku" "./Grids/9x9-2-result.sudoku" "./Grids/9x9-3-result.sudoku" "./Grids/9x9-9-result.sudoku" "./Grids/1-result.sudoku" "./Grids/2-result.sudoku" "./Grids/4-result.sudoku" "./Grids/5-result.sudoku" "./Grids/6-result.sudoku"))


(defun set-possible-digits()
  (labels ((set-possible-digits-bis(l cpt)
	     (if (eq cpt 0)
		 l
		 (set-possible-digits-bis (append l (list cpt)) (1- cpt)))))
    (set-possible-digits-bis '() *size*)))			     

(defvar *digits* (set-possible-digits))

;; coordinates
(defclass coor ()
  ((x :initarg :x :reader x-coor :type integer)
   (y :initarg :y :reader y-coor :type integer))
  (:documentation "class for coordinates in the squares grid"))

(defgeneric x-coor (coor)
  (:documentation "x slot of coor"))

(defgeneric y-coor2 (coor)
  (:documentation "y slot of coor"))

(defgeneric make-coor (x y)
  (:documentation "instance of a coor [x,y]"))

(defgeneric zcoor-to-zone (zcoor)
  (:documentation
   "zone corresponding to the zone coordinate ZCOOR"))

(defgeneric rcoor-to-coor (zone rcoor)
  (:documentation "from zone and relative coor in zone to absolute coor"))

(defclass square ()
  ((coor :initarg :coor :reader coor) 
   (possible-digits :initform (copy-list *digits*)
		  :initarg :possible-digits :accessor possible-digits)
   (digit :accessor digit :initarg :digit :initform 0)
   (protected :accessor protected :initarg :protected :initform nil))
  (:documentation "one square of the squares"))

(defgeneric assigned-p (square)
  (:documentation "T if the digit slot a striclty positive"))

(defgeneric digit (square)
  (:documentation 
   "assigned digit of square (0 if unassigned)"))

(defgeneric possible-digits (square)
  (:documentation 
   "list of possible digits remaining in SQUARE"))

(defgeneric protected (square) 
  (:documentation "T if square was originally filled in the grid"))

(defgeneric make-square (coor &optional digit)
  (:documentation "creates a square containing coor and digit"))

(defgeneric make-squares-array(size)
  (:documentation "create an array of size x size filled with 0"))
   
(defclass squares ()
  ((squares-array :initarg :squares-array :reader squares-array :type array :initform (make-squares-array *size*)))
   (:documentation "class containing a two dimensional array of instances of the square class"))

(defgeneric coor-squares (squares coor)
  (:documentation "the square at COOR in SQUARES"))

(defclass game ()
  ((game-squares :accessor game-squares :initarg :game-squares)
   (initial-grid :reader initial-grid :initarg :initial-grid)
   (result-grid :reader result-grid :initarg :result-grid))
  (:documentation "class for game instances"))

(defgeneric initial-grid (game)
  (:documentation "the initial grid of GAME"))

(defgeneric game-squares (game)
  (:documentation "the squares of GAME"))

(defgeneric game-over (game)
  (:documentation "if the game is over (either won or lost"))

(defgeneric init-game (game)
  (:documentation "initializes GAME with its initial-grid"))

(defgeneric game-with-new-grid(grid)
  (:documentation "instance of game with a grid and STRATEGY"))

(defgeneric init-sudoku ()
  (:documentation "to initialize whatever you want to initialize"))

(defgeneric game-do (game square)
  (:documentation 
   "plays coor/digit of square in the coor-square in squares of GAME"))


;;;sudoku.lisp

(defmethod x-coor((coor coor))
  (slot-value coor 'x))

(defmethod y-coor((coor coor))
  (slot-value coor 'y))

(defmethod make-coor((x integer) (y integer))
  (make-instance 'coor :x x :y y))

(defun quotient (n1 n2)
  ;; quotient of n1 by n2
  (multiple-value-bind (f r)
      (floor n1 n2)
    (declare (ignore r))
    f))

(defmethod zcoor-to-zone((zcoor coor)) 
  (make-coor (* (quotient (x-coor zcoor) *sqrt-size*) *sqrt-size*)
	     (* (quotient (y-coor zcoor) *sqrt-size*) *sqrt-size*))) 

(defmethod rcoor-to-coor ((zone coor) (rcoor coor))
  (make-coor (+ (* (x-coor zone) *sqrt-size*) (x-coor rcoor))
	     (+ (* (y-coor zone) *sqrt-size*) (y-coor rcoor))))

(defmethod assigned-p ((square square))
  (> (slot-value square 'digit) 0))

(defmethod digit ((square square))
  (slot-value square 'digit))

(defmethod possible-digits((square square))
  (slot-value square 'possible-digits))

(defmethod protected ((square square))
  (not (eq (digit (coor-squares (initial-grid *game*) (coor square)))
	   0)))

(defmethod make-square((coor coor) &optional digit)
  (make-instance 'square :coor coor :digit digit))

(defmethod coor-squares(squares coor)
  (aref (slot-value squares 'squares-array)
	(x-coor coor) (y-coor coor)))

(defmethod initial-grid ((game game))
  (slot-value game 'initial-grid))

(defun result-grid(game)
  (slot-value game 'result-grid))

(defmethod game-squares ((game game))
  (slot-value game 'game-squares))

(defmethod init-game((game game))
  (setf (slot-value game 'game-squares)
	(initial-grid game)))

(defun make-squares-array (size)
  (do ((tab (make-array (list size size)) )
       (x 0 (1+ x))
       (y 0 ))
      ((eq y size) tab)
    (if (eq x size)
	(setf x -1 y (+ 1 y))
	(setf (aref tab x y) (make-square (make-coor x y) 0)))))

(defun display-squares (squares)
  ;;return an array of 9x9 filled by digit contain in each square of squares
  (let ((array-res (make-array '(9 9))))
    (do ((x 0 (1+ x));browse through the grid
	 (y 0))
	((eq y *size*) array-res)
      (if (eq x *size*) ;if we read a complete row
	  (setf x -1 y (+ 1 y)) ;we'll write on the next column
	  (setf (aref array-res x y)
		(digit (coor-squares squares (make-coor x y))))))));put the current digit in squares in array-res at (x, y)

(defun is-multiple-of-half-size(x)
  (eq (mod x *sqrt-size*)
      0))

(defun alphabet()
  (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))

(defun alphabet-element(c)
  (nth c (alphabet)))

(defun print-squares(squares)
  (let ((list-squares (display-squares squares)))
    (format t "  ")
    (do ((y 0 (1+ y)))
	((eq y *size*))
      (if (is-multiple-of-half-size y)
	  (format t "|"))
      (format t " ")
      (format t (alphabet-element y))
      (format t " "))
    (do ((x 0 (1+ x)))
	((eq x *size*));stops when we finished reading in list-squares
      (if (is-multiple-of-half-size x);the stars separate each range of squares
	  (progn
	    (format t "~%***")
	    (do ((cpt 0 (1+ cpt)))
		((eq cpt *size*))
	      (if (and (is-multiple-of-half-size cpt) (/= cpt 0))
		  (format t "*"))
	      (format t "***"))))
      (print (1+ x));digits which represent each line
      (do ((y 0 (1+ y)));we go through the current line
	  ((eq y *size*))
	(if (eq y 0)
	    (format t "| ")
	    (progn
	      (if (is-multiple-of-half-size y)
		  (format t " | ");if we're between 2 squares on the same line
		  (format t "  "))));if we've got to separate 2 digits on the same current "squares"
	(if (/= (aref list-squares x y) 0)
	    (prin1 (aref list-squares x y));print the current value
	    (format t " "))))))

(defun init-sudoku(argv)
  ;;read a file and fill a squares that it returns
  (let ((init (make-instance 'squares)));init = a squares filled with 0
    (with-open-file (stream argv)
      (do ((cpt -1 (1+ cpt)))
	  ((eq cpt *size*) init);last line = ")"
	(let ((line (read-line stream nil)))
	  (if (not (eq cpt -1));-1 corresponds to the first line "("
	      (copy-row line init cpt)))))))

(defun copy-row(row-to-copy squares cpt)
  ;;copy the string row-to-copy to the line cpt in the array
  (let ((list-row (with-input-from-string (s row-to-copy :start 0) (read s))));converts the current row from string to list
    (do ((y 0 (1+ y)))
	((eq y *size*) '())
      (if (not (eq (car list-row)
		   0))
	  (setf (protected (coor-squares squares (make-coor cpt y)));digit initialy in the grid = the variable is protected
		t))
      (setf (digit (coor-squares squares (make-coor cpt y)))
	    (car list-row))
      (setf list-row
	    (cdr list-row)))))

(defun game-with-new-grid (grid)
  (setf *game* (make-instance 'game))
  (setf (slot-value *game* 'initial-grid) (init-sudoku (nth grid *new-grids-9*)))
  (setf (game-squares *game*) (init-sudoku (nth grid *new-grids-9*)))
  (setf (slot-value *game* 'result-grid) (init-sudoku (nth grid *new-grids-result-9*))))

(defun game-do (game square)
  ;;place a square in the array of game
  (if (protected (coor-squares (game-squares game) (coor square))) ;;if not protected you can change this value
      (format t "You can't change this value~%~%")
      (setf (digit (coor-squares (game-squares game) (coor square))) (digit square))))

(defun are-equal-squares(square1 square2)
  (eq (digit square1) (digit square2))) 

(defun game-over (game)
  ;;compare result-array and game-array in squares of game
  (do ((x 0 (1+ x))
       (n 0 (1+ n))
       (y 0 ))
      ((eq n (* *size* *size*))
       t)
    (if (eq x *size*)
	(setf x 0 y (+ 1 y)))
    (if (not (are-equal-squares
	      (coor-squares (game-squares game) (make-coor x y))
	      (coor-squares (result-grid game) (make-coor x y))))
	(return nil))))

(defun filled-grid(game)
  ;;check if the grid is filled
  (do ((x 0 (1+ x))
       (n 0 (1+ n))
       (y 0 ))
      ((eq n (* *size* *size*)) t)
    (if (eq x *size*)
 	(setf x 0 y (+ 1 y)))
    (if (eq (digit (coor-squares (game-squares game) (make-coor x y))) 0)
	(return nil))))



(defun possible-digits-column(squares square)
  ;; update possible digits for column, course sudoku column corresponding and delete number already in colum for possible digits of square 
  (do ((x (x-coor (coor square)) )
       (y 0 (+ y 1)) 
       (l (copy-list (possible-digits square)) (delete (digit (coor-squares squares (make-coor x y))) l)))
      ((eq y *size*) l)))

(defun possible-digits-line(squares square)
  ;; same whith line
  (do ((y (y-coor (coor square)) )
       (x 0 (+ x 1)) 
       (l (copy-list (possible-digits square)) (delete (digit (coor-squares squares (make-coor x y))) l)))
      ((eq x *size*) l)))

(defun possible-digits-zone(squares square)
  ;; same with zone
  (do ((xsave (x-coor (zcoor-to-zone (coor square))) )
       (n 0 (+ n 1))
       (x (x-coor (zcoor-to-zone (coor square))) (+ x 1))
       (y (y-coor (zcoor-to-zone (coor square))) )
       (l (copy-list (possible-digits square)) (delete (digit (coor-squares squares (make-coor x y))) l)))
      ((eq n *size*) l)
    (if (eq x (+ (x-coor (zcoor-to-zone (coor square))) *sqrt-size*))
	(setf x xsave y (+ y 1)))))

(defun update-possible-digits(squares square)
  ;; intersection of possible digits for one square 
  (let ((l1 (possible-digits-line squares square))
	(l2 (possible-digits-column squares square))
	(l3 (possible-digits-zone squares square)))
    (setf (possible-digits square) (intersection (intersection l1 l2) l3))))

(defun update-possible-digits-grid(game)
  ;; use update-possible-digits for all square of squares   
  (do ((x 0 (1+ x))
       (y 0 )
       (n 0 (1+ n)))
      ((eq n (* *size* *size*)) t) 
    (if (eq x *size*)
	(setf x 0 y (+ 1 y)))
    (update-possible-digits (game-squares game) (coor-squares (game-squares game) (make-coor x y)))))

(defun delete-digits-line(squares square digit)
  ;; delete digits of possible-digits for line of square 
  (do ((x (x-coor (coor square)) )
       (y 0 (+ y 1)))
      ((eq y *size*) )
    (setf (possible-digits (coor-squares squares (make-coor x y)))
	  (delete digit (possible-digits (coor-squares squares (make-coor x y)))))))

(defun delete-digits-column(squares square digit)
  ;;same with column
  (do ((y (y-coor (coor square)) )
       (x 0 (+ x 1)))
      ((eq x *size*) )
    (setf (possible-digits (coor-squares squares (make-coor x y)))
	  (delete digit (possible-digits (coor-squares squares (make-coor x y)))))))

(defun delete-digits-zone (squares square digit)
  ;;same with zone
  (do ((xsave (x-coor (zcoor-to-zone (coor square))) )
       (n 0 (+ n 1))
       (x (x-coor (zcoor-to-zone (coor square))) (+ x 1))
       (y (y-coor (zcoor-to-zone (coor square))) ))
      ((eq n *size*) )
    (if (eq x (+ (x-coor (zcoor-to-zone (coor square))) *sqrt-size*))
	(setf x xsave y (+ y 1)))
    (setf (possible-digits (coor-squares squares (make-coor x y)))
	  (delete digit (possible-digits (coor-squares squares (make-coor x y)))))))

(defun delete-possible-digits(squares square digit)
  ;;use three delete
  (delete-digits-zone squares square digit)
  (delete-digits-line squares square digit)
  (delete-digits-column squares square digit))


;;global coordinates
(defvar *x* 0)
(defvar *y* 0)
(defvar *n* 0);column
(defvar *filled* 0);number
(defvar *isFillOnce* nil)
(defvar *step* 1);number of possible digits to check in the current square

;;transforms a squares-array into a class squares which contains it
(defun make-squares-from-square-array(square-array)
  (make-instance 'squares :squares-array square-array))

;;transforms a digit-array into a square array
(defun make-square-array-from-digit-array(digit-array)
  (let ((l (make-squares-array *size*)))
    (do ((x 0 (1+ x))
	 (n 0 (1+ n))
	 (y 0))
	((eq n (* *size* *size*)) l)
      (if (eq x *size*)
	  (setf x 0
		y (1+ y)))
      (setf (digit (aref l x y)) (aref digit-array x y)))))

(defun reverse-squares-grid(square-array)
  ;;transforms a square into a grid 
  (let ((l (make-array (list *size* *size*))))
    (do ((x 0 (1+ x))
	 (n 0 (1+ n))
	 (y 0))
	((eq n (* *size* *size*)) l)
      (if (eq x *size*)
	  (setf x 0
		y (1+ y)))
      (setf (aref l x y) (digit (aref square-array x y))))))

(defun to-fill()
  ;;update *filled*
  (do ((x 0 (1+ x))
       (n 0 (1+ n))
       (y 0 ))
      ((eq n (* *size* *size*)) t)
    (if (eq x *size*)
 	(setf x 0 y (+ 1 y)))
    (if (not (eq (digit (coor-squares (game-squares *game*) (make-coor x y))) 0));if the square browsed is equal to 0, we increment the value *filled*
	(setf *filled* (1+ *filled* )))))

(defun init-standalone (grid)
  ;;initializes the global variable *game* for strategies
  (let ((squares-grid (make-squares-from-square-array (make-square-array-from-digit-array grid))))
    (setf *game* (make-instance 'game))
    (setf (slot-value *game* 'initial-grid) squares-grid)
    (setf (game-squares *game*) squares-grid)
    (update-possible-digits-grid *game*) ;;we update the grid once
    (to-fill)
    (setf *x* -1 *y* 0 *n* 0)))

(defun main-standalone()
  (setf *x* (+ *x* 1)) 
  (setf *n* (+ *n* 1))
  (if (eq *x*  *size*)
      (setf *x* 0 *y* (+ 1 *y*)))
  (if (eq *n* (* *size* *size*))
      (progn
	(setf *x* 0
	      *y* 0
	      *n* 0)
	(if *isFillOnce* ;if the array has been used
	    (setf *step* 1)
	    (setf *step* (1+ *step*)))))
  (if (= *filled* *nb-squares*) ;if the grid is filled, we return nil
      nil
      (let ((current-square (coor-squares (game-squares *game*) (make-coor *x* *y*)))) 
	;;if list possible-digits = *step* and the value of square equal 0
	(if (and (eq (list-length (possible-digits current-square))
		     *step*)
		 (eq (digit current-square)
		     0))
	    (progn
	      (let ((val (car (possible-digits current-square)))) 
		(delete-possible-digits (game-squares *game*)
					current-square
					val) ;;We replace digits with possible-digits and update possible digits of line column and zone with this square
		(setf *isFillOnce* T)
		(setf *filled* (1+ *filled*))
		(values *x* *y* val))) ;we return *x*, *y* and val from the square that we change
	    (main-standalone ))))) ;else we callback the function

(defun test-strategy()
 ;; run main-standlone until squares filled
  (init-standalone (reverse-squares-grid (squares-array (init-sudoku (nth 0 *new-grids-9*)))))
  (loop
    (multiple-value-bind (x y val)	
	(main-standalone)
      (if (and (not (eq val 0))
	       (not (protected (coor-squares (game-squares *game*) (make-coor x y)))))
	  (progn
	    (game-do *game* (make-square (make-coor x y) val))))
      (if (filled-grid *game*)
	  (return (print-squares (game-squares *game*)))))))
