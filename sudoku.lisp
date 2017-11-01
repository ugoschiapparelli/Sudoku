(in-package :sudoku)

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
  (make-coor (* (quotient (x-coor zcoor) 3) 3)
	     (* (quotient (y-coor zcoor) 3) 3))) 

(defmethod rcoor-to-coor ((zone coor) (rcoor coor))
  (make-coor (+ (* (x-coor zone) 3) (x-coor rcoor))
	     (+ (* (y-coor zone) 3) (y-coor rcoor))))

(defmethod assigned-p ((square square))
  (> (slot-value square 'digit) 0))

(defmethod digit ((square square))
  (slot-value square 'digit))

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
2
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
  (if (eq *size* 9)
      (progn
	(setf (slot-value *game* 'initial-grid) (init-sudoku (nth grid *new-grids-9*)))
	(setf (game-squares *game*) (init-sudoku (nth grid *new-grids-9*)))
	(setf (slot-value *game* 'result-grid) (init-sudoku (nth grid *new-grids-result-9*))))
      (if (eq *size* 4)
	    (progn
	      (setf (slot-value *game* 'initial-grid) (init-sudoku (nth grid *new-grids-4*)))
	      (setf (game-squares *game*) (init-sudoku (nth grid *new-grids-4*)))
	      (setf (slot-value *game* 'result-grid) (init-sudoku (nth grid *new-grids-result-4*)))))))

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

(defun quit (c)
  ;;if c = "quit": return t
  (string-equal (write-to-string c) "quit"))

(defun change-A-to-1(c)
  ;;change equivalent string and digit
  (let ((alphabet (alphabet)))
    (labels
	((change-A-to-l-bis(c val)
	   (if (string-equal c (nth (1- val) alphabet))
	       val
	       (change-A-to-l-bis c (1+ val)))))
      (change-A-to-l-bis c 1))))

(defun update-size(s)
  (setf *sqrt-size* (truncate (sqrt s)))
  (setf *size* (truncate s));truncate converts a float to an integer
  (setf *nb-squares* (* *size* *size*))
  (if (eq s 9)
      (progn
	(setf *new-grids* *new-grids-9*)
	(setf *new-grids-result* *new-grids-result-9*))
      (progn
	(setf *new-grids* *new-grids-4*)
	(setf *new-grids-result* *new-grids-result-4*))))

  
(defun sudoku-run ()
  ;;run the game
  (let ((g 0))
    (loop 
      (format t "Choose the type of grid that you want to play ~%")
      (format t "4->4x4~%")
      (format t "9->9x9~%")
      (setf g (read))
      (update-size g)
      (if (or (= g 9) (= g 4))
	  (return)
	  (format t "Wrong value~%"))))
  (let ((g 0))
    (loop 
      (format t "Choose your grid (a number between 1 and ~d) " (list-length *new-grids*))
      (setf g (read))
      (if (and (> g 0) (<= g (list-length *new-grids*)))
	  (return (game-with-new-grid (- g 1)))
	  (format t "Wrong value~%"))))
  (let ((c 0) (l 0) (val 0)) 
    (loop
      (print-squares (game-squares *game*));;print array  
      (format t "~%C ? ") 
      (setf c (read)) ;wait player input number for l
      (if (quit c) ;if "quit": exit sudoku
	  (return ))
      (format t "~%L ? ") 
      (setf l (read)) ;wait player input letter for c
      (if (quit l)
	  (return ))
      (format t "~%val ? ")
      (setf val (read ));wait player input for val
      (if (quit val) 
	  (return ))
      (if (> val 10)
	  (format t "~%Impossible value~%"))
      (setf c (change-A-to-1 c))
       (if (and (< val (1+ *size*))
		(>= val 0)
		(not (eq c nil))
		(< l (1+ *size*))
		(> l 0))
	   (game-do *game* (make-square (make-coor (- l 1) (- c 1)) val)))
       (if (game-over *game*) ;if result and game-array corresponds you won 
	   (progn
	     (format t "Congratulations, you have won !~%")
	     (return (print-squares (game-squares *game*))))))))
