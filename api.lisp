(in-package :sudoku)
(defvar *sqrt-size* 3 "side of the side of a zone")
(defvar *size* (* *sqrt-size* *sqrt-size*))
(defvar *nb-squares* (* *size* *size*))
(defvar *game* nil "the current instance of a game")

(defvar *new-grids* '())
(defvar *new-grids-9* (list "./Grids/9x9-1.sudoku" "./Grids/9x9-2.sudoku" "./Grids/9x9-3.sudoku" "./Grids/9x9-9.sudoku" "./Grids/1.sudoku" "./Grids/2.sudoku" "./Grids/4.sudoku" "./Grids/5.sudoku" "./Grids/6.sudoku"))
(defvar *new-grids-4* (list "./Grids/4x4/4x4-1.sudoku" "./Grids/4x4/4x4-2.sudoku" "./Grids/4x4/4x4-3.sudoku" "./Grids/4x4/4x4-4.sudoku" "./Grids/4x4/4x4-5.sudoku" "./Grids/4x4/4x4-6.sudoku" "./Grids/4x4/4x4-7.sudoku" "./Grids/4x4/4x4-8.sudoku"))

(defvar *new-grids-result* '())
(defvar *new-grids-result-9* (list "./Grids/9x9-1-result.sudoku" "./Grids/9x9-2-result.sudoku" "./Grids/9x9-3-result.sudoku" "./Grids/9x9-9-result.sudoku" "./Grids/1-result.sudoku" "./Grids/2-result.sudoku" "./Grids/4-result.sudoku" "./Grids/5-result.sudoku" "./Grids/6-result.sudoku"))
(defvar *new-grids-result-4* (list "./Grids/4x4/4x4-1-result.sudoku" "./Grids/4x4/4x4-2-result.sudoku" "./Grids/4x4/4x4-3-result.sudoku" "./Grids/4x4/4x4-4-result.sudoku" "./Grids/4x4/4x4-5-result.sudoku" "./Grids/4x4/4x4-6-result.sudoku" "./Grids/4x4/4x4-7-result.sudoku" "./Grids/4x4/4x4-8-result.sudoku"))

;; coordinates
(defclass coor ()
  ((x :initarg :x :reader x-coor :type integer)
   (y :initarg :y :reader y-coor :type integer))
  (:documentation "class for coordinates in the squares grid"))

(defgeneric x-coor (coor)
  (:documentation "x slot of coor"))

(defgeneric y-coor (coor)
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
