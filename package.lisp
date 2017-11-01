(cl:in-package #:common-lisp-user)

(defpackage
    #:sudoku (:use #:common-lisp)
    (:export
     ;; variables
     #:*game*
     #:*sqrt-size*
     ;; classes
     #:coor
     #:square
     #:squares
     #:game
     ;; operations
     #:game-over
     #:init-game
     #:coor-square
     #:make-square
     #:make-coor
     #:game-squares
     #:digit
     #:next-move
     #:possible-digits
     #:rcoor-to-coor
     #:zcoor-to-zone
     #:game-do
     #:assigned-p
     #:x-coor
     #:y-coor
     #:protected
     #:init-sudoku
     #:game-with-new-grid
     #:solved-p))
