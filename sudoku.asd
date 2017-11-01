(cl:in-package #:common-lisp-user)

(defpackage :sudoku-system
  (:use :asdf :common-lisp))

(in-package :sudoku-system)

(defparameter *sudoku-directory* (directory-namestring *load-truename*))
(format t "sudoku-directory is ~A ~%" *sudoku-directory*)

(asdf:defsystem :sudoku
  :serial t
  :components
  (
   (:file "package")
   (:file "general")
   (:file "file-path")
   (:file "coor")
   (:file "square")
   (:file "strategy")
   (:file "squares")
   (:file "grid")
   (:file "sgrid")
   (:file "strategies")
   (:file "game")
   (:file "universal-strategy")
   (:file "deterministic-tactic")
   (:file "non-deterministic-tactic")
   (:file "fuzzy-set")
   (:file "region-tactics")
   (:file "brute-force")
   (:file "brute-force-strategy")
   (:file "constructive-strategy")
   (:file "interactive-strategy")
   (:file "random-strategy")
   (:file "run-game")
   (:file "sudoku")
   ))

(pushnew :sudoku *features*)

(asdf:defsystem :server
  :depends-on (:sudoku)
  :serial t
  :components
  (
   (:file "Server/file-path")
   (:file "Server/player-strategy")
   (:file "Server/standalone-strategy")
   (:file "Server/server")
   (:file "Server/tournament")
   ))

(pushnew :sudoku *features*)
