(named-readtables:in-readtable :cl-smt-lib)
(in-package #:solidity)

(defun start-z3 ()
  (cl-smt-lib:make-smt "z3" '("-in" "-smt2")))
