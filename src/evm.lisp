(in-package #:evm)

(defvar *opcode-table* (make-array 256))

(defun disassemble-bytecode (bytecode)
  (multiple-value-bind (bytecode index)
      (disassemble-1 )))

(defmacro define-opcode (name (byte &optional (argument-length 0)) &body body)
  `(progn
     (setf (aref *opcode-table*)
           (lambda (byte)
             ()))))

(defvar *stack*)
(defvar *memory*)
(defvar *storage*)

(define-symbol-macro *top-of-stack* (car *stack*))

(define-opcode dup (#x256)
  (push *top-of-stack*))

(define-opcode lit1 (#00 1)
  (literal1 (fetch-argument)))
