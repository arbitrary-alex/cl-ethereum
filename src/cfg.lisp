(in-package #:solidity)

(defclass control-flow-edge (cl-graph:basic-edge)
  ((branch-taken
    :initform nil
    :initarg :branch-taken)))

(defclass basic-block ()
  ((instructions
    :initform nil
    :initarg :instructions
    :accessor basic-block-instructions)))

(defun make-basic-block (instructions)
  (make-instance 'basic-block :instructions instructions))

(defclass control-flow-graph (cl-graph:dot-graph)
  ()
  (:default-initargs :default-edge-type :directed))

(defun basic-blocks (statements)
  "Build basic blocks from three-address code statements STATEMENTS."
  (flet ((accumulate-statement (accumulator statement)
           (destructuring-bind (current-block . blocks)
               accumulator
             (ecase (car statement)
               (label
                (if current-block
                    (let* ((instructions (nreverse current-block))
                           (basic-block (make-basic-block instructions)))
                      (cons (list statement) (cons basic-block blocks)))
                    (cons (cons statement current-block) blocks)))
               ((jump cjump return)
                (let* ((instructions (nreverse (cons statement current-block)))
                       (basic-block (make-basic-block instructions)))
                  (cons nil (cons basic-block blocks))))
               ((move param)
                (cons (cons statement current-block) blocks))))))
    (destructuring-bind (current-block . blocks)
        (reduce #'accumulate-statement
                statements
                :initial-value (cons nil nil))
      (when current-block
        (setq blocks (append blocks (list (make-basic-block current-block)))))
      (nreverse blocks))))

(defun control-flow-graph (basic-blocks)
  "Build a control-flow-graph from BASIC-BLOCKS, a list of basic blocks."
  (let ((blocks-by-label (make-hash-table))
        (blocks-and-successors (mapcar #'cons basic-blocks (cdr basic-blocks)))
        (cfg (cl-graph:make-graph 'control-flow-graph)))
    (dolist (basic-block basic-blocks)
      (with-slots (instructions) basic-block
        (when (eql (caar instructions) 'label)
          (setf (gethash (cadar instructions) blocks-by-label) basic-block))))
    (dolist (block-and-successor blocks-and-successors)
      (destructuring-bind (basic-block . successor)
          block-and-successor
        (declare (ignore successor))
        (cl-graph:add-vertex cfg basic-block)))
    (dolist (block-and-successor blocks-and-successors)
      (destructuring-bind (basic-block . successor)
          block-and-successor
        (with-slots (instructions) basic-block
          (let* ((last-instruction (alexandria:last-elt instructions))
                 (branch-dest (case (car last-instruction)
                                ((jump cjump) (alexandria:last-elt last-instruction)))))
            (when branch-dest
              (multiple-value-bind (destination foundp)
                  (gethash branch-dest blocks-by-label)
                (unless foundp
                  (error "unknown branch destination: ~A" branch-dest))
                (cl-graph:add-edge-between-vertexes cfg basic-block destination)))
            ;; add fallthrough edge
            (when (and successor (not (eql (car last-instruction) 'jump)))
              (cl-graph:add-edge-between-vertexes cfg basic-block successor))))))
    cfg))
