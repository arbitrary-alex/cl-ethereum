(in-package #:solidity)

(defun node-p (object)
  (and (consp object)
       (consp (car object))
       (node-type object)))

(defun node-id (object)
  (assoc-value object :id))

(defun node-type (object)
  (assoc-value object :node-type))

(defun node-type-p (node-type)
  (lambda (object)
    (eql (node-type object) node-type)))

(defun node-nodes (node)
  (assoc-value node :nodes))

(defun node-body (node)
 (assoc-value node :body))

(defun variable-name (var)
  (assoc-value var :name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun function-visibility (function)
  (assoc-value function :visibility))

(defun function-visibility-p (visibility)
  (lambda (function)
    (eql (function-visibility function) visibility)))

(defun function-parameters (function)
  (get-in function '(:parameters :parameters)))

(defun function-return-parameters (function)
  (get-in function '(:return-parameters :parameters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun contract-variables (contract)
  (remove-if-not
   (node-type-p :variable-declaration)
   (assoc-value contract :nodes)))

(defun contract-functions (contract)
  (remove-if-not
   (node-type-p :function-definition)
   (assoc-value contract :nodes)))

(defun contract-kind (contract)
  (assoc-value contract :contract-kind))

(defun contract-kind-p (kind)
  (lambda (contract)
    (eql (contract-kind contract) kind)))

(defun contract-external-functions (contract)
  (remove-if-not
   #'(lambda (f)
       (or (funcall (function-visibility-p :external) f)
           (funcall (function-visibility-p :public) f)))
   (contract-functions contract)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct node-database
  (id 0 :type integer)
  (table (make-hash-table)))

(defvar *nodes*)

(defun build-node-table (build-info)
  (let ((nodes (make-node-database)))
    (flet ((analyze-node (n)
             (when (node-p n)
               (setf (node-database-id nodes)
                     (max (node-database-id nodes)
                          (node-id n))
                     (gethash (node-id n)
                              (node-database-table nodes))
                     n))))
      (walk-tree #'analyze-node build-info))
    nodes))

(defun find-node (id &optional (db *nodes*))
  (gethash id (node-database-table db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-solidity (file)
  (read-build-info-json-from-string
   (with-output-to-string (s)
     (uiop:run-program (list "solc"
                             "--debug-info=ast-id"
                             "--ast-compact-json"
                             "--combined-json=asm,ast,bin,bin-runtime,srcmap,userdoc"
                             (namestring file))
                       :output s
                       :error-output *standard-output*))))

#+nil
(progn
  (setq *print-level* 15)
  (setq *print-length* 100)
  (defparameter *build-info*
    (with-open-file (f "./fb02600c7e0fd9ab99da91b62ddf9387.json")
      (read-build-info-json f)))
  (defparameter *test-unit*
    (get-in *build-info* '(:output :sources :contracts/drain-emn.sol :ast))))

(defun abi-decode-string (string))
