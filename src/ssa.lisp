(in-package #:solidity)

(defun make-unique-name (var)
  (-> (variable-name var)
      (string-upcase)
      (gensym)))

(defun extend-with-variable-declarations (declarations env)
  (reduce #'(lambda (env var)
              (acons (variable-name var)
                     (make-unique-name var)
                     env))
          declarations
          :initial-value env))

(defun extend-bindings (var val env)
  (acons var val env))

(defun lookup-binding (var env)
  (assoc-value env (variable-name var) :test #'string=))

(defun ssa-convert (env)
  (lambda (e) (ssa-convert-exp e env)))

(defun ssa-convert-exp (e env)
  (declare (optimize (debug 3)))
  (ecase (node-type e)
    (:source-unit
     (values
      (asubst e :nodes (mapcar (ssa-convert env) (node-nodes e)))
      env))
    (:pragma-directive (values e env))
    (:import-directive (values e env))
    (:contract-definition
     (let* ((variables (contract-variables e))
            (functions (contract-functions e))
            (new-env (extend-with-variable-declarations variables env)))
       (-> e
           (asubst :nodes
                   (-> (ssa-convert new-env)
                       (mapcar functions)))
           (values env))))
    (:function-definition
     (let* ((fun-params (function-parameters e))
            (ret-params (function-return-parameters e))
            (all-params (append fun-params ret-params))
            (new-env (extend-with-variable-declarations all-params env)))
       (if (node-body e)
           (values (->>
                    (-> (ssa-convert new-env)
                        (funcall (node-body e)))
                    (asubst e :body))
                   env)
           (values e env))))
    (:block
     (let ((statements (assoc-value e :statements)))
       (mapcar-with #'ssa-convert-exp
                    statements
                    :initial-value env)))
    (:identifier
     (multiple-value-bind (name foundp)
         (lookup-binding e env)
       (-> (if foundp (asubst e :name name) e)
           (values env))))
    (:function-call
     (let ((new-exp (funcall (ssa-convert env) (assoc-value e :expression)))
           (new-arg (mapcar (ssa-convert env) (assoc-value e :arguments))))
       (-> e
           (asubst :expression new-exp)
           (asubst :arguments new-arg)
           (values env))))
    (:member-access
     (-> (->> (assoc-value e :expression)
              (funcall (ssa-convert env))
              (asubst e :expression))
         (values env)))
    (:variable-declaration-statement
     (let* ((variables (assoc-value e :declarations))
            (new-env (extend-with-variable-declarations variables env)))
       (values e new-env)))
    (:expression-statement
     (multiple-value-bind (new-exp new-env)
         (ssa-convert-exp (assoc-value e :expression) env)
       (values (asubst e :expression new-exp) new-env)))
    (:assignment
     (let* ((rhs (assoc-value e :right-hand-side))
            (new-rhs (funcall (ssa-convert env) rhs))
            (lhs (assoc-value e :left-hand-side))
            (new-env (extend-with-variable-declarations (list lhs) env))
            (new-lhs (funcall (ssa-convert new-env) lhs)))
       (-> e
           (asubst :right-hand-side new-rhs)
           (asubst :left-hand-side new-lhs)
           (values new-env))))
    (:literal (values e env))
    (:binary-operation
     (let ((left-expression (assoc-value e :left-expression))
           (right-expression (assoc-value e :right-expression)))
       (-> e
           (asubst :left-expression (funcall (ssa-convert env) left-expression))
           (asubst :right-expression (funcall (ssa-convert env) right-expression))
           (values env))))
    (:elementary-type-name-expression
     (values e env))
    (:if-statement
     ;; add phi function
     (let ((condition (assoc-value e :condition))
           (true-body (assoc-value e :true-body))
           (false-body (assoc-value e :false-body)))
       (-> e
           (asubst :condition (funcall (ssa-convert env) condition))
           (asubst :true-body (funcall (ssa-convert env) true-body))
           (asubst :false-body (when false-body
                                 (funcall (ssa-convert env) false-body)))
           (values env))))))
