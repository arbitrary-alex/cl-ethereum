(in-package #:solidity)

(defvar *loop-exit* nil
  "Label to jump to when BREAK'ing out of a loop.")

(defun tac-label-p (statement)
  "Is the three-address code statement STATEMENT an label?"
  (eql (car statement) 'label))

(defun tac-branch-destination (statement)
  "Branch destination for the three-address code statement STATEMENT."
  (case (car statement)
    (cjump (alexandria:last-elt statement))
    (jump (cadr statement))))

(defun tac-parse-value (kind value)
  "Parse VALUE according to its KIND."
  (case kind
    (:number (parse-integer value :radix 16))
    (t value)))

(defun tac-convert-list (es)
  "Convert list of expressions ES to three-address code form."
  (multiple-value-bind (converted vars)
      (mapcar-with #'(lambda (form vars)
                       (multiple-value-bind (forms var)
                           (tac-convert form)
                         (values forms (cons var vars))))
                   es)
    (values (apply #'append converted) vars)))

(defun tac-negate-cmp (cmp)
  (cond ((string= cmp "!=") "==")
        ((string= cmp "==") "!=")
        ((string= cmp "<") ">=")
        ((string= cmp "<=") ">")
        ((string= cmp ">") "<=")
        ((string= cmp ">=") "<")
        (t (error "unknown comparison: ~A" cmp))))

(defun tac-split-condition (condition)
  (let ((test (alexandria:last-elt condition)))
    (ecase (car test)
      (move (destructuring-bind (move tmp (binop op left right))
                test
              (declare (ignore move tmp binop))
              (values op left right (butlast condition))))
      (lit (values "==" test t (butlast condition))))))

(defun tac-convert-one-armed-if (condition cond-var true-body true-var)
  (declare (ignore true-var))
  (multiple-value-bind (op left right cond-body)
      (if condition
          (tac-split-condition condition)
          (values "==" cond-var t nil))
    (let ((l0 (gensym "L"))
          (l1 (gensym "L")))
      `(,@cond-body
        (cjump ,op ,left ,right ,l0)
        (jump ,l1)
        (label ,l0)
        ,@true-body
        (label ,l1)))))

(defun tac-convert-two-armed-if (condition cond-var true-body true-var false-body false-var)
  (declare (ignore true-var false-var))
  (let ((l0  (gensym "L"))
        (l1  (gensym "L"))
        (l2  (gensym "L")))
    (multiple-value-bind (op left right cond-body)
        (if condition
            (tac-split-condition condition)
            (values "==" cond-var t nil))
      `(,@cond-body
        (cjump ,op ,left ,right ,l0)
        (jump ,l1)
        (label ,l0)
        ,@true-body
        (jump ,l2)
        (label ,l1)
        ,@false-body
        (label ,l2)))))

(defun tac-convert-if (e)
  (if (assoc-value e :false-body)
      (multiple-value-call #'tac-convert-two-armed-if
        (tac-convert (assoc-value e :condition))
        (tac-convert (assoc-value e :true-body))
        (tac-convert (assoc-value e :false-body)))
      (multiple-value-call #'tac-convert-one-armed-if
        (tac-convert (assoc-value e :condition))
        (tac-convert (assoc-value e :true-body)))))

(defun tac-convert-for (e)
  "Convert for-loop E into intermediate representation."
  (let (loop-init loop-expr)
    (let* ((l-top (gensym "L"))
           (l-end (gensym "L"))
           (*loop-exit* l-end)
           (loop-body (tac-convert (assoc-value e :body))))
      (when (assoc :initialization-expression e)
        (setq loop-init (tac-convert (assoc-value e :initialization-expression))))
      (when (assoc :loop-expression e)
        (setq loop-expr (tac-convert (assoc-value e :loop-expression))))
      (if (assoc :condition e)
          (multiple-value-bind (op left right cond-body)
              (tac-split-condition (tac-convert (assoc-value e :condition)))
            `(,@loop-init
              (label ,l-top)
              ,@cond-body
              (cjump ,(tac-negate-cmp op) ,left ,right ,l-end)
              ,@loop-body
              ,@loop-expr
              (jump ,l-top)
              (label ,l-end)))
          `(,@loop-init
            (label ,l-top)
            ,@loop-body
            ,@loop-expr
            (jump ,l-top)
            (label ,l-end))))))

(defun tac-convert-while (e)
  "Convert while-loop E into intermediate representation."
  (let* ((l-top (gensym "L"))
         (l-end (gensym "L"))
         (*loop-exit* l-end)
         (loop-body (tac-convert (assoc-value e :body)))
         (condition (tac-convert (assoc-value e :condition))))
    (multiple-value-bind (op left right cond-body)
        (tac-split-condition condition)
      `((label ,l-top)
        ,@cond-body
        (cjump ,(tac-negate-cmp op) ,left ,right ,l-end)
        ,@loop-body
        (jump ,l-top)
        (label ,l-end)))))

(defun tac-convert (e)
  "Convert expression E to a three-address code form."
  (ecase (node-type e)
    (:source-unit
     (-> e
         (asubst :nodes (alexandria:mappend #'tac-convert (node-nodes e)))
         (list)
         (values nil)))
    (:pragma-directive (values (list e) nil))
    (:import-directive (values (list e) nil))
    (:new-expression (values nil 'new))
    (:if-statement (tac-convert-if e))
    (:for-statement (tac-convert-for e))
    (:while-statement (tac-convert-while e))
    (:expression-statement
     (tac-convert (assoc-value e :expression)))
    (:variable-declaration-statement
     (let ((initial-value (assoc-value e :initial-value)))
       (multiple-value-bind (declarations declaration-var)
           (tac-convert-list (assoc-value e :declarations))
         (if initial-value
             (multiple-value-bind (initial-exprs new-var)
                 (tac-convert initial-value)
               (values `(,@declarations
                         ,@initial-exprs
                         (move ,(first declaration-var) ,new-var))
                       declaration-var))
             (values declarations declaration-var)))))

    (:literal
     (values nil
             `(lit ,(tac-parse-value
                     (assoc-value e :kind)
                     (assoc-value e :hex-value)))))

    (:break
     `((jump ,*loop-exit*)))

    (:contract-definition
     (-> (ecase (contract-kind e)
           (:contract
            (->> (node-nodes e)
                 (mapcar #'tac-convert)))
           (t e))
         (list)
         (values nil)))

    (:variable-declaration
     (if (assoc-value e :state-variable)
         (values `((global ,(assoc-value e :name)
                           ,(get-in e (list :type-descriptions :type-string))))
                 nil)
         (values nil `(var ,(assoc-value e :name)))))

    (:function-definition
     (multiple-value-bind (body var)
         (tac-convert (assoc-value e :body))
       (declare (ignore var))
       `((label ,(assoc-value e :name))
         ,@body)))

    (:block
     (multiple-value-bind (statements block-vars)
         (tac-convert-list (assoc-value e :statements))
       (values statements (alexandria:last-elt block-vars))))

    (:member-access
     (multiple-value-bind (exprs var)
         (tac-convert (assoc-value e :expression))
       (let ((member (assoc-value e :member-name)))
         (if (assoc-value e :l-value-requested)
             (values exprs `(access ,var ,member))
             (let ((tmp (gensym "T")))
               (values `(,@exprs
                         (move ,tmp (access ,var ,member)))
                       tmp))))))

    (:index-access
     (let ((base (assoc-value e :base-expression))
           (indx (assoc-value e :index-expression)))
       (multiple-value-bind (base-stmts base-var)
           (tac-convert base)
         (multiple-value-bind (indx-stmts indx-var)
             (tac-convert indx)
           (if (assoc-value e :l-value-requested)
               (values (append base-stmts indx-stmts)
                       `(mem ,base-var ,indx-var))
               (let ((tmp (gensym "T")))
                 (values `(,@base-stmts
                           ,@indx-stmts
                           (move ,tmp (mem ,base-var ,indx-var)))
                         tmp)))))))

    (:function-call
     (multiple-value-bind (exprs function)
         (tac-convert (assoc-value e :expression))
       (multiple-value-bind (arguments vars)
           (tac-convert-list (assoc-value e :arguments))
         (let ((tmp (gensym "T")))
           (values `(,@arguments
                     ,@(mapcar #'(lambda (v) `(param ,v)) vars)
                     ,@exprs
                     (move ,tmp (funcall ,function ,(length vars))))
                   tmp)))))

    (:identifier
     (values nil `(var ,(assoc-value e :name))))

    (:elementary-type-name-expression
     (->> (list :type-name :name)
          (get-in e)
          (make-symbol)
          (values nil)))


    (:assignment
     (multiple-value-bind (lhs lhs-var)
         (tac-convert (assoc-value e :left-hand-side))
       (multiple-value-bind (rhs rhs-var)
           (tac-convert (assoc-value e :right-hand-side))
         (values `(,@rhs
                   ,@lhs
                   (move ,lhs-var ,rhs-var))
                 lhs-var))))

    (:binary-operation
     (multiple-value-bind (left left-var)
         (tac-convert (assoc-value e :left-expression))
       (multiple-value-bind (right right-var)
           (tac-convert (assoc-value e :right-expression))
         (let ((tmp (gensym "T"))
               (op (assoc-value e :operator)))
           (values
            `(,@left
              ,@right
              (move ,tmp (binop ,op ,left-var ,right-var)))
            tmp)))))

    (:unary-operation
     (multiple-value-bind (value var)
         (tac-convert (assoc-value e :sub-expression))
       (let ((op (assoc-value e :operator)))
         (values `(,@value (move ,var (unary ,op ,var)))
                 var))))

    (:return
      (multiple-value-bind (value var)
          (tac-convert (assoc-value e :expression))
        (if var
            `(,@value (return ,var))
            `(return))))))
