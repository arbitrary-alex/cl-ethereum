(in-package #:ethereum/utils)

(defmacro -> (form &rest forms)
  "Threads an FORM through FORMS. Inserts FORM as the secound item in the first form of FORMS. IF there are more forms, insert the first as the second item in second form, etc."
  (if (null forms)
      form
      (destructuring-bind (head &rest tail)
          (car forms)
        `(-> (,head ,form ,@tail) ,@(cdr forms)))))

(defmacro ->> (form &rest forms)
  "Threads FORM through FORMS. Inserts FORM as the last item in the first form of FORMS. If there are more forms, inserts the first form as the last item in second form, etc."
  (if (null forms)
      form
      `(->> ,(append (car forms) (list form))
            ,@(cdr forms))))

(defun get-in (alist keys &optional default)
  "Traverse potentially nested alist ALIST according to KEYS, fetching associated values."
  (cond ((null keys) (values alist t))
        ((null (cdr keys))
         (multiple-value-bind (value foundp)
             (alexandria:assoc-value alist (car keys))
           (if foundp
               (values value t)
               (values default nil))))
        (t
         (multiple-value-bind (value foundp)
             (alexandria:assoc-value alist (car keys))
           (if foundp
               (get-in value (cdr keys) default)
               (values default nil))))))

(defun hexstring-to-bytes (string &aux (n (length string)) (m (/ n 2)))
  "Parse STRING as a string of hex-encoded bytes."
  (unless (= (mod n 2) 0) (error "odd-length string"))
  (loop :with bytes := (make-array m :element-type '(unsigned-byte 8))
        :for i :from 0 :to (1- n) :by 2
        :do (setf (aref bytes (/ i 2))
                  (parse-integer string :start i :end (+ i 2) :radix 16))
        :finally (return bytes)))

(defun mapcar-with (function list &key initial-value)
  "Like MAPCAR, but supplies an accumulator to FUNCTION."
  (if (null list)
      (values nil initial-value)
      (multiple-value-bind (car car-context)
          (funcall function (car list) initial-value)
        (multiple-value-bind (cdr cdr-context)
            (mapcar-with function (cdr list) :initial-value car-context)
          (values (cons car cdr) cdr-context)))))

(defun walk-tree (function tree)
  "Apply FUNCTION to each node in TREE."
  (subst-if t (constantly nil) tree :key function))

(defun asubst (alist key datum)
  "Replace the association of KEY in ALIST with (KEY . DATUM)"
  (if (find key alist :key #'car)
      (substitute-if (cons key datum)
                     #'(lambda (k) (eql k key))
                     alist
                     :key #'car)
      (acons key datum alist)))

(defun psubst (plist indicator value)
  "Replace the value associated with KEY in PLIST with VALUE."
  (cond ((null plist))
        ((eql indicator (car plist))
         (cons (car plist)
               (cons value
                     (cddr plist))))
        (t (psubst (cddr plist) indicator value))))

(defun keccak256 (sequence &rest args &key (digest-start 0) digest end (start 0))
  "Compute the keccak-256 hash of SEQUENCE."
  (declare (ignorable digest-start digest end start))
  (apply #'ironclad:digest-sequence 'ironclad:keccak/256 sequence args))

(defconstant fail nil
  "Inidicates search failure.")

(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies GOAL-P. Start with STATES, and search according to SUCCESSORS and COMBINER."
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
            (funcall combiner
                     (funcall successors (first states))
                     (rest states))
            goal-p successors combiner))))

(defun graph-search (states goal-p successors combiner &optional (state= #'eql) old-states)
  "Find a state that satisfies GOAL-P. Start with STATES, and search according to SUCCESSORS and COMBINER. Don't try the same state twice."
  (cond ((null states) fail)
        ((funcall goal-p (first states) (first states)))
        (t (graph-search
            (funcall combiner
                     (new-states states successors state= old-states)
                     (rest states))
            goal-p successors combiner state=
            (adjoin (first states) old-states :test state=)))))

(defun new-states (states successors state= old-states)
  "Generate successor states that have not been seen before"
  (remove-if
   #'(lambda (state)
       (or (member state states :test state=)
           (member state old-states :test state=)))
   (funcall successors (first states))))
