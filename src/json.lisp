(in-package #:solidity)

(defun json-keywordify (name)
  "Intern the string NAME into the *JSON-SYMBOLS-PACKAGE*, after converting to lisp case."
  (json:json-intern (json:simplified-camel-case-to-lisp name)))

(defun json-symbolify (name)
  "Transfer NAME into lisp case, then try to find the named symbol in the current *PACKAGE*."
  (let ((name (json:simplified-camel-case-to-lisp name)))
    (multiple-value-bind (symbol status)
        (find-symbol name)
      (unless status
        (error "unknown symbol ~A" name))
      symbol)))

(defun read-build-info-json-plist (stream)
  (let (*key* *object*)
    (declare (special *key* *object*))
    (json:bind-custom-vars
        (:beginning-of-object
         #'(lambda () (setq *object* nil))
         :object-key
         #'(lambda (key)
             (setq *key* (json-keywordify key)))
         :object-value
         #'(lambda (value)
             (setq *object*
                   (append (list *key*
                                 (case *key*
                                   (:node-type
                                    (json-symbolify value))
                                   ((:state-mutability :visibility :kind :contract-kind)
                                    (json-keywordify value))
                                   (:src
                                    (mapcar #'parse-integer
                                            (split-sequence:split-sequence #\: value)))
                                   (t value)))
                           *object*)))
         :end-of-object
         #'(lambda () *object*)
         :object-scope '(*object* *key*))
      (json:decode-json stream))))

(defun read-build-info-json-alist (stream)
  (let (*key* *object*)
    (declare (special *key* *object*))
    (json:bind-custom-vars
        (:beginning-of-object
         #'(lambda () (setq *object* nil))
         :object-key
         #'(lambda (key)
             (setq *key* (json-keywordify key)))
         :object-value
         #'(lambda (value)
             (push (cons *key*
                         (case *key*
                           ((:node-type :state-mutability :visibility :kind :contract-kind)
                            (json-keywordify value))
                           (:src
                            (mapcar #'parse-integer
                                    (split-sequence:split-sequence #\: value)))
                           (t value)))
                   *object*))
         :end-of-object
         #'(lambda () (nreverse *object*))
         :object-scope '(*object* *key*))
      (json:decode-json stream))))

(defun read-build-info-json (stream &optional plistp)
  "Read and parse a solc input/output JSON from STREAM."
  (if plistp
      (read-build-info-json-plist stream)
      (read-build-info-json-alist stream)))

(defun read-build-info-json-from-string (string)
  "Parse a solc input/output JSON from STRING."
  (with-input-from-string (stream string)
    (read-build-info-json stream)))

#+nil
(defun encode-json (object)
  (json:bind-custom-vars)
  (json:encode-json object))
