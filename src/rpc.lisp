(in-package #:web3)

(defvar *provider* nil
  "The current web3 provider.")

(defmacro with-provider (provider-uri &body body)
  `(let ((*provider* ,provider-uri))
     ,@body))

(defun json-rpc (id method params)
  `((:jsonrpc . "2.0")
    (:id . ,id)
    (:method . ,method)
    (:params . ,params)))

(defun send-rpc (id method params &optional (uri *provider*))
  (drakma:http-request uri
                       :method :post
                       :content-type "application/json"
                       :content
                       (json:encode-json-to-string
                        (json-rpc id method params))))
