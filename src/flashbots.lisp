(in-package #:flashbots)

(defvar *flashbots-uri* (puri:uri "relay.flashbots.net")
  "Flashbots relay URI.")

(defclass bundle ()
  ((transactions
    :initform nil
    :initarg :transactions
    :accessor bundle-transactions
    :documentation "A list of signed transactions to execute in an atomic bundle")
   (block-number
    :type integer
    :initform 0
    :initarg :block-number
    :accessor bundle-block-number
    :documentation "block number for which this bundle is valid on")
   (min-timestamp
    :type (or null integer)
    :initform nil
    :accessor bundle-min-timestamp
    :documentation "Number, the minimum timestamp for which this bundle is valid, in seconds since unix epoch (optional).")
   (max-timestamp
    :type (or null integer)
    :initform nil
    :accessor bundle-max-timestamp
    :documentation "Number, the maximum timestamp for which this bundle is valid, in seconds since unix epoch (optional).")
   (reverting-tx-hashes
    :initform nil
    :accessor bundle-reverting-tx-hashes
    :documentation "List of tx hashes that are allowed to revert (optional)."))
  (:documentation ""))

(defmethod json:encode-json ((bundle bundle) &optional stream)
  (with-slot (transactions block-number min-timestamp max-timestamp reverting-tx-hashes) bundle
    (json:with-object (stream)
      (json:as-object-member ("txs" stream)
        (json:with-array (stream)
          (dolist (tx transactions)
            (json:encode-array-member tx))))
      (json:encode-object-member "blockNumber" block-number stream)
      (json:encode-object-member "minTimestamp" min-timestamp stream)
      (json:encode-object-member "maxTimestamp" max-timestamp stream)
      (json:as-object-member ("revertingTxHashes" stream)
        (json:with-array (stream)
          (dolist (tx reverting-tx-hashes)
            (json:encode-array-member tx)))))))

(defclass private-transaction ()
  ((transaction
    :initarg :transaction
    :accessor private-transaction-transaction
    :documentation "")
   (max-block-number
    :initarg :max-block-number
    :accessor private-transaction-max-block-nunmber
    :documentation "")
   (preferences
    :initarg :preferences
    :accessor private-transaction-preferences
    :documentation ""))
  (:documentation ""))

(defmethod json:encode-json ((tx private-transaction) &optional stream)
  (with-slots (transaction max-block-number preferences) tx
    (json:with-object (stream)
      (json:encode-object-member "tx" transaction)
      (json:encode-object-member "maxBlockNumber" max-block-number)
      (json:encode-object-member "preferences" preferences))))
