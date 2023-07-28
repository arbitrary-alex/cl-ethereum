(in-package #:cl-user)

(defpackage #:ethereum/utilities
  (:nicknames #:ethereum/utils)
  (:use #:cl)
  (:export #:->
           #:->>
           #:get-in
           #:hexstring-to-bytes
           #:mapcar-with
           #:walk-tree
           #:asubst
           #:psubst
           #:keccak256
           #:tree-search
           #:graph-search)
  (:documentation "Generic utilities."))

(defpackage #:ethereum/evm
  (:nicknames #:evm)
  (:use #:cl)
  (:export)
  (:documentation "Ethereum Virtual Machine tools."))

(defpackage #:ethereum/solidity/ast
  (:nicknames #:solidity-ast)
  (:use #:cl)
  (:shadow #:block :return)
  (:documentation "Solidity AST CLOS definitions."))

(defpackage #:ethereum/solidity
  (:nicknames #:solidity)
  (:use #:cl #:ethereum/utils)
  (:import-from #:alexandria
                #:assoc-value)
  (:export #:read-build-info-json
           #:compile-solidity)
  (:documentation "Solidity analysis tools."))

(defpackage #:web3
  (:use #:cl)
  (:export #:*provider*
           #:send-rpc)
  (:documentation "Web3 JSON-RPC API."))

(defpackage #:flashbots
  (:use #:cl)
  (:export #:*flashbots-uri*
           #:bundle
           #:private-transaction)
  (:documentation "Flashbots JSON-RPC API."))
