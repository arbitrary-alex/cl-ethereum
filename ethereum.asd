(defsystem "ethereum"
  :version "0.0.1"
  :author "Alex Segura <alex@arbitraryexecution.com>"
  :description ""
  :depends-on ("alexandria"
               "cl-graph"
               "cl-json"
               "cl-smt-lib"
               "drakma"
               "ironclad"
               "split-sequence")
  :pathname "src/"
  :components
  ((:file "package")
   (:file "ast" :depends-on ("package"))
   (:file "utils" :depends-on ("package"))
   (:file "json" :depends-on ("package"))
   (:file "solc" :depends-on ("package" "utils"))
   (:file "tac" :depends-on ("solc"))
   (:file "cfg" :depends-on ("tac"))
   (:file "ssa" :depends-on ("cfg"))))
