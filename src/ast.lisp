(in-package #:solidity-ast)

(defun build-ast (plist)
  (apply #'make-instance
         (getf plist :node-type)
         (alexandria:delete-from-plist plist :node-type)))

(defclass node ()
  ((id :type integer :initarg :id :reader node-id)
   (scope :initarg :scope)
   (source :initarg :src :reader node-source)))

(defclass directive (node) ())

(defclass pragma-directive (directive)
  ((literals :initarg :literals :reader pragma-directive-literals)))

(defclass import-directive (directive)
  ((absolute-path
    :initarg :absolute-path
    :reader import-directive-absolute-path)
   (file
    :initarg :file
    :reader import-directive-file)
   (source-unit
    :initarg :source-unit
    :reader import-directive-source-unit)
   (symbol-aliases
    :initarg :symbol-aliases
    :reader import-directive-symbol-aliases)
   (unit-alias
    :initarg :unit-alias
    :reader import-directive-unit-alias)))

(defclass block (node)
  ((statements :initarg :statements :reader block-statements)))

(defmethod initialize-instance :after ((block block) &key statements)
  (setf (slot-value block 'statements)
        (mapcar #'build-ast statements)))

(defclass statement (node) ())

(defclass return (statement)
  ((expression :reader return-expression)
   (function-return-parameters
    :initarg :function-return-parameters
    :reader return-function-return-parameters)))

(defmethod initialize-instance :after ((return return) &key expression)
  (setf (slot-value return 'expression) (build-ast expression)))

(defclass if-statement (statement)
  ((condition :initarg :condition :reader if-statement-condition)
   (true-body :initarg :true-body :reader if-statement-true-body)
   (false-body :initarg :false-body :reader if-statement-false-body)))

(defmethod initialize-instance :after ((if-statement if-statement) &key condition true-body false-body)
  (setf (slot-value if-statement 'condition) (build-ast condition)
        (slot-value if-statement 'true-body) (build-ast true-body)
        (slot-value if-statement 'false-body) (when false-body (build-ast false-body))))

(defclass for-statement (statement)
  ((body :reader for-statement-body)
   (condition :reader for-statement-condition)
   (initialization-expression :reader for-statement-initialization-expression)
   (loop-expression :reader for-statement-loop-expression)))

(defmethod initialize-instance :after
    ((for-statement for-statement) &key body condition initialization-expression loop-expression)
  (setf (slot-value for-statement 'body) (build-ast body)
        (slot-value for-statement 'condition) (build-ast condition)
        (slot-value for-statement 'initialization-expression) (build-ast initialization-expression)
        (slot-value for-statement 'loop-expression) (build-ast loop-expression)))

(defclass variable-declaration-statement (statement)
  ((assignments :initarg :assignments :reader variable-declaration-statement-assignments)
   (declarations :reader variable-declaration-statement-declarations)
   (initial-value :reader variable-declaration-statement-initial-value)))

(defmethod initialize-instance :after ((declaration variable-declaration-statement) &key declarations initial-value)
  ;; TODO: declarations seems incorrect
  (setf (slot-value declaration 'declarations) (build-ast (car declarations))
        (slot-value declaration 'initial-value)
        (when initial-value
          (build-ast initial-value))))

(defclass expression-statement (statement)
  ((expression :initarg :expression :reader expression-statement-expression)))

(defmethod initialize-instance :after ((expression-statement expression-statement) &key expression)
  (setf (slot-value expression-statement 'expression) (build-ast expression)))

(defclass assignment (expression)
  ((operator :initarg :operator :reader assignment-operator)
   (left-hand-side :initarg :left-hand-side :reader assignment-left-hand-side)
   (right-hand-side :initarg :right-hand-side :reader assignment-right-hand-side)))

(defmethod initialize-instance :after ((assignment assignment) &key left-hand-side right-hand-side)
  (setf (slot-value assignment 'left-hand-side) (build-ast left-hand-side)
        (slot-value assignment 'right-hand-side) (build-ast right-hand-side)))

(defclass specifier (node) ())

(defclass override-specifier (specifier)
  ((overrides :initarg :overrides :reader override-specifier-overrides)))

(defclass inheritance-specifier (specifier)
  ((base-name :reader inheritence-specifier-base-name)
   (arguments :initarg :arguments)))

(defmethod initialize-instance :after ((inheritance-specifier inheritance-specifier) &key base-name)
  (setf (slot-value inheritance-specifier 'base-name) (build-ast base-name)))

(defclass using-for-directive (node)
  ((libary-name :initarg :library-name :reader using-for-directive-library-name)
   (type-name :initarg :type-name :reader using-for-directive-type-name)))

(defclass user-defined-type-name (node)
  ((name :initarg :name :reader type-name)
   (contract-scope :initarg :contract-scope :reader contract-scope)
   (referenced-declaration :initarg :referenced-declaration)
   (type-descriptions :initarg :type-descriptions)))

(defclass expression (node)
  ((l-value-requested-p :initarg :l-value-requested :reader expression-l-value-requested-p)
   (l-value-p :initarg :is-lvalue :reader expression-l-value-p)
   (constant-p :initarg :is-constant :reader expression-constant-p)
   (pure-p :initarg :is-pure :reader expression-pure-p)
   (argument-types :initarg :argument-types :reader expression-argument-types)
   (type-name :initarg :type-name :reader expression-type-name)
   (type-descriptions :initarg :type-descriptions :reader expression-type-descriptions)))

(defclass new-expression (expression) ())

(defclass elementary-type-name-expression (expression) ())

(defclass function-call (expression)
  ((try-call-p :initarg :try-call :reader function-call-try-call-p)
   (kind :initarg :kind :reader function-call-kind)
   (names :initarg :names :reader function-call-names)
   (argument-types :initarg :argument-types :reader function-call-argument-types)
   (expression :initarg :expression :reader function-call-expression)
   (arguments :initarg :arguments :reader function-call-arguments)))

(defmethod initialize-instance :after ((function-call function-call) &key argument-types expression arguments)
  (setf (slot-value function-call 'argument-types) (mapcar #'build-ast argument-types)
        (slot-value function-call 'expression) (build-ast expression)
        (slot-value function-call 'arguments) (mapcar #'build-ast arguments)))

(defclass literal (expression)
  ((value :initarg :value :reader literal-value)
   (kind :initarg :kind :reader literal-kind)
   (hex-value :initarg :hex-value :reader literal-hex-value)
   (subdenomination :initarg :subdenomination :reader literal-subdenomination)))

(defclass tuple-expression (expression)
  ((components :initarg :components :reader tuple-expression-components)
   (inine-array-p :initarg :is-inline-array :reader tuple-expression-inline-array-p)))

(defmethod initialize-instance :after ((tuple-expression tuple-expression) &key components)
  (setf (slot-value tuple-expression 'components)
        (mapcar #'build-ast components)))

(defclass binary-operation (expression)
  ((common-type :initarg :common-type :reader binary-operation-common-type)
   (operator :initarg :operator :reader binary-operation-operator)
   (left-expression :initarg :left-expression :reader binary-operation-left-expression)
   (right-expression :initarg :right-expression :reader binary-operation-right-expression)))

(defmethod initialize-instance :after ((binary-operation binary-operation) &key left-expression right-expression)
  (setf (slot-value binary-operation 'left-expression) (build-ast left-expression)
        (slot-value binary-operation 'right-expression) (build-ast right-expression)))

(defclass unary-operation (expression)
  ((operator :initarg :operator :reader unary-operation-operator)
   (prefix :initarg :prefix :reader unary-operation-prefix)
   (sub-expression :reader unary-operation-sub-expression)))

(defmethod initialize-instance :after ((unary-operation unary-operation) &key sub-expression)
  (setf (slot-value unary-operation 'sub-expression) (build-ast sub-expression)))

(defclass index-access (expression)
  ((base-expression :reader index-access-base-expression)
   (index-expression :reader index-access-index-expression)))

(defmethod initialize-instance :after ((index-access index-access) &key base-expression index-expression)
  (setf (slot-value index-access 'base-expression) (build-ast base-expression)
        (slot-value index-access 'index-expression) (build-ast index-expression)))

(defclass member-access (expression)
  ((expression :initarg :expression :reader member-access-expression)
   (member-name :initarg :member-name :reader member-access-member-name)
   (referenced-declaration :initarg :referenced-declaration :reader member-access-referenced-declaration)))

(defmethod initialize-instance :after ((member-access member-access) &key expression)
  (setf (slot-value member-access 'expression) (build-ast expression)))

(defclass conditional (expression)
  ((condition :reader conditional-condition)
   (true-expression :reader conditional-true-expression)
   (false-expression :reader conditional-false-expression)))

(defmethod initialize-instance :after ((conditional conditional) &key condition true-expression false-expression)
  (setf (slot-value conditional 'condition) (build-ast condition)
        (slot-value conditional 'true-expression) (build-ast true-expression)
        (slot-value conditional 'false-expression) (build-ast false-expression)))

(defclass identifier (node)
  ((name :initarg :name :reader identifier-name)))

(defmethod initialize-instance :after ((identifier identifier) &key &allow-other-keys)
  ;; TODO: other keys?
  )

(defclass array-type-name (node)
  ((base-type :initarg :base-type :reader array-type-name-base-type)
   (length :initarg :length :reader array-type-name-length)
   (type-descriptions :initarg :type-descriptions :reader array-type-name-type-descriptions)))

(defclass event-definition (node)
  ((name :initarg :name :reader event-definition-name)
   (documentation :initarg :documentation :reader event-definition-documentation)
   (anonymous-p :initarg :anonymous :reader event-definition-anonymous-p)
   (parameters :reader event-definition-parameters)))

(defmethod initialize-instance :after ((event-definition event-definition) &key parameters)
  (setf (slot-value event-definition 'parameters)
        (build-ast parameters)))

(defclass elementary-type-name (node)
  ((name :initarg :name :reader elementary-type-name-name)
   (state-mutability :initarg :state-mutability :reader elementary-type-name-state-mutability)
   (type-descriptions :initarg :type-descriptions :reader elementary-type-name-type-descriptions)))

(deftype visibility ()
  '(member :internal :external :public :private))

(defclass variable-declaration (node)
  ((name :initarg :name :reader variable-declaration-name)
   (value :initarg :value :reader variable-declaration-value)
   (visibility :type visibility :initarg :visibility)
   (storage-location :initarg :storage-location)
   (mutability :initarg :mutability :reader variable-declaration-mutability)
   (type-descriptions :initarg :type-descriptions :reader variable-declaration-type-descriptions)
   (state-variable-p :initarg :state-variable :reader state-variable-p)
   (indexed-p :initarg :indexed :reader variable-declaration-indexed-p)
   (type-name :initarg :type-name :reader variable-declaration-type-name)
   (overrides :initarg :overrides :reader variable-declaration-overrides)
   (constant :initarg :constant :reader varialbe-declaration-constant-p)))

(defmethod initialize-instance :after
    ((variable-declaration variable-declaration) &key value type-name type-descriptions)
  (setf (slot-value variable-declaration 'value)
        (when value
          (build-ast value))
        (slot-value variable-declaration 'type-name) (build-ast type-name)))

(defclass parameter-list (node)
  ((parameters :initarg :parameters :reader parameter-list-parameters)))

(defmethod initialize-instance :after ((parameter-list parameter-list) &key parameters)
  (setf (slot-value parameter-list 'parameters)
        (mapcar #'build-ast parameters)))

(defclass function-definition (node)
  ((name :initarg :name :reader function-definition-name)
   (body :initarg :body :reader function-definition-body)
   (documentation :initarg :documentation :reader function-documentation)
   (implemented :initarg :implemented :reader function-implemented-p)
   (kind :initarg :kind :reader function-definition-kind)
   (modifiers :reader function-modifiers)
   (overrides :reader function-definition-overrides)
   (parameters :reader function-parameters)
   (function-selector :initarg :function-selector :reader function-selector)
   (return-parameters :initarg :parameters :reader function-return-parameters)
   (state-mutability :initarg :state-mutability :reader mutability)
   (virtual :initarg :virtual :reader function-virtual-p)
   (base-functions :initarg :base-functions :reader function-definition-base-functions)
   (visibility :initarg :visibility :reader visibility)))

(defmethod initialize-instance :after
    ((function-definition function-definition) &key body parameters return-parameters overrides modifiers)
  (setf (slot-value function-definition 'body)
        (when body
          (build-ast body))
        (slot-value function-definition 'parameters) (build-ast parameters)
        (slot-value function-definition 'return-parameters) (build-ast return-parameters)
        (slot-value function-definition 'overrides)
        (when overrides
          (build-ast overrides))
        (slot-value function-definition 'modifiers) (mapcar #'build-ast modifiers)))

(deftype contract-definition-kind ()
  '(member :library :contract))

(defclass contract-definition (node)
  ((name :initarg :name :reader contact-definition-name)
   (abstract-p :initarg :abstract-p :reader contract-definition-abstract-p)
   (fully-implemented-p :initarg :fully-implemented-p :reader contract-definition-fully-implemented-p)
   (kind :type contract-definition-kind :initarg :kind :reader contract-definition-kind)
   (base-contracts :reader contract-definition-base-contracts)
   (linearized-base-contracts :initarg :linearized-base-contracts)
   (dependencies :initarg :contract-dependencies)
   (documentation :initarg :documentation :reader contract-definition-documentation)
   (nodes :initarg :nodes :reader contract-definition-nodes)))

(defmethod initialize-instance :after
    ((contract-definition contract-definition) &key nodes contract-kind abstract fully-implemented base-contracts)
  (setf (slot-value contract-definition 'fully-implemented-p)  fully-implemented
        (slot-value contract-definition 'abstract-p) abstract
        (slot-value contract-definition 'kind) contract-kind
        (slot-value contract-definition 'nodes) (mapcar #'build-ast nodes)
        (slot-value contract-definition 'base-contracts) (mapcar #'build-ast base-contracts)))

(defclass source-unit (node)
  ((absolute-path :initarg :absolute-path :reader source-unit-absolute-path)
   (exports :initarg :exported-symbols)
   (nodes :initarg :nodes :reader source-unit-nodes)))

(defmethod initialize-instance :after ((source-unit source-unit) &key nodes)
  (setf (slot-value source-unit 'nodes)
        (mapcar #'build-ast nodes)))

(defclass inline-assembly (node)
  ((ast :initarg :ast :reader inline-assembly-ast)
   (evm-version :initarg :evm-version)
   (external-references :initarg :external-references)))

;; TODO: initialize inline assembly

(defclass yul-typed-name (node)
  ())

(defclass yul-function-call (node)
  ())

(defclass yul-literal (node)
  ())

(defclass yul-identifier (node)
  ())

(defclass yul-variable-declaration (node)
  ())

(defclass yul-block (node)
  ())

(defclass input-source ()
  ((name :initarg :name :reader source-name)
   (content :initarg :content :reader source-content)))

(defclass build-input ()
  ((language :initarg :language :reader build-input-language)
   (sources :initarg :sources :reader build-input-sources)
   (settings :initarg :settings :reader build-input-settings)))

(defmethod initialize-instance :after ((build-input build-input) &key sources)
  (flet ((convert-source (source)
           (destructuring-bind (name . content) source
             (make-instance 'input-source
                            :name name
                            :content content))))
    (setf (slot-value build-input 'sources)
          (mapcar #'convert-source (alexandria:plist-alist sources)))))

(defclass output-source ()
  ((id :initarg :id :reader output-source-id)
   (name :initarg :name :reader output-source-name)
   (ast :initarg :ast :reader output-source-ast)))

(defmethod initialize-instance :after ((output-source output-source) &key ast legacy-ast)
  (declare (ignore legacy-ast))
  (setf (slot-value output-source 'ast) (build-ast ast)))

(defclass solidity-error ()
  ((component :initarg :component :reader solidity-error-component)
   (formatted-message :initarg :formatted-message :reader solidity-error-formatted-message)
   (message :initarg :message :reader solidity-error-message)
   (severity :initarg :severity :reader solidity-error-severity)
   (source-location :initarg :source-location :reader solidity-error-source-location)
   (type :initarg :type :reader solidity-error-type)))

(defclass build-output ()
  ((errors :reader build-output-errors)
   (sources :initarg :sources :reader build-output-sources)
   (contracts :initarg :contracts :reader build-output-contracts)))

(defmethod initialize-instance :after ((build-output build-output) &key sources errors)
  (flet ((convert-source (source)
           (destructuring-bind (name . content) source
             (apply #'make-instance 'output-source :name name
                    content))))
    (setf (slot-value build-output 'sources)
          (mapcar #'convert-source (alexandria:plist-alist sources))
          (slot-value build-output 'errors)
          (mapcar #'(lambda (err)
                      (apply #'make-instance 'solidity-error err))
                  errors))))

(defclass build-info ()
  ((id :type integer :initarg :id :reader build-id)
   (format :initarg :format)
   (solc-version :initarg :solc-version :reader build-solc-version)
   (solc-long-version :initarg :solc-long-version :reader build-solc-long-version)
   (input :type build-input :initarg :input :reader build-input)
   (output :type build-output :initarg :output :reader build-output)))

(defmethod initialize-instance :after ((build-info build-info) &key _format input output)
  (when _format (setf (slot-value build-info 'format) _format))
  (setf (slot-value build-info 'input) (apply #'make-instance 'build-input input)
        (slot-value build-info 'output) (apply #'make-instance 'build-output output)))
