(in-package #:cl-resources)

;;;; -- Optional external adapter boundaries --

(defclass callback-resolver (resource-resolver)
  ((resolve :initarg :resolve :reader callback-resolver--resolve
            :documentation "Function of resolver, opaque identifier and context."))
  (:documentation "Resolver bridge for external services without library-specific subclasses."))

(defmethod initialize-instance :after ((resolver callback-resolver) &key)
  "Require an explicit resolver callback."
  (unless (functionp (callback-resolver--resolve resolver))
    (error 'type-error :datum (callback-resolver--resolve resolver) :expected-type 'function)))

(defmethod resource-resolver-resolve ((resolver callback-resolver) identifier context)
  "Delegate resolution with explicit authority context."
  (funcall (callback-resolver--resolve resolver) resolver identifier context))

(defclass callback-resource (resource)
  ((observe :initarg :observe :initform nil :reader callback-resource--observe
            :documentation "Optional function of resource and context returning an observation.")
   (capabilities :initarg :capabilities :initform nil :reader callback-resource--capabilities
                 :documentation "Optional function of resource and context returning operations.")
   (transaction :initarg :transaction :initform nil :reader callback-resource--transaction
                :documentation "Optional function of resource, context and thunk providing backend scope.")
   (stage :initarg :stage :initform nil :reader callback-resource--stage
          :documentation "Optional function of resource and context, with base-revision/operations keywords."))
  (:documentation "Callback bridge to a service or independent text-edit backend."))

(defmethod initialize-instance :after ((resource callback-resource) &key)
  "Validate optional callbacks."
  (dolist (callback (list (callback-resource--observe resource)
                         (callback-resource--capabilities resource)
                         (callback-resource--transaction resource)
                         (callback-resource--stage resource)))
    (when callback (check-type callback function))))

(defmethod resource-capabilities ((resource callback-resource) context)
  "Discover callback operations, or ask the service for context-sensitive support."
  (if (callback-resource--capabilities resource)
      (funcall (callback-resource--capabilities resource) resource context)
      (append (when (callback-resource--observe resource) (list ':observe))
              (when (callback-resource--stage resource) (list ':stage))
              (when (and (callback-resource--observe resource)
                         (callback-resource--stage resource)
                         (callback-resource--transaction resource))
                (list ':apply-operations))
              (when (callback-resource--transaction resource) (list ':transaction)))))

(defmethod resource-observe ((resource callback-resource) context)
  "Observe external state through the callback boundary."
  (if (callback-resource--observe resource)
      (funcall (callback-resource--observe resource) resource context)
      (call-next-method)))

(defmethod resource-call-with-transaction ((resource callback-resource) context thunk)
  "Delegate backend synchronization without assuming remote atomicity."
  (if (callback-resource--transaction resource)
      (funcall (callback-resource--transaction resource) resource context thunk)
      (call-next-method)))

(defmethod resource-stage-operations ((resource callback-resource) context
                                      &key base-revision operations)
  "Delegate preparation of independent external payload and undo state."
  (if (callback-resource--stage resource)
      (funcall (callback-resource--stage resource) resource context
               :base-revision base-revision :operations operations)
      (call-next-method)))

(defclass callback-prepared-change (prepared-change)
  ((validate :initarg :validate :initform nil :reader callback-change--validate
             :documentation "Optional additional payload validator of change and context.")
   (commit :initarg :commit :reader callback-change--commit
           :documentation "Function of change and context installing prepared state.")
   (rollback :initarg :rollback :reader callback-change--rollback
             :documentation "Idempotent function of change and context restoring exact prior state.")
   (discard :initarg :discard :reader callback-change--discard
            :documentation "Idempotent function of change and context discarding staging data.")
   (finalize :initarg :finalize :reader callback-change--finalize
             :documentation "Idempotent function of change and context releasing committed undo data."))
  (:documentation "External adapter's explicit prepared payload lifecycle callbacks."))

(defmethod initialize-instance :after ((change callback-prepared-change) &key)
  "Require every mutation and cleanup callback, with optional payload validation."
  (unless (typep (prepared-change-resource change) 'resource)
    (error 'type-error :datum (prepared-change-resource change) :expected-type 'resource))
  (resource--check-revision-value (prepared-change-base-revision change)
                                (resource-uri (prepared-change-resource change)))
  (dolist (callback (list (callback-change--commit change)
                         (callback-change--rollback change)
                         (callback-change--discard change)
                         (callback-change--finalize change)))
    (check-type callback function))
  (let ((validator (callback-change--validate change)))
    (when validator (check-type validator function))))

(defmethod prepared-change-validate ((change callback-prepared-change) context)
  "Check base revision before any optional service payload validation."
  (call-next-method)
  (when (callback-change--validate change)
    (funcall (callback-change--validate change) change context)))

(defmethod prepared-change-commit ((change callback-prepared-change) context)
  "Install external prepared state."
  (funcall (callback-change--commit change) change context))

(defmethod prepared-change-rollback ((change callback-prepared-change) context)
  "Restore external state after failed or partial installation."
  (funcall (callback-change--rollback change) change context))

(defmethod prepared-change-discard ((change callback-prepared-change) context)
  "Release discarded external staging state."
  (funcall (callback-change--discard change) change context))

(defmethod prepared-change-finalize ((change callback-prepared-change) context)
  "Release external undo state after successful publication."
  (funcall (callback-change--finalize change) change context))
