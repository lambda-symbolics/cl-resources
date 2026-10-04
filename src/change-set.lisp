(in-package #:cl-resources)

;;;; -- Staging protocol --

(defclass resource-change ()
  ((resource :initarg :resource :reader resource-change-resource
             :documentation "Resource to mutate.")
   (base-revision :initarg :base-revision :reader resource-change-base-revision
                  :documentation "Required opaque base revision.")
   (operations :initarg :operations :reader resource-change-operations
               :documentation "Adapter-defined operation list."))
  (:documentation "One immutable revision-gated request in a change set."))

(defun make-resource-change (resource &key base-revision operations)
  "Construct a revision-gated request for RESOURCE."
  (check-type resource resource)
  (resource--check-revision-value base-revision (resource-uri resource))
  (check-type operations list)
  (make-instance 'resource-change :resource resource
                 :base-revision base-revision :operations operations))

(defclass prepared-change ()
  ((resource :initarg :resource :reader prepared-change-resource
             :documentation "Resource whose backend state this change controls.")
   (base-revision :initarg :base-revision :reader prepared-change-base-revision
                  :documentation "Exact revision used to prepare the change."))
  (:documentation "Backend-owned prepared payload and undo data for one request."))

(defgeneric resource-call-with-transaction (resource context thunk)
  (:documentation
   "Call THUNK exactly once in an exclusive backend scope. The scope must protect
reads, validation, commit and rollback, with unwind-safe release. Nested scopes
for distinct identities are acquired in lexical URI order by change sets.
The default rejects mutation instead of pretending that unlocked writes are safe.")
  (:method ((resource resource) context thunk)
    (declare (ignore context thunk))
    (error 'resource-operation-unsupported :uri (resource-uri resource)
           :operation ':transaction)))

(defmethod resource-call-with-transaction :around ((resource resource) context thunk)
  "Require exactly one synchronous invocation of the backend-scope body."
  (let ((calls 0)
        (active-p t))
    (unwind-protect
         (multiple-value-prog1
             (call-next-method
              resource context
              (lambda ()
                (unless (and active-p (zerop calls))
                  (error 'resource-protocol-error :uri (resource-uri resource)
                         :reason "transaction body must be called exactly once synchronously"))
                (incf calls)
                (funcall thunk)))
           (unless (= calls 1)
             (error 'resource-protocol-error :uri (resource-uri resource)
                    :reason "transaction scope did not invoke its body")))
      (setf active-p nil))))

(defgeneric resource-stage-operations (resource context &key base-revision operations)
  (:documentation
   "Prepare OPERATIONS without changing observable state. Return a prepared-change.
If preparation exits without returning a handle, the adapter must release any
partially allocated staging state. CONTEXT supplies external authorization.")
  (:method ((resource resource) context &key base-revision operations)
    (declare (ignore context base-revision operations))
    (error 'resource-operation-unsupported :uri (resource-uri resource)
           :operation ':stage)))

(defgeneric prepared-change-validate (change context)
  (:documentation "Validate prepared payload and current base revision under backend scope.")
  (:method ((change prepared-change) context)
    (declare (ignore change context))
    t))

(defmethod prepared-change-validate :around ((change prepared-change) context)
  "Enforce the optimistic base guard independently of adapter payload validation."
  (resource-check-revision (prepared-change-resource change) context
                           (prepared-change-base-revision change))
  (call-next-method))

(defgeneric prepared-change-commit (change context)
  (:documentation
   "Install prepared state under backend scope. A failed or interrupted call may
have mutated state; rollback will be called even for that failing change.")
  (:method ((change prepared-change) context)
    (declare (ignore context))
    (error 'resource-operation-unsupported
           :uri (resource-uri (prepared-change-resource change)) :operation ':commit)))

(defgeneric prepared-change-rollback (change context)
  (:documentation
   "Restore exact pre-commit state, even after partial commit. Must be idempotent
and safe to retry following partial rollback failure, with undo data retained.")
  (:method ((change prepared-change) context)
    (declare (ignore context))
    (error 'resource-operation-unsupported
           :uri (resource-uri (prepared-change-resource change)) :operation ':rollback)))

(defgeneric prepared-change-discard (change context)
  (:documentation
   "Release uncommitted or successfully rolled-back staging state. Idempotent;
retain enough state to retry if release fails. No observable mutation is allowed.")
  (:method ((change prepared-change) context)
    (declare (ignore context))
    (error 'resource-operation-unsupported
           :uri (resource-uri (prepared-change-resource change)) :operation ':discard)))

(defgeneric prepared-change-finalize (change context)
  (:documentation
   "Release undo/staging data after publication. Idempotent and retryable. Failure
does not invalidate published state; recovery retries cleanup, never rollback.")
  (:method ((change prepared-change) context)
    (declare (ignore context))
    (error 'resource-operation-unsupported
           :uri (resource-uri (prepared-change-resource change)) :operation ':finalize)))

;;;; -- Change set state machine --

(defstruct (change-entry (:constructor change-entry--make (change)))
  "Internal retry journal for one backend prepared change."
  change
  (phase ':prepared))

(defclass change-set ()
  ((requests :initarg :requests :reader change-set--requests
             :documentation "Original requests in caller order.")
   (context :initarg :context :reader change-set--context
            :documentation "Explicit external caller context.")
   (entries :initform nil :accessor change-set--entries
            :documentation "Prepared payloads and completed lifecycle steps.")
   (state :initform ':staging :accessor change-set-state
          :documentation "Lifecycle status; caller must not set this slot.")
   (observations :initform nil :reader change-set-observations
                 :documentation "Published observations, or NIL before publication.")
   (publisher :initarg :publisher :initform nil :reader change-set--publisher
              :documentation "Optional atomic publication callback."))
  (:documentation "Recoverable, in-memory staged transaction across resource adapters."))

(define-condition change-set-error (resource-error)
  ((set :initarg :set :reader change-set-error-set
        :documentation "Recoverable transaction containing retained backend handles.")
   (cause :initarg :cause :initform nil :reader change-set-error-cause
          :documentation "Original failure, if any.")
   (failures :initarg :failures :reader change-set-error-failures
             :documentation "Cleanup errors in attempted execution order."))
  (:documentation "Rollback or post-publication cleanup requires explicit recovery.")
  (:report (lambda (condition stream)
             (format stream "Change set is ~S; ~D recovery failure(s)."
                     (change-set-state (change-set-error-set condition))
                     (length (change-set-error-failures condition))))))

(define-condition change-set-state-error (resource-error)
  ((state :initarg :state :reader change-set-state-error-state
          :documentation "Lifecycle state that rejected the operation."))
  (:documentation "An operation was invoked outside its allowed lifecycle state.")
  (:report (lambda (condition stream)
             (format stream "Change set operation is invalid in state ~S."
                     (change-set-state-error-state condition)))))

(defun change-set--require-state (set allowed)
  "Reject operations outside ALLOWED states."
  (unless (member (change-set-state set) allowed)
    (error 'change-set-state-error :state (change-set-state set))))

(defun change-set--with-scopes (set thunk)
  "Acquire all backend scopes in lexical identity order and call THUNK."
  (let ((resources (sort (mapcar #'resource-change-resource
                                (copy-list (change-set--requests set)))
                         #'string< :key #'resource-uri)))
    (labels ((acquire (remaining)
               (if remaining
                   (resource-call-with-transaction
                    (first remaining) (change-set--context set)
                    (lambda () (acquire (rest remaining))))
                   (funcall thunk))))
      (acquire resources))))

(defun change-set--cleanup (set published-p)
  "Retry pending cleanup. Return a list of errors and update the lifecycle state."
  (let ((failures nil)
        (context (change-set--context set)))
    (dolist (entry (if published-p
                      (change-set--entries set)
                      (reverse (change-set--entries set))))
      (handler-case
          (let ((change (change-entry-change entry)))
            (if published-p
                (unless (eq (change-entry-phase entry) ':finalized)
                  (prepared-change-finalize change context)
                  (setf (change-entry-phase entry) ':finalized))
                (progn
                  (when (member (change-entry-phase entry) '(:started :committed))
                    (prepared-change-rollback change context)
                    (setf (change-entry-phase entry) ':rolled-back))
                  (unless (eq (change-entry-phase entry) ':discarded)
                    (prepared-change-discard change context)
                    (setf (change-entry-phase entry) ':discarded)))))
        (error (condition)
          (push condition failures))))
    (setf (change-set-state set)
          (if failures
              (if published-p ':cleanup-required ':recovery-required)
              (if published-p ':committed ':aborted)))
    (nreverse failures)))

(defun change-set--cleanup-or-error (set published-p &optional cause)
  "Perform cleanup, exposing retained recovery state if a backend step fails."
  (let ((failures (change-set--cleanup set published-p)))
    (when failures
      (restart-case
          (error 'change-set-error :set set :cause cause :failures failures)
        (retry-recovery ()
          :report "Retry pending rollback or cleanup with retained adapter state."
          (change-set--cleanup-or-error set published-p cause))))))

(defun change-set-stage (requests context &key publisher)
  "Stage REQUESTS under backend scopes and return a change-set in :staged state.
Duplicate canonical URIs are rejected. PUBLISHER receives SET, the complete
observation list, and CONTEXT exactly once on successful commit. A publisher must
atomically publish or signal without externally visible partial publication."
  (check-type requests list)
  (when publisher (check-type publisher function))
  (let ((identities (make-hash-table :test #'equal)))
    (dolist (request requests)
      (check-type request resource-change)
      (let ((uri (resource-uri (resource-change-resource request))))
        (when (gethash uri identities)
          (error 'resource-protocol-error :uri uri
                 :reason "duplicate resource identity in change set"))
        (setf (gethash uri identities) t))))
  (let ((set (make-instance 'change-set :requests (copy-list requests)
                            :context context :publisher publisher))
        (staged-p nil)
        (cause nil))
    (change-set--with-scopes
     set
     (lambda ()
       (unwind-protect
            (handler-bind ((error (lambda (condition) (setf cause condition))))
              ;; Validate every revision before allocating any prepared payload.
              (dolist (request requests)
                (resource-check-revision (resource-change-resource request) context
                                         (resource-change-base-revision request)))
              (dolist (request requests)
                (let* ((resource (resource-change-resource request))
                       (change (resource-stage-operations
                                resource context
                                :base-revision (resource-change-base-revision request)
                                :operations (resource-change-operations request))))
                  (unless (typep change 'prepared-change)
                    (error 'resource-protocol-error :uri (resource-uri resource)
                           :reason "stage must return a prepared-change"))
                  ;; Retain even a malformed returned handle for cleanup.
                  (setf (change-set--entries set)
                        (append (change-set--entries set)
                                (list (change-entry--make change))))
                  (unless (and (eq resource (prepared-change-resource change))
                               (equal (resource-change-base-revision request)
                                      (prepared-change-base-revision change)))
                    (error 'resource-protocol-error :uri (resource-uri resource)
                           :reason "prepared change must match resource and base revision"))))
              (setf (change-set-state set) ':staged
                    staged-p t))
         (unless staged-p
           (change-set--cleanup-or-error set nil cause)))))
    set))

(defun change-set-commit (set)
  "Revalidate all bases, commit all changes, observe, and publish once.
Failures before publication restore every started backend, in reverse order.
After publication, failed cleanup is retryable without republishing or rollback.
Return SET and its observations as two values."
  (change-set--require-state set '(:staged))
  (let ((published-p nil)
        (cause nil))
    (change-set--with-scopes
     set
     (lambda ()
       (change-set--require-state set '(:staged))
       (setf (change-set-state set) ':committing)
       (unwind-protect
            (handler-bind ((error (lambda (condition) (setf cause condition))))
              (dolist (entry (change-set--entries set))
                (prepared-change-validate (change-entry-change entry)
                                          (change-set--context set)))
              (dolist (entry (change-set--entries set))
                ;; Mark before calling: a failing commit may have mutated state.
                (setf (change-entry-phase entry) ':started)
                (prepared-change-commit (change-entry-change entry)
                                        (change-set--context set))
                (setf (change-entry-phase entry) ':committed))
              (let ((observations
                      (mapcar (lambda (request)
                                (resource-observe (resource-change-resource request)
                                                  (change-set--context set)))
                              (change-set--requests set))))
                (when (change-set--publisher set)
                  (funcall (change-set--publisher set) set observations
                           (change-set--context set)))
                (setf (slot-value set 'observations) observations
                      published-p t
                      (change-set-state set) ':published)))
         (change-set--cleanup-or-error set published-p cause)))))
  (values set (change-set-observations set)))

(defun change-set-abort (set)
  "Discard a staged set. An already aborted set is an idempotent no-op."
  (change-set--require-state set '(:staged :aborted))
  (unless (eq (change-set-state set) ':aborted)
    (change-set--with-scopes
     set (lambda () (change-set--cleanup-or-error set nil))))
  set)

(defun change-set-recover (set)
  "Retry failed rollback/discard or post-publication finalization.
Completed steps are not repeated. Use the retained set in change-set-error."
  (change-set--require-state set '(:recovery-required :cleanup-required :aborted :committed))
  (when (member (change-set-state set) '(:recovery-required :cleanup-required))
    (let ((published-p (eq (change-set-state set) ':cleanup-required)))
      (change-set--with-scopes
       set (lambda () (change-set--cleanup-or-error set published-p)))))
  set)

(defmethod resource-apply-operations ((resource resource) context
                                    &key base-revision operations)
  "Use the staged protocol for a revision-gated single-resource mutation."
  (let ((set (change-set-stage
              (list (make-resource-change resource :base-revision base-revision
                                          :operations operations))
              context)))
    (change-set-commit set)
    (first (change-set-observations set))))
