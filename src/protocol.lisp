(in-package #:cl-resources)

;;;; -- Conditions --

(define-condition resource-error (error)
  ((uri :initarg :uri :initform nil :reader resource-error-uri
        :documentation "Identity associated with this failure, if available."))
  (:documentation "Base condition for resource protocol failures."))

(define-condition resource-uri-malformed (resource-error)
  ((reason :initarg :reason :reader resource-uri-malformed-reason
           :documentation "The violated URI grammar rule."))
  (:documentation "A value is not a simple lowercase scheme:identifier URI.")
  (:report (lambda (condition stream)
             (format stream "Malformed resource URI ~S: ~A"
                     (resource-error-uri condition)
                     (resource-uri-malformed-reason condition)))))

(define-condition resource-scheme-unknown (resource-error)
  ((scheme :initarg :scheme :reader resource-scheme-unknown-scheme
           :documentation "The unregistered URI scheme."))
  (:documentation "No resolver is registered for a valid scheme.")
  (:report (lambda (condition stream)
             (format stream "Unknown resource scheme ~S."
                     (resource-scheme-unknown-scheme condition)))))

(define-condition resource-access-denied (resource-error)
  ()
  (:documentation "An adapter's external authority policy rejected access.")
  (:report (lambda (condition stream)
             (format stream "Access denied to ~S." (resource-error-uri condition)))))

(define-condition resource-operation-unsupported (resource-error)
  ((operation :initarg :operation :reader resource-operation-unsupported-operation
              :documentation "The rejected operation identifier."))
  (:documentation "A resource or adapter cannot implement this operation.")
  (:report (lambda (condition stream)
             (format stream "Resource ~S does not support ~S."
                     (resource-error-uri condition)
                     (resource-operation-unsupported-operation condition)))))

(define-condition resource-revision-stale (resource-error)
  ((expected-revision :initarg :expected-revision
                      :reader resource-revision-stale-expected-revision
                      :documentation "The caller's base revision.")
   (actual-revision :initarg :actual-revision
                    :reader resource-revision-stale-actual-revision
                    :documentation "Current revision, or NIL for absent state."))
  (:documentation "An optimistic revision guard rejected a changed resource.")
  (:report (lambda (condition stream)
             (format stream "Resource ~S changed from ~S to ~S."
                     (resource-error-uri condition)
                     (resource-revision-stale-expected-revision condition)
                     (resource-revision-stale-actual-revision condition)))))

(define-condition resource-protocol-error (resource-error)
  ((reason :initarg :reason :reader resource-protocol-error-reason
           :documentation "The adapter contract violation."))
  (:documentation "An adapter returned an invalid protocol value.")
  (:report (lambda (condition stream)
             (format stream "Resource protocol violation for ~S: ~A"
                     (resource-error-uri condition)
                     (resource-protocol-error-reason condition)))))

;;;; -- Identities and observations --

(defun resource-uri--valid-scheme-p (scheme)
  "Return a boolean for the strict lowercase ASCII URI scheme grammar."
  (and (stringp scheme) (plusp (length scheme))
       (loop for character across scheme
             for index from 0
             always (or (char<= #\a character #\z)
                        (and (plusp index)
                             (or (char<= #\0 character #\9)
                                 (find character "+.-")))))
       t))

(defun resource-uri-parse (uri)
  "Return scheme and identifier from strict SCHEME:IDENTIFIER URI.
Identifiers are opaque graphic text, spaces included. No decoding, path
normalization, network authority interpretation or authorization is performed."
  (flet ((reject (reason)
           (error 'resource-uri-malformed :uri uri :reason reason)))
    (unless (stringp uri)
      (reject "the URI must be a string"))
    (let ((separator (position #\: uri)))
      (unless separator
        (reject "the URI must contain a scheme separator"))
      (let ((scheme (subseq uri 0 separator))
            (identifier (subseq uri (1+ separator))))
        (unless (resource-uri--valid-scheme-p scheme)
          (reject "the scheme must use lowercase ASCII URI scheme characters"))
        (unless (and (plusp (length identifier))
                     (every #'graphic-char-p identifier))
          (reject "the identifier must be nonempty graphic text"))
        (values scheme identifier)))))

(defun resource--check-revision-value (revision uri)
  "Require a nonempty opaque string revision and return it."
  (unless (and (stringp revision) (plusp (length revision)))
    (error 'resource-protocol-error :uri uri
           :reason "revision must be a nonempty string"))
  revision)

(defclass resource ()
  ((uri :initarg :uri :reader resource-uri
        :documentation "Stable identity, never a capability or authority grant."))
  (:documentation "Authority-neutral resource identity resolved by an adapter."))

(defclass resource-observation ()
  ((uri :initarg :uri :reader resource-observation-uri
        :documentation "Canonical resource identity.")
   (revision :initarg :revision :reader resource-observation-revision
             :documentation "Opaque string identifying the exact observed state.")
   (content :initarg :content :reader resource-observation-content
            :documentation "Adapter-defined observed content.")
   (metadata :initarg :metadata :initform nil :reader resource-observation-metadata
             :documentation "Adapter-defined metadata, including bounded views."))
  (:documentation "Revisioned value without retention or caller authority policy."))

(defmethod initialize-instance :after ((resource resource) &key)
  "Validate a resource's stable identity."
  (resource-uri-parse (resource-uri resource)))

(defmethod initialize-instance :after ((observation resource-observation) &key)
  "Validate observation identity and opaque revision."
  (resource-uri-parse (resource-observation-uri observation))
  (resource--check-revision-value (resource-observation-revision observation)
                                (resource-observation-uri observation)))

(defgeneric resource-capabilities (resource context)
  (:documentation "Return supported operation identifiers under explicit CONTEXT.")
  (:method ((resource resource) context)
    (declare (ignore resource context))
    nil))

(defgeneric resource-observe (resource context)
  (:documentation "Return a resource-observation under explicit caller CONTEXT.")
  (:method ((resource resource) context)
    (declare (ignore context))
    (error 'resource-operation-unsupported :uri (resource-uri resource)
           :operation ':observe)))

(defmethod resource-observe :around ((resource resource) context)
  "Check adapter observations, including canonical identity agreement."
  (declare (ignore context))
  (let ((observation (call-next-method)))
    (unless (and (typep observation 'resource-observation)
                 (string= (resource-uri resource)
                          (resource-observation-uri observation)))
      (error 'resource-protocol-error :uri (resource-uri resource)
             :reason "observe must return an observation of the same identity"))
    observation))

(defgeneric resource-check-revision (resource context expected-revision)
  (:documentation
   "Validate EXPECTED-REVISION and return the current observation.
Call within the backend transaction scope before mutation. Adapters may
specialize this guard to resolve caller-scoped opaque revision handles.")
  (:method ((resource resource) context expected-revision)
    "Compare a literal observation revision for an ordinary adapter."
    (resource--check-revision-value expected-revision (resource-uri resource))
    (let* ((observation (resource-observe resource context))
           (actual (resource-observation-revision observation)))
      (unless (string= expected-revision actual)
        (error 'resource-revision-stale :uri (resource-uri resource)
               :expected-revision expected-revision :actual-revision actual))
      observation)))

(defgeneric resource-apply-operations (resource context &key base-revision operations)
  (:documentation "Apply a revision-gated single-resource change and return an observation."))

;;;; -- Resolver registry --

(defclass resource-resolver ()
  ((scheme :initarg :scheme :reader resource-resolver-scheme
           :documentation "Strict lowercase URI scheme handled by this resolver."))
  (:documentation "CLOS resolver boundary, with caller context passed explicitly."))

(defmethod initialize-instance :after ((resolver resource-resolver) &key)
  "Validate resolver scheme grammar."
  (unless (resource-uri--valid-scheme-p (resource-resolver-scheme resolver))
    (error 'resource-uri-malformed :uri (resource-resolver-scheme resolver)
           :reason "invalid resolver scheme")))

(defgeneric resource-resolver-resolve (resolver identifier context)
  (:documentation "Resolve opaque IDENTIFIER under explicit caller CONTEXT.")
  (:method ((resolver resource-resolver) identifier context)
    (declare (ignore context))
    (error 'resource-operation-unsupported
           :uri (format nil "~A:~A" (resource-resolver-scheme resolver) identifier)
           :operation ':resolve)))

(defclass resource-registry ()
  ((resolvers :initform (make-hash-table :test #'equal)
              :reader resource-registry--resolvers
              :documentation "Per-registry scheme to resolver mapping."))
  (:documentation "An explicit resolver registry without process-global policy."))

(defun make-resource-registry ()
  "Return an empty resolver registry."
  (make-instance 'resource-registry))

(defun resource-registry-register (registry resolver)
  "Install RESOLVER, returning it and the previous resolver as two values."
  (check-type resolver resource-resolver)
  (let* ((scheme (resource-resolver-scheme resolver))
         (previous (gethash scheme (resource-registry--resolvers registry))))
    (setf (gethash scheme (resource-registry--resolvers registry)) resolver)
    (values resolver previous)))

(defun resource-registry-unregister (registry scheme)
  "Remove SCHEME, returning the removed resolver and a presence boolean."
  (multiple-value-bind (resolver present-p)
      (gethash scheme (resource-registry--resolvers registry))
    (remhash scheme (resource-registry--resolvers registry))
    (values resolver present-p)))

(defun resource-registry-schemes (registry)
  "Return a fresh sorted list of registered schemes."
  (sort (loop for scheme being the hash-keys of (resource-registry--resolvers registry)
              collect scheme)
        #'string<))

(defun resource-registry-find (registry scheme)
  "Return the resolver for SCHEME, or NIL, without resolving or authorizing access."
  (gethash scheme (resource-registry--resolvers registry)))

(defun resource-registry-resolve (registry uri context)
  "Resolve URI with REGISTRY and CONTEXT, without an implicit authority policy."
  (multiple-value-bind (scheme identifier) (resource-uri-parse uri)
    (let ((resolver (resource-registry-find registry scheme)))
      (unless resolver
        (error 'resource-scheme-unknown :uri uri :scheme scheme))
      (let ((resource (resource-resolver-resolve resolver identifier context)))
        (unless (typep resource 'resource)
          (error 'resource-protocol-error :uri uri
                 :reason "resolver must return a resource"))
        resource))))
