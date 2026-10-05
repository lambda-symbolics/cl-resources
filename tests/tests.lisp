(in-package #:cl-resources)

;;;; -- Dependency-free behavioral test harness --

(defvar *test-checks* 0)
(defvar *test-events* nil)
(defvar *test-scopes* nil)

(define-condition test-injected-failure (error)
  ((step :initarg :step :reader test-failure-step))
  (:documentation "Deterministic backend fault injection."))

(defun test-check (value &optional (description "check"))
  "Record one behavioral assertion."
  (incf *test-checks*)
  (unless value (error "Failed: ~A" description)))

(defun test-condition (type thunk)
  "Require THUNK to signal TYPE, returning the condition."
  (let ((condition (handler-case (progn (funcall thunk) nil)
                     (error (condition) condition))))
    (test-check (typep condition type) (format nil "expected ~S, got ~S" type condition))
    condition))

(defstruct test-backend
  "Mutable external service fixture with counted fault injection."
  resource
  (value 0)
  (revision 0)
  (failures (make-hash-table :test #'eq)))

(defun test-backend--step (backend step)
  "Record STEP and inject a counted fault at this exact boundary."
  (push (list (resource-uri (test-backend-resource backend)) step) *test-events*)
  (when (plusp (gethash step (test-backend-failures backend) 0))
    (decf (gethash step (test-backend-failures backend)))
    (error 'test-injected-failure :step step)))

(defun test-backend--authorize (backend context)
  "Model adapter-owned authorization, including repeated checks."
  (unless (eq context ':allowed)
    (error 'resource-access-denied :uri (resource-uri (test-backend-resource backend)))))

(defun test-backend--revision (backend)
  "Return a deterministic opaque fixture revision."
  (format nil "v~D" (test-backend-revision backend)))

(defun test-backend-make (name)
  "Return a callback resource backend exercising all protocol boundaries."
  (let ((backend (make-test-backend)))
    (setf (test-backend-resource backend)
          (make-instance
           'callback-resource :uri (format nil "test:~A" name)
           :observe
           (lambda (resource context)
             (test-backend--authorize backend context)
             (test-backend--step backend ':observe)
             (make-instance 'resource-observation :uri (resource-uri resource)
                            :revision (test-backend--revision backend)
                            :content (test-backend-value backend)
                            :metadata (list :exists t)))
           :transaction
           (lambda (resource context thunk)
             (test-backend--authorize backend context)
             (test-backend--step backend ':lock)
             (test-check (not (member (resource-uri resource) *test-scopes* :test #'equal))
                         "no duplicate lock")
             (let ((*test-scopes* (append *test-scopes* (list (resource-uri resource)))))
               (unwind-protect (funcall thunk)
                 (test-backend--step backend ':unlock))))
           :stage
           (lambda (resource context &key base-revision operations)
             (test-backend--authorize backend context)
             (test-backend--step backend ':stage)
             (test-check (member (resource-uri resource) *test-scopes* :test #'equal)
                         "stage is locked")
             (let ((old-value (test-backend-value backend))
                   (old-revision (test-backend-revision backend)))
               (make-instance
                'callback-prepared-change :resource resource :base-revision base-revision
                :validate
                (lambda (change context)
                  (declare (ignore change))
                  (test-backend--authorize backend context)
                  (test-backend--step backend ':validate))
                :commit
                (lambda (change context)
                  (declare (ignore change))
                  (test-backend--authorize backend context)
                  (test-check (member (resource-uri resource) *test-scopes* :test #'equal)
                              "commit is locked")
                  (test-backend--step backend ':commit-before)
                  (setf (test-backend-value backend) (first operations))
                  (incf (test-backend-revision backend))
                  (test-backend--step backend ':commit-after))
                :rollback
                (lambda (change context)
                  (declare (ignore change))
                  (test-backend--authorize backend context)
                  (test-check (member (resource-uri resource) *test-scopes* :test #'equal)
                              "rollback is locked")
                  (test-backend--step backend ':rollback-before)
                  (setf (test-backend-value backend) old-value
                        (test-backend-revision backend) old-revision)
                  (test-backend--step backend ':rollback-after))
                :discard
                (lambda (change context)
                  (declare (ignore change))
                  (test-backend--authorize backend context)
                  (test-backend--step backend ':discard))
                :finalize
                (lambda (change context)
                  (declare (ignore change))
                  (test-backend--authorize backend context)
                  (test-backend--step backend ':finalize)))))))
    backend))

(defun test-request (backend &optional (value 10))
  "Create a request at the current fixture state."
  (make-resource-change (test-backend-resource backend)
                        :base-revision (test-backend--revision backend)
                        :operations (list value)))

(defun test-stage (backends &key publisher)
  "Stage fixture requests in caller order."
  (change-set-stage (mapcar #'test-request backends) ':allowed :publisher publisher))

(defun test-fail-at (backend step &optional (count 1))
  "Inject COUNT faults in STEP."
  (setf (gethash step (test-backend-failures backend)) count))

(defun test-event-count (backend step)
  "Count fixture events at one lifecycle step."
  (count (list (resource-uri (test-backend-resource backend)) step)
         *test-events* :test #'equal))

(defun test-original-state (backends)
  "Require exact restoration of value and revision."
  (dolist (backend backends)
    (test-check (zerop (test-backend-value backend)) "value restored")
    (test-check (zerop (test-backend-revision backend)) "revision restored")))

(defun test-identities-and-registry ()
  "Exercise URI grammar, resolver lifecycle and explicit context."
  (dolist (uri '("x:a" "work-space+v1.2:path with spaces" "file:/a:b" "x:%20" "x://host/key"))
    (multiple-value-bind (scheme identifier) (resource-uri-parse uri)
      (test-check (string= uri (format nil "~A:~A" scheme identifier)))))
  (dolist (uri (list nil 1 "" ":a" "x:" "X:a" "1x:a" "x_:a" "abc"
                    (format nil "x:a~%b") (format nil "x:a~Cb" #\Tab)))
    (test-condition 'resource-uri-malformed (lambda () (resource-uri-parse uri))))
  (let* ((registry (make-resource-registry))
         (contexts nil)
         (resolver (make-instance 'callback-resolver :scheme "x"
                                  :resolve (lambda (resolver identifier context)
                                             (push context contexts)
                                             (make-instance 'resource :uri
                                                            (format nil "~A:~A"
                                                                    (resource-resolver-scheme resolver)
                                                                    identifier))))))
    (test-condition 'resource-scheme-unknown
                    (lambda () (resource-registry-resolve registry "x:a" ':caller)))
    (multiple-value-bind (installed previous) (resource-registry-register registry resolver)
      (test-check (eq installed resolver))
      (test-check (null previous)))
    (test-check (equal (resource-registry-schemes registry) '("x")))
    (let ((first (resource-registry-resolve registry "x:a" ':first))
          (second (resource-registry-resolve registry "x:a" ':second)))
      (test-check (equal (resource-uri first) (resource-uri second)))
      (test-check (equal contexts '(:second :first))))
    (multiple-value-bind (installed previous) (resource-registry-register registry resolver)
      (test-check (eq installed previous)))
    (multiple-value-bind (removed present-p) (resource-registry-unregister registry "x")
      (test-check (eq removed resolver))
      (test-check present-p))
    (test-check (not (nth-value 1 (resource-registry-unregister registry "x")))))
  (test-condition 'resource-uri-malformed
                  (lambda () (make-instance 'resource-resolver :scheme "BAD")))
  (test-condition 'resource-uri-malformed
                  (lambda () (make-instance 'resource :uri "bad")))
  (test-condition 'resource-protocol-error
                  (lambda () (make-instance 'resource-observation :uri "x:a"
                                            :revision "" :content nil)))
  (test-condition 'resource-protocol-error
                  (lambda () (make-instance 'resource-observation :uri "x:a"
                                            :revision 1 :content nil)))
  (let ((registry (make-resource-registry)))
    (resource-registry-register registry
                                (make-instance 'callback-resolver :scheme "x"
                                               :resolve (lambda (&rest arguments)
                                                          (declare (ignore arguments)) nil)))
    (test-condition 'resource-protocol-error
                    (lambda () (resource-registry-resolve registry "x:a" nil)))))

(defun test-observation-and-authority ()
  "Exercise operation discovery, read validation, stale guards and denial."
  (let* ((backend (test-backend-make "one"))
         (resource (test-backend-resource backend)))
    (test-check (equal (resource-capabilities resource ':allowed)
                       '(:observe :stage :apply-operations :transaction)))
    (let ((observation (resource-observe resource ':allowed)))
      (test-check (string= "test:one" (resource-observation-uri observation)))
      (test-check (equal (resource-observation-metadata observation) '(:exists t)))
      (test-check (zerop (resource-observation-content observation))))
    (test-condition 'resource-access-denied (lambda () (resource-observe resource ':denied)))
    (test-condition 'resource-access-denied
                    (lambda () (change-set-stage (list (test-request backend)) ':denied)))
    (let ((condition (test-condition 'resource-revision-stale
                                    (lambda () (resource-check-revision resource ':allowed "old")))))
      (test-check (equal (resource-revision-stale-expected-revision condition) "old"))
      (test-check (equal (resource-revision-stale-actual-revision condition) "v0")))
    (let ((observation (resource-apply-operations resource ':allowed
                                                :base-revision "v0" :operations '(42))))
      (test-check (= (resource-observation-content observation) 42))
      (test-check (equal (resource-observation-revision observation) "v1")))
    (test-condition 'resource-revision-stale
                    (lambda () (resource-apply-operations resource ':allowed
                                                         :base-revision "v0" :operations '(9))))
    (test-check (= (test-backend-value backend) 42)))
  (let ((abstract (make-instance 'resource :uri "x:a")))
    (test-check (null (resource-capabilities abstract nil)))
    (test-condition 'resource-operation-unsupported (lambda () (resource-observe abstract nil)))
    (test-condition 'resource-operation-unsupported
                    (lambda () (resource-stage-operations abstract nil)))
    (test-condition 'resource-operation-unsupported
                    (lambda () (resource-call-with-transaction abstract nil (lambda () t))))
    (test-condition 'resource-operation-unsupported
                    (lambda () (resource-resolver-resolve
                                (make-instance 'resource-resolver :scheme "x") "a" nil))))
  (dolist (result '(nil 3))
    (let ((resource (make-instance 'callback-resource :uri "x:a"
                                   :observe (lambda (&rest arguments)
                                              (declare (ignore arguments)) result))))
      (test-condition 'resource-protocol-error (lambda () (resource-observe resource nil)))))
  (let ((resource (make-instance 'callback-resource :uri "x:a"
                                 :observe (lambda (&rest arguments)
                                            (declare (ignore arguments))
                                            (make-instance 'resource-observation :uri "x:b"
                                                           :revision "1" :content nil)))))
    (test-condition 'resource-protocol-error (lambda () (resource-observe resource nil))))
  (let ((resource (make-instance 'callback-resource :uri "x:a")))
    (test-check (null (resource-capabilities resource nil)))
    (test-condition 'resource-operation-unsupported (lambda () (resource-observe resource nil))))
  (test-condition 'type-error (lambda () (make-instance 'callback-resource :uri "x:a" :observe 3))))

(defun test-success-and-lifecycle ()
  "Require staged isolation, lock ordering, atomic publication and lifecycle guards."
  (let* ((backends (mapcar #'test-backend-make '("z" "a" "m")))
         (publications 0)
         (set (test-stage backends
                          :publisher (lambda (set observations context)
                                       (test-check (eq (change-set-state set) ':committing))
                                       (test-check (eq context ':allowed))
                                       (test-check (= (length observations) 3))
                                       (test-check (equal *test-scopes* '("test:a" "test:m" "test:z")))
                                       (test-check (every (lambda (backend)
                                                            (= (test-backend-value backend) 10)) backends))
                                       (incf publications)))))
    (test-original-state backends)
    (test-check (eq (change-set-state set) ':staged))
    (test-check (null (change-set-observations set)))
    (multiple-value-bind (returned observations) (change-set-commit set)
      (test-check (eq returned set))
      (test-check (equal (mapcar #'resource-observation-uri observations)
                         '("test:z" "test:a" "test:m"))))
    (test-check (= publications 1))
    (test-check (eq (change-set-state set) ':committed))
    (test-check (null *test-scopes*))
    (dolist (backend backends)
      (test-check (= (test-event-count backend ':finalize) 1)))
    (test-condition 'change-set-state-error (lambda () (change-set-commit set)))
    (test-condition 'change-set-state-error (lambda () (change-set-abort set)))
    (change-set-recover set)
    (test-check (= publications 1)))
  (let* ((backend (test-backend-make "abort"))
         (set (test-stage (list backend))))
    (change-set-abort set)
    (change-set-abort set)
    (test-check (eq (change-set-state set) ':aborted))
    (test-check (= (test-event-count backend ':discard) 1))
    (test-check (zerop (test-event-count backend ':commit-before)))
    (test-original-state (list backend))
    (test-condition 'change-set-state-error (lambda () (change-set-commit set))))
  (let ((backend (test-backend-make "duplicate")))
    (test-condition 'resource-protocol-error
                    (lambda () (test-stage (list backend backend))))
    (test-check (zerop (test-event-count backend ':lock))))
  (let ((set (change-set-stage nil nil)))
    (change-set-commit set)
    (test-check (eq (change-set-state set) ':committed)))
  (let* ((backend (test-backend-make "stale"))
         (set (test-stage (list backend))))
    (incf (test-backend-revision backend))
    (setf (test-backend-value backend) 77)
    (test-condition 'resource-revision-stale (lambda () (change-set-commit set)))
    (test-check (eq (change-set-state set) ':aborted))
    (test-check (= (test-backend-value backend) 77))
    (test-check (zerop (test-event-count backend ':commit-before)))))

(defun test-failure-matrix ()
  "Inject preparation, validation, partial commit and observation failure at every position."
  (dolist (step '(:stage :validate :commit-before :commit-after :observe))
    (dotimes (position 3)
      (let* ((*test-events* nil)
             (backends (mapcar #'test-backend-make '("a" "b" "c")))
             (failing (nth position backends))
             (publications 0))
        (if (eq step ':stage)
            (progn
              (test-fail-at failing step)
              (test-condition 'test-injected-failure (lambda () (test-stage backends)))
              (dotimes (prior position)
                (test-check (= (test-event-count (nth prior backends) ':discard) 1))))
            (let ((set (test-stage backends
                                   :publisher (lambda (&rest arguments)
                                                (declare (ignore arguments))
                                                (incf publications)))))
              ;; Observe faults here fire during commit-time validation.
              (test-fail-at failing step)
              (test-condition 'test-injected-failure (lambda () (change-set-commit set)))
              (test-check (eq (change-set-state set) ':aborted))
              (test-check (null (change-set-observations set)))
              (test-check (zerop publications))
              (dolist (backend backends)
                (test-check (= (test-event-count backend ':discard) 1)))
              (when (member step '(:commit-before :commit-after))
                (test-check (equal
                             (loop for (uri event) in (reverse *test-events*)
                                   when (eq event ':rollback-before) collect uri)
                             (reverse (subseq '("test:a" "test:b" "test:c") 0 (1+ position))))))))
        (test-original-state backends)
        (test-check (null *test-scopes*)))))
  ;; A failing read after all commits must roll back every installed resource.
  (let* ((backends (mapcar #'test-backend-make '("a" "b")))
         (set (test-stage backends)))
    (let ((original (callback-change--commit
                     (change-entry-change (second (change-set--entries set))))))
      (setf (slot-value (change-entry-change (second (change-set--entries set))) 'commit)
            (lambda (change context)
              (funcall original change context)
              (test-fail-at (first backends) ':observe))))
    (test-condition 'test-injected-failure (lambda () (change-set-commit set)))
    (test-original-state backends)
    (test-check (eq (change-set-state set) ':aborted)))
  ;; Publication failure is compensated before any undo data is finalized.
  (let* ((backends (mapcar #'test-backend-make '("a" "b")))
         (set (test-stage backends :publisher (lambda (&rest arguments)
                                               (declare (ignore arguments))
                                               (error 'test-injected-failure :step ':publish)))))
    (test-condition 'test-injected-failure (lambda () (change-set-commit set)))
    (test-original-state backends)
    (test-check (null (change-set-observations set)))
    (test-check (eq (change-set-state set) ':aborted))))

(defun test-recovery-matrix ()
  "Exercise real compensation failure, repeated recovery and published cleanup retry."
  (dolist (step '(:rollback-before :rollback-after :discard))
    (dotimes (position 3)
      (let* ((*test-events* nil)
             (backends (mapcar #'test-backend-make '("a" "b" "c")))
             (failing (nth position backends))
             (set (test-stage backends)))
        (test-fail-at (third backends) ':commit-after)
        (test-fail-at failing step 2)
        (let ((condition (test-condition 'change-set-error (lambda () (change-set-commit set)))))
          (test-check (eq (change-set-error-set condition) set))
          (test-check (typep (change-set-error-cause condition) 'test-injected-failure))
          (test-check (= (length (change-set-error-failures condition)) 1)))
        (test-check (eq (change-set-state set) ':recovery-required))
        (test-condition 'change-set-error (lambda () (change-set-recover set)))
        (test-condition 'change-set-state-error (lambda () (change-set-commit set)))
        (change-set-recover set)
        (test-check (eq (change-set-state set) ':aborted))
        (test-original-state backends)
        (dolist (backend backends)
          (test-check (= (test-event-count backend ':commit-after) 1))
          (test-check (= (test-event-count backend ':discard)
                         (if (and (eq backend failing) (eq step ':discard)) 3 1)))))))
  (dotimes (position 3)
    (let* ((*test-events* nil)
           (backends (mapcar #'test-backend-make '("a" "b" "c")))
           (publications 0)
           (set (test-stage backends :publisher (lambda (&rest arguments)
                                                 (declare (ignore arguments))
                                                 (incf publications)))))
      (test-fail-at (nth position backends) ':finalize 2)
      (test-condition 'change-set-error (lambda () (change-set-commit set)))
      (test-check (eq (change-set-state set) ':cleanup-required))
      (test-check (= (length (change-set-observations set)) 3))
      (test-condition 'change-set-error (lambda () (change-set-recover set)))
      (change-set-recover set)
      (test-check (eq (change-set-state set) ':committed))
      (test-check (= publications 1))
      (dolist (backend backends)
        (test-check (= (test-backend-value backend) 10))
        (test-check (zerop (test-event-count backend ':rollback-before)))
        (test-check (= (test-event-count backend ':finalize)
                       (if (eq backend (nth position backends)) 3 1))))))
  ;; Failed discard of a staged set retains its payload for explicit recovery.
  (let* ((backend (test-backend-make "discard"))
         (set (test-stage (list backend))))
    (test-fail-at backend ':discard)
    (test-condition 'change-set-error (lambda () (change-set-abort set)))
    (change-set-recover set)
    (test-check (eq (change-set-state set) ':aborted)))
  ;; A cleanup condition exposes a set even when stage never returned one.
  (let* ((first (test-backend-make "first"))
         (second (test-backend-make "second")))
    (test-fail-at second ':stage)
    (test-fail-at first ':discard)
    (let* ((condition (test-condition 'change-set-error
                                      (lambda () (test-stage (list first second)))))
           (set (change-set-error-set condition)))
      (change-set-recover set)
      (test-check (eq (change-set-state set) ':aborted)))))

(defun test-nonlocal-exits-and-restarts ()
  "Require unwind cleanup for callback throws and a working retry restart."
  (let* ((backends (mapcar #'test-backend-make '("a" "b")))
         (set (test-stage backends :publisher (lambda (&rest arguments)
                                               (declare (ignore arguments))
                                               (throw 'test-exit ':escaped)))))
    (test-check (eq (catch 'test-exit (change-set-commit set)) ':escaped))
    (test-original-state backends)
    (test-check (eq (change-set-state set) ':aborted))
    (test-check (null *test-scopes*)))
  (let* ((backend (test-backend-make "restart"))
         (set (test-stage (list backend))))
    (test-fail-at backend ':finalize)
    (handler-bind ((change-set-error
                     (lambda (condition)
                       (declare (ignore condition))
                       (invoke-restart 'retry-recovery))))
      (change-set-commit set))
    (test-check (eq (change-set-state set) ':committed))
    (test-check (= (test-backend-value backend) 10))))

;;;; -- Interrupted recovery and external storage --

(defun test-interrupted-cleanup ()
  "Require retryable lifecycle state even when cleanup exits nonlocally."
  (dolist (step '(rollback discard finalize))
    (let* ((backend (test-backend-make "interrupted"))
           (set (test-stage (list backend)))
           (change (change-entry-change (first (change-set--entries set))))
           (original (slot-value change step))
           (interrupt-p t))
      (setf (slot-value change step)
            (lambda (change context)
              (test-condition 'change-set-state-error
                              (lambda () (change-set-recover set)))
              (funcall original change context)
              (when interrupt-p
                (setf interrupt-p nil)
                (throw 'test-interrupt ':interrupted))))
      (when (eq step 'rollback)
        (test-fail-at backend ':commit-after))
      (test-check (eq (catch 'test-interrupt
                       (handler-case
                           (if (eq step 'discard)
                               (change-set-abort set)
                               (change-set-commit set))
                         (test-injected-failure () ':unexpected)))
                     ':interrupted))
      (test-check (eq (change-set-state set)
                      (if (eq step 'finalize) ':cleanup-required ':recovery-required)))
      (test-check (null *test-scopes*))
      (change-set-recover set)
      (test-check (eq (change-set-state set)
                      (if (eq step 'finalize) ':committed ':aborted)))
      (test-check (= (test-backend-value backend) (if (eq step 'finalize) 10 0))))))

(defun test-adapter-contracts ()
  "Exercise rejected scope callbacks, malformed prepared handles and all-base validation."
  (let ((registry (make-resource-registry))
        (resolver (make-instance 'resource-resolver :scheme "x")))
    (test-check (null (resource-registry-find registry "x")))
    (resource-registry-register registry resolver)
    (test-check (eq (resource-registry-find registry "x") resolver)))
  (dolist (mode '(:none :twice :deferred))
    (let ((deferred nil)
          (calls 0))
      (let ((resource
              (make-instance
               'callback-resource :uri "x:scope"
               :transaction (lambda (resource context thunk)
                              (declare (ignore resource context))
                              (case mode
                                (:none nil)
                                (:twice (funcall thunk) (funcall thunk))
                                (:deferred (setf deferred thunk)))))))
        (test-condition 'resource-protocol-error
                        (lambda () (resource-call-with-transaction
                                    resource nil (lambda () (incf calls)))))
        (test-check (= calls (if (eq mode ':twice) 1 0)))
        (when deferred
          (test-condition 'resource-protocol-error deferred)
          (test-check (zerop calls))))))
  (let* ((*test-events* nil)
         (backends (mapcar #'test-backend-make '("a" "b" "c")))
         (requests (mapcar #'test-request backends)))
    (incf (test-backend-revision (third backends)))
    (test-condition 'resource-revision-stale
                    (lambda () (change-set-stage requests ':allowed)))
    (dolist (backend backends)
      (test-check (zerop (test-event-count backend ':stage)))))
  (dolist (mode '(:not-a-handle :wrong-base :wrong-resource))
    (let* ((*test-events* nil)
           (backend (test-backend-make "invalid"))
           (resource (test-backend-resource backend))
           (original (callback-resource--stage resource)))
      (setf (slot-value resource 'stage)
            (lambda (resource context &key base-revision operations)
              (if (eq mode ':not-a-handle)
                  nil
                  (let ((change (funcall original resource context
                                         :base-revision base-revision :operations operations)))
                    (case mode
                      (:wrong-base (setf (slot-value change 'base-revision) "other"))
                      (:wrong-resource
                       (setf (slot-value change 'resource)
                             (make-instance 'resource :uri "test:other"))))
                    change))))
      (test-condition 'resource-protocol-error (lambda () (test-stage (list backend))))
      (test-original-state (list backend))
      (unless (eq mode ':not-a-handle)
        (test-check (= (test-event-count backend ':discard) 1)))))
  ;; Lock acquisition failure leaves a returned staged set available for retry.
  (let* ((backends (mapcar #'test-backend-make '("a" "b")))
         (set (test-stage backends)))
    (test-fail-at (second backends) ':lock)
    (test-condition 'test-injected-failure (lambda () (change-set-commit set)))
    (test-check (eq (change-set-state set) ':staged))
    (test-original-state backends)
    (test-check (null *test-scopes*))
    (change-set-commit set)
    (test-check (eq (change-set-state set) ':committed))))

(defun test-file-read (path)
  "Read exact bytes from a real fixture file."
  (with-open-file (stream path :direction ':input :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun test-file-write (path bytes)
  "Replace a real fixture file's byte content."
  (with-open-file (stream path :direction ':output :element-type '(unsigned-byte 8)
                               :if-exists ':supersede :if-does-not-exist ':create)
    (write-sequence bytes stream)))

(defun test-file-resource (path name &key fail-after-write)
  "Build a real-file callback adapter with exact byte snapshots and partial-write fault."
  (make-instance
   'callback-resource :uri (format nil "file:~A" name)
   :observe (lambda (resource context)
              (declare (ignore context))
              (let ((bytes (test-file-read path)))
                (make-instance 'resource-observation :uri (resource-uri resource)
                               :revision (format nil "bytes-~{~2,'0X~}" (coerce bytes 'list))
                               :content bytes)))
   :transaction (lambda (resource context thunk)
                  ;; The fixture is deliberately serial; production adapters supply a lock.
                  (declare (ignore resource context))
                  (funcall thunk))
   :stage (lambda (resource context &key base-revision operations)
            (declare (ignore context))
            (let ((old-bytes (test-file-read path))
                  (new-bytes (copy-seq (first operations))))
              (make-instance
               'callback-prepared-change :resource resource :base-revision base-revision
               :commit (lambda (change context)
                         (declare (ignore change context))
                         (test-file-write path new-bytes)
                         (when fail-after-write
                           (error 'test-injected-failure :step ':file-write)))
               :rollback (lambda (change context)
                           (declare (ignore change context))
                           (test-file-write path old-bytes))
               :discard (lambda (change context)
                          (declare (ignore change context)) t)
               :finalize (lambda (change context)
                           (declare (ignore change context)) t))))))

(defun test-real-file-rollback ()
  "Verify exact byte restoration of two real files after partial installation."
  (uiop:with-temporary-file (:pathname first-path :stream first-stream)
    (close first-stream)
    (uiop:with-temporary-file (:pathname second-path :stream second-stream)
      (close second-stream)
      (let ((first-bytes (make-array 6 :element-type '(unsigned-byte 8)
                                     :initial-contents '(0 255 13 10 0 128)))
            (second-bytes (make-array 4 :element-type '(unsigned-byte 8)
                                      :initial-contents '(10 10 13 255)))
            (new-bytes (make-array 3 :element-type '(unsigned-byte 8)
                                   :initial-contents '(1 2 3))))
        (test-file-write first-path first-bytes)
        (test-file-write second-path second-bytes)
        (let* ((first (test-file-resource first-path "first"))
               (second (test-file-resource second-path "second" :fail-after-write t))
               (requests (mapcar (lambda (resource)
                                   (make-resource-change
                                    resource :base-revision
                                    (resource-observation-revision (resource-observe resource nil))
                                    :operations (list new-bytes)))
                                 (list first second)))
               (set (change-set-stage requests nil)))
          (test-check (equalp first-bytes (test-file-read first-path)))
          (test-check (equalp second-bytes (test-file-read second-path)))
          (test-condition 'test-injected-failure (lambda () (change-set-commit set)))
          (test-check (eq (change-set-state set) ':aborted))
          (test-check (equalp first-bytes (test-file-read first-path)))
          (test-check (equalp second-bytes (test-file-read second-path)))
          (let ((observation
                  (resource-apply-operations
                   first nil :base-revision
                   (resource-observation-revision (resource-observe first nil))
                   :operations (list new-bytes))))
            (test-check (equalp new-bytes (resource-observation-content observation)))
            (test-check (equalp new-bytes (test-file-read first-path)))))))))

(defun test-operation-discovery ()
  "Discover every callback subset and honor explicit context-sensitive discovery."
  (dotimes (mask 8)
    (let* ((callback (lambda (&rest arguments) (declare (ignore arguments)) nil))
           (observe-p (logbitp 0 mask))
           (stage-p (logbitp 1 mask))
           (transaction-p (logbitp 2 mask))
           (resource (make-instance 'callback-resource :uri "x:callbacks"
                                    :observe (when observe-p callback)
                                    :stage (when stage-p callback)
                                    :transaction (when transaction-p callback)))
           (capabilities (resource-capabilities resource nil)))
      (test-check (eq (not (null (member ':observe capabilities))) observe-p))
      (test-check (eq (not (null (member ':stage capabilities))) stage-p))
      (test-check (eq (not (null (member ':transaction capabilities))) transaction-p))
      (test-check (eq (not (null (member ':apply-operations capabilities)))
                      (and observe-p stage-p transaction-p)))))
  (let ((resource (make-instance 'callback-resource :uri "x:policy"
                                 :capabilities
                                 (lambda (resource context)
                                   (unless (eq context ':allowed)
                                     (error 'resource-access-denied :uri (resource-uri resource)))
                                   (list ':external-operation)))))
    (test-check (equal (resource-capabilities resource ':allowed) '(:external-operation)))
    (test-condition 'resource-access-denied (lambda () (resource-capabilities resource nil)))))

(defun run-tests ()
  "Run all protocol, lifecycle and fault-injection tests; return assertion count."
  (let ((*test-checks* 0) (*test-events* nil) (*test-scopes* nil))
    (test-identities-and-registry)
    (test-observation-and-authority)
    (test-success-and-lifecycle)
    (test-failure-matrix)
    (test-recovery-matrix)
    (test-nonlocal-exits-and-restarts)
    (test-interrupted-cleanup)
    (test-adapter-contracts)
    (test-real-file-rollback)
    (test-operation-discovery)
    (test-caller-scoped-revision-guard)
    (test-concurrent-lifecycle)
    (test-stage-scope-ownership)
    (format t "~&cl-resources: ~D checks passed.~%" *test-checks*)
    *test-checks*))
