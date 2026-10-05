(in-package #:cl-resources)

(defun test-thread (thunk &key (name "resource test"))
  "Run THUNK in a real worker, returning errors rather than invoking its debugger."
  (bordeaux-threads:make-thread
   (lambda ()
     (let ((*test-checks* 0) (*test-events* nil) (*test-scopes* nil)
           (*print-readably* nil))
       (handler-case (progn (funcall thunk) ':success)
         (error (condition) condition)))) :name name))

(defun test-wait (semaphore)
  "Bound every fixture rendezvous so a broken lifecycle fails rather than hangs."
  (unless (bordeaux-threads:wait-on-semaphore semaphore :timeout 10)
    (error "Timed out waiting for concurrent fixture")))

(defun test-concurrent-empty-commits ()
  "Simultaneous callers publish an empty set once, without any backend lock."
  (dotimes (iteration 100)
    (let* ((start (bordeaux-threads:make-semaphore))
           (ready (bordeaux-threads:make-semaphore))
           (count-lock (bordeaux-threads:make-lock))
           (publications 0)
           (set (change-set-stage
                 nil nil :publisher
                 (lambda (set observations context)
                   (declare (ignore set observations context))
                   (bordeaux-threads:with-lock-held (count-lock)
                     (incf publications)))))
           (threads (loop repeat 8 collect
                          (test-thread
                           (lambda ()
                             (bordeaux-threads:signal-semaphore ready)
                             (test-wait start)
                             (change-set-commit set))))))
      (unwind-protect
           (progn
             (dotimes (i 8) (test-wait ready))
             (dotimes (i 8) (bordeaux-threads:signal-semaphore start)))
        (dotimes (i 8) (bordeaux-threads:signal-semaphore start)))
      (let ((results (mapcar #'bordeaux-threads:join-thread threads)))
        (test-check (= publications 1)
                    (format nil "empty set published ~D times" publications))
        (test-check (= (count ':success results) 1))
        (test-check (= (count-if (lambda (r) (typep r 'change-set-state-error)) results) 7))
        (test-check (eq (change-set-state set) ':committed))))))

(defun test-concurrent-publish-abort ()
  "A queued abort cannot compensate a publication with retained undo data."
  (let* ((backend (test-backend-make "queued-abort"))
         (resource (test-backend-resource backend))
         (scope (callback-resource--transaction resource))
         (backend-lock (bordeaux-threads:make-lock))
         (entered (bordeaux-threads:make-semaphore))
         (release (bordeaux-threads:make-semaphore))
         (abort-started (bordeaux-threads:make-semaphore))
         (abort-at-scope (bordeaux-threads:make-semaphore))
         (gate-p nil)
         (publications 0)
         (set nil))
    (setf (slot-value resource 'transaction)
          (lambda (resource context thunk)
            (when (and gate-p (string= (bordeaux-threads:thread-name
                                       (bordeaux-threads:current-thread)) "abort"))
              (bordeaux-threads:signal-semaphore abort-at-scope))
            (bordeaux-threads:with-lock-held (backend-lock)
              (when (and gate-p (string= (bordeaux-threads:thread-name
                                         (bordeaux-threads:current-thread)) "commit"))
                (bordeaux-threads:signal-semaphore entered)
                (test-wait release))
              (funcall scope resource context thunk))))
    (setf set (test-stage (list backend)
                         :publisher (lambda (&rest arguments)
                                      (declare (ignore arguments))
                                      (incf publications))))
    (test-fail-at backend ':finalize)
    (setf gate-p t)
    (let ((commit (test-thread (lambda () (change-set-commit set)) :name "commit"))
          (abort nil))
      (unwind-protect
           (progn
             (test-wait entered)
             (setf abort (test-thread
                          (lambda ()
                            (bordeaux-threads:signal-semaphore abort-started)
                            (change-set-abort set)) :name "abort"))
             (test-wait abort-started)
             ;; An unlocked implementation reaches the backend here; a serialized
             ;; caller waits for the set lock. Release either case after a bound.
             (bordeaux-threads:wait-on-semaphore abort-at-scope :timeout 0.05))
        (bordeaux-threads:signal-semaphore release))
      (test-check (typep (bordeaux-threads:join-thread commit) 'change-set-error))
      (let ((result (bordeaux-threads:join-thread abort)))
        (test-check (typep result 'change-set-state-error)
                    (format nil "queued abort returned ~A; state ~S, value ~D"
                            result (change-set-state set) (test-backend-value backend)))))
    (test-check (= publications 1))
    (test-check (= (test-backend-value backend) 10))
    (test-check (eq (change-set-state set) ':cleanup-required))
    (setf gate-p nil)
    (change-set-recover set)
    (test-check (= publications 1))
    (test-check (= (test-backend-value backend) 10))
    (test-check (eq (change-set-state set) ':committed))))

(defun test-lifecycle-reentrancy ()
  "Reject same-set calls from callbacks before waiting for a nonrecursive lock."
  (let ((set (change-set-stage
              nil nil :publisher
              (lambda (set observations context)
                (declare (ignore observations context))
                (dolist (operation (list #'change-set-commit #'change-set-abort
                                         #'change-set-recover))
                  (test-condition 'change-set-state-error (lambda () (funcall operation set))))))))
    (change-set-commit set)
    (test-check (eq (change-set-state set) ':committed))))

(defun test-concurrent-lifecycle ()
  "Exercise independent set synchronization and queued state revalidation."
  (test-concurrent-empty-commits)
  (test-concurrent-publish-abort)
  (test-lifecycle-reentrancy))
