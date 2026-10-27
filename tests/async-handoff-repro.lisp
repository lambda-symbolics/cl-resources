(in-package #:cl-resources)

;;;; -- Optional SBCL cancellation ownership diagnostic --

#+sbcl
(defun test-stage-handoff-reproduce (&key defer-p)
  "Interrupt the exact prepared-handle return-to-registration window.
Temporarily gate the internal entry constructor; run this diagnostic alone.
The fixture allocates no external assets. DEFER-P models application-owned SBCL
interruption deferral through retaining the returned set."
  (let* ((backend    (test-backend-make "async-handoff"))
         (ready      (bordeaux-threads:make-semaphore))
         (release    (bordeaux-threads:make-semaphore))
         (original   (symbol-function 'change-entry--make))
         (outcome    nil)
         (discards   nil)
         (retained-p nil)
         (thread     nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'change-entry--make)
                 (lambda (change)
                   (bordeaux-threads:signal-semaphore ready)
                   (test-wait release)
                   (funcall original change)))
           (setf thread
                 (test-thread
                  (lambda ()
                    (let ((set nil))
                      (unwind-protect
                           (setf outcome
                                 (catch 'handoff-cancelled
                                   (if defer-p
                                       (sb-sys:without-interrupts
                                         (setf set (test-stage (list backend))))
                                       (setf set (test-stage (list backend))))))
                        (setf retained-p (not (null set)))
                        (when set (change-set-abort set))
                        (setf discards (count ':discard *test-events* :key #'second)))))))
           (test-wait ready)
           (bordeaux-threads:interrupt-thread
            thread (lambda () (throw 'handoff-cancelled ':cancelled)))
           (when defer-p (bordeaux-threads:signal-semaphore release))
           (let ((result (bordeaux-threads:join-thread thread)))
             (setf thread nil)
             (unless (eq result ':success) (error result)))
           (list :outcome outcome :retained-set retained-p :discards discards))
      (bordeaux-threads:signal-semaphore release)
      (when thread (bordeaux-threads:join-thread thread))
      (setf (symbol-function 'change-entry--make) original))))

#+sbcl
(defun reproduce-asynchronous-stage-handoff ()
  "Report the unprotected handoff and verify application-owned cancellation deferral.
Load cl-resources/tests and this file, then call this function by itself."
  (let ((unprotected (test-stage-handoff-reproduce))
        (protected   (test-stage-handoff-reproduce :defer-p t)))
    (test-check (eq (getf protected :outcome) ':cancelled))
    (test-check (getf protected :retained-set)
                "application captures the set before deferred cancellation")
    (test-check (= (getf protected :discards) 1)
                "the retained set owns and releases its prepared handle")
    (list :unprotected unprotected :application-deferral protected)))
