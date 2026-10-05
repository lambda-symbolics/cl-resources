(in-package #:cl-resources)

(defclass test-handle-resource (callback-resource)
  ((handles :initform (make-hash-table :test #'equal) :reader test-resource-handles)
   (guards :initform 0 :accessor test-resource-guards))
  (:documentation "Adapter fixture resolving caller-owned handles to literal revisions."))

(defmethod resource-check-revision ((resource test-handle-resource) context expected)
  "Resolve only handles owned by CONTEXT, then use the literal default guard."
  (incf (test-resource-guards resource))
  (let ((literal (gethash (list context expected) (test-resource-handles resource))))
    (unless literal
      (error 'resource-revision-stale :uri (resource-uri resource)
             :expected-revision expected :actual-revision nil))
    (call-next-method resource context literal)))

(defun test-caller-scoped-revision-guard ()
  "Exercise the adapter guard at staging and commit, including stale handles."
  (let* ((backend (test-backend-make "handles"))
         (resource (change-class (test-backend-resource backend) 'test-handle-resource)))
    (setf (gethash (list ':allowed "opaque:1") (test-resource-handles resource)) "v0")
    (test-condition 'resource-revision-stale
                    (lambda () (resource-check-revision resource ':other "opaque:1")))
    (let ((set (change-set-stage
                (list (make-resource-change resource :base-revision "opaque:1"
                                            :operations '(20))) ':allowed)))
      (test-check (= (test-resource-guards resource) 2))
      (change-set-commit set)
      (test-check (= (test-resource-guards resource) 3))
      (test-check (= (test-backend-value backend) 20)))
    (test-condition 'resource-revision-stale
                    (lambda () (change-set-stage
                                (list (make-resource-change resource :base-revision "opaque:1"
                                                            :operations '(30))) ':allowed)))
    (setf (gethash (list ':allowed "opaque:2") (test-resource-handles resource)) "v1")
    (let ((set (change-set-stage
                (list (make-resource-change resource :base-revision "opaque:2"
                                            :operations '(30))) ':allowed)))
      (incf (test-backend-revision backend))
      (test-condition 'resource-revision-stale (lambda () (change-set-commit set)))
      (test-check (eq (change-set-state set) ':aborted))
      (test-check (= (test-backend-value backend) 20))))
  (let* ((backend (test-backend-make "literal"))
         (resource (test-backend-resource backend)))
    (test-check (string= "v0" (resource-observation-revision
                                (resource-check-revision resource ':allowed "v0"))))
    (let ((condition (test-condition 'resource-revision-stale
                                    (lambda () (resource-check-revision resource ':allowed "V0")))))
      (test-check (string= "V0" (resource-revision-stale-expected-revision condition)))
      (test-check (string= "v0" (resource-revision-stale-actual-revision condition))))
    (test-condition 'resource-protocol-error
                    (lambda () (resource-check-revision resource ':allowed "")))))
