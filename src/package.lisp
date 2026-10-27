(defpackage #:cl-resources
  (:documentation "Resources named by URIs, with revisions, and change sets that stage,
commit and recover changes to several resources together.")
  (:use #:cl)
  (:export
   #:resource #:resource-uri #:resource-observation
   #:resource-observation-uri #:resource-observation-revision
   #:resource-observation-content #:resource-observation-metadata
   #:resource-uri-parse #:resource-capabilities #:resource-observe
   #:resource-apply-operations #:resource-check-revision
   #:resource-resolver #:resource-resolver-scheme #:resource-resolver-resolve
   #:resource-registry #:make-resource-registry #:resource-registry-register
   #:resource-registry-unregister #:resource-registry-resolve
   #:resource-registry-schemes #:resource-registry-find #:retry-recovery
   #:resource-error #:resource-error-uri #:resource-uri-malformed
   #:resource-uri-malformed-reason #:resource-scheme-unknown
   #:resource-scheme-unknown-scheme #:resource-access-denied
   #:resource-operation-unsupported #:resource-operation-unsupported-operation
   #:resource-revision-stale #:resource-revision-stale-expected-revision
   #:resource-revision-stale-actual-revision #:resource-protocol-error
   #:resource-protocol-error-reason #:change-set-error #:change-set-error-set
   #:change-set-error-cause #:change-set-error-failures
   #:change-set-state-error #:change-set-state-error-state
   #:resource-change #:make-resource-change #:resource-change-resource
   #:resource-change-base-revision #:resource-change-operations
   #:resource-call-with-transaction #:resource-stage-operations
   #:prepared-change #:prepared-change-resource #:prepared-change-base-revision
   #:prepared-change-validate #:prepared-change-commit #:prepared-change-rollback
   #:prepared-change-discard #:prepared-change-finalize
   #:change-set #:change-set-state #:change-set-observations
   #:change-set-stage #:change-set-commit #:change-set-abort #:change-set-recover
   #:callback-resource #:callback-resolver #:callback-prepared-change
   #:callback-argument-invalid #:callback-argument-invalid-argument))
