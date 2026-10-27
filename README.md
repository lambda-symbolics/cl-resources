# cl-resources

Authority-neutral, revisioned resources for Common Lisp. The library provides
URI resolution, observations, operation discovery and recoverable staged change
sets. Dependencies: ASDF and Bordeaux Threads. Package: `cl-resources`.

## Load and test

```lisp
(require :asdf)
(asdf:load-asd (truename "cl-resources.asd"))
(asdf:load-system "cl-resources")
(asdf:test-system "cl-resources")
```

From the repository root on SBCL:

```sh
sbcl --noinform --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "cl-resources.asd"))' \
  --eval '(asdf:test-system "cl-resources")'
```

## Identity, resolution and observation

A URI uses strict lowercase ASCII `scheme:identifier` syntax. Schemes begin
with a letter and continue with letters, digits, `+`, `.` or `-`. Identifiers
are nonempty opaque graphic strings, including spaces. The library preserves
colons, percent escapes and slashes literally. Adapters own normalization and
return canonical resource identities. A URI grants no authority.

- `resource-uri-parse` returns scheme and identifier as two values.
- `make-resource-registry` creates an explicit registry.
- `resource-registry-register` returns installed and previous resolver.
- `resource-registry-find` looks up a scheme without resolution or authorization.
- `resource-registry-unregister` returns removed resolver and presence boolean.
- `resource-registry-schemes` returns a sorted fresh list.
- `resource-registry-resolve` takes registry, URI and explicit caller context.

Subclass `resource-resolver` and implement
`resource-resolver-resolve(resolver identifier context)`. Initialize resolvers
with `:scheme`. Resolve to a `resource` initialized with canonical `:uri`.

Implement `resource-observe(resource context)` to return a
`resource-observation` with `:uri`, nonempty string `:revision`, `:content` and
optional `:metadata`. Revisions are opaque exact-state identifiers supplied by
the adapter. Missing resources can have an observation and revision; encode
existence in content or metadata. Reads may be bounded views, but the revision
must guard all state relevant to the requested mutation, not just visible text.
The library checks observation identity and revision shape.

`resource-check-revision(resource context expected-revision)` is the mutation
revision guard. Its default method observes the resource and compares literal
revisions with `string=`. Adapters may specialize it to resolve caller-scoped
opaque handles, authorize their use, validate current backend state and return
the current observation. Both staging and prepared-change validation invoke
this guard within backend scopes. Retain the submitted handle as the prepared
change's base revision; the library does not retain or resolve handles itself.

Implement `resource-capabilities(resource context)` to return supported
operation identifiers. The base resource reports no operations. Discovery is
informational; authorize every operation independently. The application owns
caller policy, retained observations, aliases, credentials and presentation.
Serialize registry mutation externally if registering concurrently.

## Staged changes

```lisp
(let ((set (cl-resources:change-set-stage
            (list (cl-resources:make-resource-change
                   resource :base-revision revision :operations operations))
            context
            :publisher (lambda (set observations context)
                         (declare (ignore set context))
                         (publish-observations-atomically observations)))))
  (cl-resources:change-set-commit set))
```

`resource-apply-operations(resource context &key base-revision operations)`
uses the same staged protocol for one resource and returns its new observation.
The operation list is adapter-defined. Treat requests, revisions, observations
and prepared payloads as immutable after submission.

`change-set-stage(requests context &key publisher)` validates all revisions,
prepares changes without changing visible state, and returns a `:staged` set.
It rejects duplicate canonical URIs. Preparation and commit use backend scopes
acquired in lexical URI order; commit revalidates all bases under those scopes.
Caller request order determines preparation, commit and observation order.

`change-set-commit(set)` installs all prepared changes, obtains all new
observations, invokes the publisher once, and releases undo data. It returns
the set and published observations as two values. Only successfully published
observations appear in `change-set-observations`. Without a publisher, assigning
that complete observation list to the set is the publication boundary.

`change-set-abort(set)` discards uncommitted staging data; repeated abort of an
aborted set is harmless. Committing an aborted or already committed set signals
`change-set-state-error`.

### Adapter lifecycle

Implement these generic functions, or use the callback bridge below:

| Operation | Contract |
| --- | --- |
| `resource-call-with-transaction(resource context thunk)` | Invoke thunk exactly once synchronously inside an exclusive, unwind-safe backend scope. |
| `resource-stage-operations(resource context &key base-revision operations)` | Return a `prepared-change` with the same resource object and base revision, retaining independent payload and undo state. |
| `prepared-change-validate(change context)` | Additional payload validation. The library checks the current base revision before the primary method. |
| `prepared-change-commit(change context)` | Install prepared state; a failed or interrupted call may have partially installed it. |
| `prepared-change-rollback(change context)` | Restore exact original state after successful, failed or partial commit. Retain undo state if rollback fails. |
| `prepared-change-discard(change context)` | Release uncommitted or successfully rolled-back staging data. |
| `prepared-change-finalize(change context)` | Release undo data after successful publication. |

Initialize `prepared-change` with `:resource` and `:base-revision`; subclasses
own additional payloads. Rollback, discard and finalize must be idempotent and
retryable after partial failure. If staging exits before returning a handle,
release its partial allocations inside the adapter. The base protocol rejects
unsupported transaction, preparation and lifecycle operations explicitly.

Adapters sharing a global lock must use a recursive lock, because a batch
nests one scope per identity. All cooperating readers and writers must use the
same backend synchronization. Remote services need a suitable transaction,
lease or version-checked compensation mechanism. Local lexical lock ordering
alone cannot make a remote service transactional. Scope callbacks must release
locks reliably even on nonlocal exit.

The publisher must atomically expose the complete batch or fail without any
externally visible publication. In particular, serial nontransactional external
notifications are not a valid atomic publisher. Publication executes within
backend scopes, before irreversible cleanup. Avoid nested change sets that
acquire overlapping scopes in a different order.

### Failure and recovery

Before publication, any validation, commit, observation or publisher failure
triggers reverse-order rollback of every started change, including a failing
commit that partially mutated state. Untouched changes are discarded. Once all
compensation succeeds, the set becomes `:aborted` and the original condition
propagates. Nonlocal exits also run compensation.

The staging journal owns registered prepared handles until the entire nested
scope call returns normally. A scope failure or nonlocal exit after preparation
triggers scoped discard, reacquiring scopes when necessary. If scope reacquisition or
discard fails or is interrupted, `change-set-error` exposes the retained set
in `:recovery-required`. This condition takes precedence over a nonlocal exit
when necessary to transfer ownership of unreleased handles. Its `retry-recovery`
restart reacquires scopes and retries cleanup; otherwise retain the set and
recover after unwinding. The original error, when present, is its `cause`.

A compensation failure signals `change-set-error`, with readers for `set`,
`cause` and the list of recovery `failures`. The set becomes
`:recovery-required`; retain it and call `change-set-recover`. Successfully
completed steps are not repeated. The `retry-recovery` restart retries pending
steps while current backend scopes are held. An interrupted rollback step
retains its previous phase for retry.
During cleanup, the transient `:recovering` state rejects reentrant lifecycle
calls. A nonlocal exit from rollback, discard or finalize leaves a retryable
pending state.

Lifecycle calls acquire a per-set Bordeaux Threads lock before backend scopes,
including for empty sets. Concurrent callers wait and recheck state after the
preceding call releases its lock. Calls on the same set from its own callbacks
signal `change-set-state-error` instead of recursively entering or deadlocking.
Use `retry-recovery` inside a failure handler, or retain the set and recover
after unwinding. Across different sets, avoid nested lifecycle calls that
reverse set-lock or backend-lock order. Lifecycle readers are diagnostic
snapshots; coordinate readers externally when an atomic view is required.

After publication, finalize failure produces `:cleanup-required` and the same
recoverable condition. Recovery retries only incomplete finalization. Committed
state and observations are preserved; publication is not repeated. Successful
cleanup yields `:committed`. Recovery of a terminal set is an idempotent no-op.

### Asynchronous cancellation ownership

Returning a prepared handle to the staging journal and returning a set to its
caller are ownership handoffs. Common Lisp has no portable interruption-deferral
primitive; the per-set lock serializes callers but does not make these handoffs
atomic against asynchronous thread cancellation. Applications that deliver such
cancellation must defer it across stage invocation, handle registration and
recording the returned set or recovery condition in an application-owned journal.
Deferring only inside `resource-stage-operations` leaves the return-to-registration
window unprotected. Publication likewise requires deferral across publisher
success and recording the library's publication marker.

Keep those handoffs short. Adapters may selectively permit interruption during
backend I/O once payload and undo ownership has been journaled. Preparation I/O
that permits interruption must first establish adapter-owned, unwind-safe cleanup
of partial allocations. The library does not disable cancellation in backend
callbacks or supply an implementation-specific interrupt policy.

Run the optional SBCL handoff diagnostic by itself from the repository root:

```sh
sbcl --noinform --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "cl-resources.asd"))' \
  --eval '(asdf:load-system "cl-resources/tests")' \
  --eval '(load "tests/async-handoff-repro.lisp")' \
  --eval '(format t "~S~%" (cl-resources::reproduce-asynchronous-stage-handoff))'
```

The diagnostic gates internal registration and interrupts a real thread. It
reports the unprotected handoff and checks that application-owned deferral
retains the set and releases its prepared handle. The fixture owns only memory;
it temporarily replaces the entry constructor and restores it on exit.

Recovery reacquires backend scopes. If other writers can intervene between
failure and recovery, the adapter must reject unsafe compensation instead of
overwriting unrelated changes. Keep the relevant lease or installed-version
ownership token in its prepared payload. Recovery state is in memory: durable
crash recovery requires backend-owned persisted handles/journals and an
application reconstruction policy. The library makes no cross-service atomic
visibility or process-crash durability promise for arbitrary callbacks.

## External callback bridge

`callback-resolver` takes `:scheme` and `:resolve`, a function of resolver,
identifier and context. `callback-resource` takes `:uri` and optional callbacks:

- `:observe`: `(resource context)` returning an observation.
- `:capabilities`: `(resource context)` returning operation identifiers.
- `:transaction`: `(resource context thunk)` providing the exclusive scope.
- `:stage`: `(resource context &key base-revision operations)` returning a handle.

Without an explicit capabilities callback, discovery reports callbacks present.
For context-sensitive discovery, supply `:capabilities`. Read-only adapters need
only `:observe`.

`callback-prepared-change` takes `:resource`, `:base-revision`, required
`:commit`, `:rollback`, `:discard`, `:finalize`, and optional `:validate`.
Each lifecycle callback takes `(change context)`. Capture service-specific
payload and undo state in closures or specialize `prepared-change` directly.
Translate external denials and revision conflicts into the typed conditions.

`cl-hashline` is an independent backend. A text resource adapter may use it to
prepare edits; this library has no hashline dependency, text editing policy or
credential storage.

## Conditions

All resource conditions inherit `resource-error`, with `resource-error-uri`.

- `resource-uri-malformed`: `:uri`, `:reason`.
- `resource-scheme-unknown`: `:uri`, `:scheme`.
- `resource-access-denied`: `:uri`.
- `resource-operation-unsupported`: `:uri`, `:operation`.
- `resource-revision-stale`: `:uri`, `:expected-revision`, `:actual-revision`.
- `resource-protocol-error`: `:uri`, `:reason`.
- `change-set-error`: `:set`, optional `:cause`, `:failures`.
- `change-set-state-error`: `:state`.

The callback bridge signals `callback-argument-invalid`, a `type-error` with
`:argument`, `:datum` and `:expected-type`, when `callback-resolver` gets a
`:resolve` that is not a function or `callback-prepared-change` gets a
`:resource` that is not a resource.

Specialized condition readers use the condition name as their prefix.
The generic base guard compares string revisions exactly with `string=`.
Adapters can report an absent current state with `:actual-revision nil` when
signaling their own stale condition.

## Development

```sh
./script/bootstrap
./script/check
./script/check --fix
```

`./script/check` runs the tests, then checks the source with
[cheque](https://github.com/lambda-symbolics/cheque) as `cheque.sexp`
configures it. `--fix` skips the tests and repairs what cheque can repair.
cheque and its tools are development dependencies: `checks/qlfile` lists them,
and the check installs them on first use into `~/.cache/cheque/tools/`.

## Tests and license

The tests cover URI grammar, resolver replacement/removal,
identity and observation validation, authority callbacks, operation discovery,
single and multiple resources, stale staging and commit, lexical scope ordering,
publication, nonlocal exits, repeated lifecycle calls and a deterministic
fault/recovery matrix at every batch position. Faults include partial commit,
partial rollback, discard and post-publication cleanup. Additional tests exercise
interrupted cleanup, rejected scope callbacks, malformed prepared handles and
exact byte restoration of real files after partial installation. Real concurrent
callers exercise empty-set publication and abort queued behind publication.

ISC license, copyright 2026 Lambda Symbolics.
