(in-package #:idsmall)

;;;; -- Entropy --

(defun identifier--seeded-random-index (limit)
  "Return an operating-system-seeded index below LIMIT.

Entropy quality only spreads identifiers evenly across the available seeds. It
never affects correctness, because allocation probes every remaining seed once
before reporting exhaustion."
  (random limit
          #+sbcl (sb-ext:seed-random-state t)
          #-sbcl (make-random-state t)))

(defvar *random-index-function* #'identifier--seeded-random-index
  "The function returning the first probe index below a supplied limit.

Bind this to a deterministic function to make allocation reproducible.")


;;;; -- Reservations --

(defvar *reservations* (make-hash-table :test #'equal)
  "Identifiers allocated in this process but not yet reported as occupied.")

(defvar *lock* (make-lock "idsmall identifiers")
  "Serializes allocation and reservation across the threads of this process.")

(defun identifier-reserved-p (identifier &key (namespace t))
  "Return true when IDENTIFIER is reserved in NAMESPACE by this process."
  (with-lock-held (*lock*)
    (not (null (gethash (identifier--reservation-key identifier namespace)
                        *reservations*)))))

(defun identifier-release (identifier &key (namespace t))
  "Drop IDENTIFIER's reservation in NAMESPACE.

Return true when a reservation existed. Release an identifier once the caller's
own storage holds it, or once the work that requested it is abandoned."
  (with-lock-held (*lock*)
    (not (null (remhash (identifier--reservation-key identifier namespace)
                        *reservations*)))))

(defun identifier-clear-reservations ()
  "Drop every reservation this process holds, in every namespace."
  (with-lock-held (*lock*)
    (clrhash *reservations*))
  nil)

(defun identifier--reservation-key (identifier namespace)
  "Return the reservation table key for canonical IDENTIFIER within NAMESPACE."
  (cons namespace (identifier-normalize identifier)))


;;;; -- Allocation --

(defun identifier-generate (&key (timestamp (get-universal-time))
                                 occupied-p
                                 (namespace t)
                                 reserved-identifiers)
  "Allocate and reserve one identifier for universal TIMESTAMP in NAMESPACE.

*RANDOM-INDEX-FUNCTION* chooses the first seed. A candidate that is already
taken causes every remaining seed to be probed exactly once, so allocation
either succeeds or reports genuine exhaustion.

A candidate is taken when it is already reserved in NAMESPACE, when it appears
in RESERVED-IDENTIFIERS, or when OCCUPIED-P is supplied and returns true for
it. OCCUPIED-P receives one candidate identifier and reports whether the
caller's own storage already holds it; it runs while the allocation lock is
held, so it must not call back into this library. RESERVED-IDENTIFIERS extends
the taken set with a list of identifiers, which suits migration planning.

The returned identifier stays reserved until IDENTIFIER-RELEASE drops it, so
concurrent callers in this process never receive it twice. Signal
IDENTIFIER-SPACE-EXHAUSTED when all IDENTIFIER-BASE seeds are taken for
TIMESTAMP, which bounds one namespace to IDENTIFIER-BASE identifiers per
second."
  (let ((excluded (identifier--canonical-members reserved-identifiers)))
    (with-lock-held (*lock*)
      (let ((first-seed (identifier--first-seed)))
        (loop for probe below (identifier-base)
              for seed-index = (mod (+ first-seed probe) (identifier-base))
              for candidate = (identifier-from-seed timestamp seed-index)
              for key = (identifier--reservation-key candidate namespace)
              unless (or (member candidate excluded :test #'string=)
                         (gethash key *reservations*)
                         (and occupied-p (funcall occupied-p candidate)))
                do (setf (gethash key *reservations*) t)
                   (return candidate)
              finally
                 (error 'identifier-space-exhausted
                        :message
                        (format nil
                                "Every identifier seed is occupied for universal time ~D."
                                timestamp)
                        :value timestamp
                        :timestamp timestamp))))))

(defun identifier--first-seed ()
  "Return the validated first probe index from *RANDOM-INDEX-FUNCTION*."
  (let ((seed (funcall *random-index-function* (identifier-base))))
    (unless (and (integerp seed)
                 (<= 0 seed)
                 (< seed (identifier-base)))
      (identifier--fail
       "The identifier entropy source returned an invalid seed index."
       seed))
    seed))

(defun identifier--canonical-members (values)
  "Return the canonical identifiers among VALUES, ignoring every other value.

A value that is not an identifier cannot collide with a generated one, so it is
dropped rather than reported as an error."
  (loop for value in values
        for canonical = (handler-case (identifier-normalize value)
                          (identifier-error ()
                            nil))
        when canonical
          collect canonical))
