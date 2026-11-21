(in-package #:idsmall)

;;;; -- Conditions --

(define-condition identifier-error (error)
  ((message
    :initarg       :message
    :reader        identifier-error-message
    :type          string
    :documentation "The human-readable description of the failure.")
   (value
    :initarg       :value
    :initform      nil
    :reader        identifier-error-value
    :type          t
    :documentation "The value that could not be parsed, encoded, or decoded."))
  (:report
   (lambda (condition stream)
     (write-string (identifier-error-message condition) stream)))
  (:documentation "An identifier is malformed, or a value cannot be encoded."))

(define-condition identifier-space-exhausted (identifier-error)
  ((timestamp
    :initarg       :timestamp
    :reader        identifier-space-exhausted-timestamp
    :type          unsigned-byte
    :documentation "The universal time for which every seed was occupied."))
  (:documentation
   "Every seed is occupied for one second, so no identifier remains free."))

(define-condition identifier-ambiguity (error)
  ((reference
    :initarg       :reference
    :reader        identifier-ambiguity-reference
    :type          string
    :documentation "The prefix that matches several identifiers.")
   (identifiers
    :initarg       :identifiers
    :reader        identifier-ambiguity-identifiers
    :type          list
    :documentation "The matching identifiers in their original order."))
  (:report (lambda (condition stream)
             (format stream "Reference ~S matches several identifiers: ~{~S~^, ~}."
                     (identifier-ambiguity-reference condition)
                     (identifier-ambiguity-identifiers condition))))
  (:documentation "A nonempty prefix matches several identifiers without an exact match."))

(defun identifier--fail (message value)
  "Signal an IDENTIFIER-ERROR reporting MESSAGE and carrying VALUE."
  (error 'identifier-error :message message :value value))
