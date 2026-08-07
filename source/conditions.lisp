(in-package #:idsmall)

;;;; -- Conditions --

(define-condition identifier-error (error)
  ((message
    :initarg :message
    :reader identifier-error-message
    :type string
    :documentation "The human-readable description of the failure.")
   (value
    :initarg :value
    :initform nil
    :reader identifier-error-value
    :type t
    :documentation "The value that could not be parsed, encoded, or decoded."))
  (:report
   (lambda (condition stream)
     (write-string (identifier-error-message condition) stream)))
  (:documentation "An identifier is malformed, or a value cannot be encoded."))

(define-condition identifier-space-exhausted (identifier-error)
  ((timestamp
    :initarg :timestamp
    :reader identifier-space-exhausted-timestamp
    :type unsigned-byte
    :documentation "The universal time for which every seed was occupied."))
  (:documentation
   "Every seed is occupied for one second, so no identifier remains free."))

(defun identifier--fail (message value)
  "Signal an IDENTIFIER-ERROR reporting MESSAGE and carrying VALUE."
  (error 'identifier-error :message message :value value))
