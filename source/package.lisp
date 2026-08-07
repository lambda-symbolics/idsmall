(defpackage #:idsmall
  (:use #:cl)
  (:export #:identifier-base
           #:identifier-display
           #:identifier-error
           #:identifier-error-message
           #:identifier-error-value
           #:identifier-from-seed
           #:identifier-length
           #:identifier-normalize
           #:identifier-p
           #:identifier-space-exhausted
           #:identifier-space-exhausted-timestamp))
