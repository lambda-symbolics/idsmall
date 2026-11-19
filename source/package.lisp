(defpackage #:idsmall
  (:documentation "Short Base58 easy to type IDs based on encoded scrambled timestamps.")
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:export #:*random-index-function*
           #:identifier-base
           #:identifier-clear-reservations
           #:identifier-display
           #:identifier-error
           #:identifier-error-message
           #:identifier-error-value
           #:identifier-from-seed
           #:identifier-generate
           #:identifier-length
           #:identifier-match
           #:identifier-normalize
           #:identifier-p
           #:identifier-release
           #:identifier-reserved-p
           #:identifier-seed-index
           #:identifier-space-exhausted
           #:identifier-space-exhausted-timestamp
           #:identifier-timestamp))

(defpackage #:idsmall/tests
  (:documentation "Regression tests for idsmall.")
  (:use #:cl)
  (:import-from #:idsmall
                #:*random-index-function*
                #:identifier-base
                #:identifier-clear-reservations
                #:identifier-display
                #:identifier-error
                #:identifier-from-seed
                #:identifier-generate
                #:identifier-length
                #:identifier-match
                #:identifier-normalize
                #:identifier-p
                #:identifier-release
                #:identifier-reserved-p
                #:identifier-seed-index
                #:identifier-space-exhausted
                #:identifier-space-exhausted-timestamp
                #:identifier-timestamp)
  (:export #:run-tests))
