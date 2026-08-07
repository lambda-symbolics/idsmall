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
           #:identifier-seed-index
           #:identifier-space-exhausted
           #:identifier-space-exhausted-timestamp
           #:identifier-timestamp))

(defpackage #:idsmall/tests
  (:use #:cl)
  (:import-from #:idsmall
                #:identifier-base
                #:identifier-display
                #:identifier-error
                #:identifier-from-seed
                #:identifier-length
                #:identifier-normalize
                #:identifier-p
                #:identifier-seed-index
                #:identifier-timestamp)
  (:export #:run-tests))
