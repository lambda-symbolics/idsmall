(in-package #:idsmall/tests)

(defparameter *timestamps*
  '(0 1 3661 2208988800 3993000000 4294967295)
  "Universal times spanning the epoch, ordinary dates, and the modulus edge.")

(defparameter *malformed-identifiers*
  '(nil
    42
    ""
    "131Xcb"
    "131Xcbii"
    "131Xcb0"
    "131XcbO"
    "131XcbI"
    "131Xcbl"
    "1-31Xcb"
    "13-1Xcbi"
    "1-31Xcb0")
  "Values that are neither canonical nor display identifiers.")

(defun tests--encoding ()
  "Exercise fixed-width Base58 encoding across seeds and timestamps."
  (dolist (timestamp *timestamps*)
    (let ((identifiers
            (loop for seed-index below (identifier-base)
                  collect (identifier-from-seed timestamp seed-index))))
      (test-assert (every #'identifier-p identifiers)
                   "every seed encodes one canonical identifier")
      (test-assert (every (lambda (identifier)
                            (= (length identifier) (identifier-length)))
                          identifiers)
                   "every identifier has the canonical width")
      (test-assert (= (length (remove-duplicates identifiers :test #'string=))
                      (identifier-base))
                   "seeds produce distinct identifiers for one timestamp")))
  (test-assert (string= (identifier-from-seed 1000 5)
                        (identifier-from-seed (+ 1000 #x100000000) 5))
               "timestamps reduce modulo two to the thirty-second")
  (test-assert (not (string= (identifier-from-seed 1000 5)
                             (identifier-from-seed 1001 5)))
               "adjacent seconds encode distinct identifiers for one seed")
  (test-assert (signals identifier-error (identifier-from-seed 1000 -1))
               "a negative seed index is rejected")
  (test-assert (signals identifier-error
                 (identifier-from-seed 1000 (identifier-base)))
               "a seed index at the radix is rejected")
  (test-assert (signals identifier-error (identifier-from-seed -1 0))
               "a negative timestamp is rejected")
  nil)

(defun tests--forms ()
  "Exercise the canonical predicate, normalization, and the display form."
  (let* ((canonical (identifier-from-seed 3993000000 7))
         (displayed (identifier-display canonical)))
    (test-assert (identifier-p canonical)
                 "a generated identifier is canonical")
    (test-assert (= (length displayed) (1+ (identifier-length)))
                 "the display form adds one character")
    (test-assert (char= (char displayed 1) #\-)
                 "the display form hyphenates after the seed")
    (test-assert (string= (subseq displayed 0 1) (subseq canonical 0 1))
                 "the display form preserves the seed character")
    (test-assert (string= (identifier-normalize displayed) canonical)
                 "normalization recovers the canonical form")
    (test-assert (string= (identifier-normalize canonical) canonical)
                 "normalization accepts an already canonical identifier")
    (test-assert (string= (identifier-display displayed) displayed)
                 "the display form is idempotent"))
  (dolist (value *malformed-identifiers*)
    (test-assert (not (identifier-p value))
                 (format nil "~S is not canonical" value))
    (test-assert (signals identifier-error (identifier-normalize value))
                 (format nil "normalizing ~S signals" value))
    (test-assert (signals identifier-error (identifier-display value))
                 (format nil "displaying ~S signals" value)))
  nil)

(defun run-tests ()
  "Run every idsmall regression test."
  (setf *test-count* 0)
  (tests--encoding)
  (tests--forms)
  (format t "~&~:D idsmall tests passed.~%" *test-count*)
  nil)
