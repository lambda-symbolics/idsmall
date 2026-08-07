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

(defun tests--decoding ()
  "Exercise seed and timestamp recovery from encoded identifiers."
  (dolist (timestamp *timestamps*)
    (loop for seed-index below (identifier-base)
          for identifier = (identifier-from-seed timestamp seed-index)
          do (test-assert (= (identifier-seed-index identifier) seed-index)
                          "an identifier reports the seed that encoded it")
             (test-assert (= (identifier-timestamp identifier) timestamp)
                          "an identifier decodes to the timestamp that encoded it")))
  (test-assert (= (identifier-timestamp
                   (identifier-display (identifier-from-seed 3993000000 3)))
                  3993000000)
               "decoding accepts the display form")
  (test-assert (= (identifier-timestamp (identifier-from-seed #x100000001 0))
                  1)
               "decoding recovers a wrapped timestamp modulo the bound")
  (dolist (value *malformed-identifiers*)
    (test-assert (signals identifier-error (identifier-timestamp value))
                 (format nil "decoding ~S signals" value))
    (test-assert (signals identifier-error (identifier-seed-index value))
                 (format nil "reading the seed of ~S signals" value)))
  (test-assert (signals identifier-error (identifier-timestamp "zzzzzzz"))
               "a suffix above the unsigned 32-bit bound is rejected")
  nil)

(defun tests--allocation ()
  "Exercise seeded probing, the occupancy callback, and exhaustion."
  (let ((timestamp 3993000000))
    (let ((*random-index-function* (constantly 0)))
      (test-assert (string= (identifier-generate :timestamp timestamp)
                            (identifier-from-seed timestamp 0))
                   "allocation starts from the seed the entropy source returns")
      (identifier-clear-reservations)
      (test-assert
       (string= (identifier-generate
                 :timestamp timestamp
                 :reserved-identifiers (list (identifier-from-seed timestamp 0)))
                (identifier-from-seed timestamp 1))
       "a reserved identifier is skipped")
      (identifier-clear-reservations)
      (test-assert
       (string= (identifier-generate
                 :timestamp timestamp
                 :reserved-identifiers
                 (list (identifier-display (identifier-from-seed timestamp 0))))
                (identifier-from-seed timestamp 1))
       "a display form reserves its canonical identifier")
      (identifier-clear-reservations)
      (test-assert (string= (identifier-generate
                             :timestamp timestamp
                             :reserved-identifiers (list "nonsense" nil 42))
                            (identifier-from-seed timestamp 0))
                   "values that are not identifiers cannot reserve a seed")
      (identifier-clear-reservations)
      (test-assert
       (string= (identifier-generate
                 :timestamp timestamp
                 :occupied-p (lambda (candidate)
                               (string= candidate
                                        (identifier-from-seed timestamp 0))))
                (identifier-from-seed timestamp 1))
       "the occupancy callback excludes a candidate")
      (identifier-clear-reservations))
    (let ((*random-index-function* (constantly (1- (identifier-base)))))
      (test-assert
       (string= (identifier-generate
                 :timestamp timestamp
                 :reserved-identifiers
                 (list (identifier-from-seed timestamp (1- (identifier-base)))))
                (identifier-from-seed timestamp 0))
       "probing wraps from the highest seed to the lowest")
      (identifier-clear-reservations))
    (let ((*random-index-function* (constantly (identifier-base))))
      (test-assert (signals identifier-error
                     (identifier-generate :timestamp timestamp))
                   "an out-of-range entropy result is rejected"))
    (let ((identifiers (loop repeat (identifier-base)
                             collect (identifier-generate :timestamp timestamp))))
      (test-assert (= (length (remove-duplicates identifiers :test #'string=))
                      (identifier-base))
                   "one second allocates every seed exactly once")
      (test-assert (handler-case
                       (progn (identifier-generate :timestamp timestamp) nil)
                     (identifier-space-exhausted (condition)
                       (= (identifier-space-exhausted-timestamp condition)
                          timestamp)))
                   "exhausting a second reports the timestamp")
      (test-assert (every #'identifier-reserved-p identifiers)
                   "an allocated identifier is reserved")
      (test-assert (every #'identifier-reserved-p
                          (mapcar #'identifier-display identifiers))
                   "a reservation is recognized through the display form")
      (test-assert (every #'identifier-release identifiers)
                   "releasing a reservation reports that one existed")
      (test-assert (notany #'identifier-release identifiers)
                   "releasing twice reports that none remained")))
  (let ((identifier (identifier-generate :namespace :alpha)))
    (test-assert (identifier-reserved-p identifier :namespace :alpha)
                 "an identifier is reserved within its own namespace")
    (test-assert (not (identifier-reserved-p identifier :namespace :beta))
                 "reservations do not leak between namespaces")
    (identifier-clear-reservations))
  (let* ((before     (get-universal-time))
         (identifier (identifier-generate))
         (after      (get-universal-time)))
    (test-assert (<= before (identifier-timestamp identifier) after)
                 "allocation defaults to the current universal time"))
  (identifier-clear-reservations)
  nil)

(defun run-tests ()
  "Run every idsmall regression test."
  (setf *test-count* 0)
  (tests--encoding)
  (tests--forms)
  (tests--decoding)
  (tests--allocation)
  (format t "~&~:D idsmall tests passed.~%" *test-count*)
  nil)
