(in-package #:idsmall)

;;;; -- Base58 Alphabet --

(defparameter *alphabet*
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  "The Bitcoin Base58 alphabet, which omits the visually ambiguous glyphs.")

(defparameter *suffix-length* 6
  "The fixed Base58 width of the scrambled timestamp suffix.")

(defun identifier-base ()
  "Return the radix derived from the Base58 alphabet."
  (length *alphabet*))

(defun identifier-length ()
  "Return the character width of a canonical identifier."
  (1+ *suffix-length*))

(defun identifier--modulus ()
  "Return the modulus the scramble operates in, which is two to the thirty-second."
  #x100000000)

(defun identifier--mask ()
  "Return the mask that reduces an intermediate value to unsigned 32 bits."
  #xffffffff)

(defun identifier--alphabet-index (character)
  "Return CHARACTER's zero-based Base58 index, or NIL when it is not Base58."
  (position character *alphabet* :test #'char=))


;;;; -- Canonical and Display Forms --

(defun identifier-p (value)
  "Return true when VALUE is one canonical identifier."
  (not (null
        (and (stringp value)
             (= (length value) (identifier-length))
             (every #'identifier--alphabet-index value)))))

(defun identifier-normalize (value)
  "Return VALUE as a canonical identifier, also accepting its display form.

Signal IDENTIFIER-ERROR when VALUE is neither form."
  (let ((normalized
          (cond
            ((identifier-p value)
             value)
            ((identifier--display-shaped-p value)
             (concatenate 'string (subseq value 0 1) (subseq value 2)))
            (t
             nil))))
    (unless (identifier-p normalized)
      (identifier--fail
       (format nil
               "An identifier must contain ~D case-sensitive Bitcoin Base58 characters, with an optional hyphen after the first."
               (identifier-length))
       value))
    normalized))

(defun identifier-display (identifier)
  "Return IDENTIFIER in its display form, hyphenated after the seed character.

Signal IDENTIFIER-ERROR when IDENTIFIER is neither canonical nor display form.
Callers holding foreign or legacy values should guard this call rather than
expect the value back unchanged."
  (let ((canonical (identifier-normalize identifier)))
    (format nil "~A-~A" (subseq canonical 0 1) (subseq canonical 1))))

(defun identifier--display-shaped-p (value)
  "Return true when VALUE has the length and hyphen of a display identifier.

The remaining characters are validated by IDENTIFIER-P after the hyphen is
removed, so this predicate only recognizes the shape."
  (not (null
        (and (stringp value)
             (= (length value) (1+ (identifier-length)))
             (char= (char value 1) #\-)))))


;;;; -- Scrambling --

(defun identifier--mix32 (value)
  "Return the unsigned 32-bit mix of VALUE.

The operation is the MurmurHash3 32-bit finalizer. Every intermediate product
is reduced modulo two to the thirty-second, so the result never depends on the
host fixnum width."
  (let ((mixed (logand value (identifier--mask))))
    (setf mixed (logand (logxor mixed (ash mixed -16)) (identifier--mask))
          mixed (logand (* mixed #x85ebca6b)           (identifier--mask))
          mixed (logand (logxor mixed (ash mixed -13)) (identifier--mask))
          mixed (logand (* mixed #xc2b2ae35)           (identifier--mask))
          mixed (logand (logxor mixed (ash mixed -16)) (identifier--mask)))
    mixed))

(defun identifier--seed-parameters (seed-index)
  "Return the odd multiplier and the offset derived from SEED-INDEX.

Two independent hexadecimal domain constants and IDENTIFIER--MIX32 fully
specify a portable derivation. The multiplier is forced odd, which makes the
scramble a bijection modulo two to the thirty-second and therefore reversible."
  (identifier--check-seed-index seed-index)
  (values
   (logior 1 (identifier--mix32 (logxor seed-index #x73656564)))
   (identifier--mix32 (logxor seed-index #x6f666673))))

(defun identifier--check-seed-index (seed-index)
  "Require SEED-INDEX to be a Base58 index, signaling IDENTIFIER-ERROR otherwise."
  (unless (and (integerp seed-index)
               (<= 0 seed-index)
               (< seed-index (identifier-base)))
    (identifier--fail
     (format nil "An identifier seed index must be from 0 through ~D."
             (1- (identifier-base)))
     seed-index))
  t)


;;;; -- Encoding --

(defun identifier-from-seed (timestamp seed-index)
  "Return the canonical identifier for universal TIMESTAMP and SEED-INDEX.

TIMESTAMP is reduced modulo two to the thirty-second before it is scrambled."
  (unless (and (integerp timestamp) (<= 0 timestamp))
    (identifier--fail
     "An identifier timestamp must be a non-negative universal time."
     timestamp))
  (multiple-value-bind (multiplier offset)
      (identifier--seed-parameters seed-index)
    (let* ((seconds   (mod timestamp (identifier--modulus)))
           (scrambled (mod (+ (* multiplier seconds) offset)
                           (identifier--modulus))))
      (concatenate 'string
                   (string (char *alphabet* seed-index))
                   (identifier--encode-suffix scrambled)))))

(defun identifier--encode-suffix (value)
  "Encode unsigned 32-bit VALUE as exactly *SUFFIX-LENGTH* Base58 characters.

Six Base58 characters span more than two to the thirty-second values, so every
unsigned 32-bit input has one fixed-width encoding."
  (unless (and (integerp value)
               (<= 0 value (identifier--mask)))
    (identifier--fail
     "An identifier suffix value must be an unsigned 32-bit integer."
     value))
  (let ((encoded   (make-string *suffix-length*
                                :initial-element (char *alphabet* 0)))
        (remaining value))
    (loop for position downfrom (1- *suffix-length*) to 0
          do (multiple-value-bind (quotient remainder)
                 (floor remaining (identifier-base))
               (setf (char encoded position) (char *alphabet* remainder)
                     remaining               quotient)))
    encoded))


;;;; -- Decoding --

(defun identifier-seed-index (identifier)
  "Return the Base58 seed index that IDENTIFIER carries in its first character."
  (identifier--alphabet-index (char (identifier-normalize identifier) 0)))

(defun identifier-timestamp (identifier)
  "Return the universal time that IDENTIFIER encodes.

The scramble is a bijection, so the recovered value is exact for every
timestamp below two to the thirty-second seconds, which covers universal times
before the year 2036. Later timestamps are recovered modulo that bound.

Signal IDENTIFIER-ERROR when IDENTIFIER is malformed, or when its suffix does
not encode an unsigned 32-bit value and therefore was not produced here."
  (let* ((canonical  (identifier-normalize identifier))
         (seed-index (identifier--alphabet-index (char canonical 0)))
         (scrambled  (identifier--decode-suffix (subseq canonical 1))))
    (multiple-value-bind (multiplier offset)
        (identifier--seed-parameters seed-index)
      (mod (* (identifier--invert32 multiplier) (- scrambled offset))
           (identifier--modulus)))))

(defun identifier--decode-suffix (suffix)
  "Return the unsigned 32-bit value that Base58 SUFFIX encodes.

Six Base58 characters span more values than two to the thirty-second, so a
suffix above that bound is rejected rather than silently reduced."
  (let ((value 0))
    (loop for character across suffix
          do (setf value (+ (* value (identifier-base))
                            (identifier--alphabet-index character))))
    (unless (<= value (identifier--mask))
      (identifier--fail
       "An identifier suffix does not encode an unsigned 32-bit value."
       suffix))
    value))

(defun identifier--invert32 (multiplier)
  "Return the modular inverse of odd MULTIPLIER modulo two to the thirty-second.

Newton's iteration doubles the number of correct low bits at each step, so five
steps from an initial estimate of one reach the full 32 bits."
  (let ((inverse 1))
    (dotimes (step 5)
      (declare (ignore step))
      (setf inverse (logand (* inverse (- 2 (* multiplier inverse)))
                            (identifier--mask))))
    (unless (= 1 (logand (* multiplier inverse) (identifier--mask)))
      (identifier--fail
       "An identifier multiplier is not invertible modulo two to the thirty-second."
       multiplier))
    inverse))
