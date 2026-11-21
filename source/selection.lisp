(in-package #:idsmall)

;;;; -- Exact and Prefix Selection --

(defun identifier-select (reference entities &key (key #'identity))
  "Return the exact match for REFERENCE, otherwise its unique nonempty prefix match.

KEY maps each entity to an arbitrary string identifier. Comparisons are
case-sensitive, without identifier normalization. Return NIL when no entity
matches. Signal IDENTIFIER-AMBIGUITY when several prefix matches exist; its
identifiers are in the order of ENTITIES. An exact match takes priority."
  (let ((exact (member reference entities :key key :test #'string=)))
    (if exact
        (first exact)
        (when (plusp (length reference))
          (let ((matches (remove-if-not
                          (lambda (entity)
                            (let ((identifier (funcall key entity)))
                              (and (<= (length reference) (length identifier))
                                   (string= reference identifier :end2 (length reference)))))
                          entities)))
            (when (rest matches)
              (error 'identifier-ambiguity :reference reference
                     :identifiers (mapcar key matches)))
            (first matches))))))

(defun identifier-reference (identifier identifiers &key (minimum-length 7))
  "Return the shortest prefix that selects IDENTIFIER from IDENTIFIERS.

Compare arbitrary strings case-sensitively, with exact matches taking
priority. Start at MINIMUM-LENGTH, or the full length of a shorter identifier.
Return IDENTIFIER when no shorter prefix selects it."
  (check-type minimum-length (integer 1 *))
  (loop for end from (min minimum-length (length identifier)) to (length identifier)
        for prefix = (subseq identifier 0 end)
        when (handler-case
                 (equal identifier (identifier-select prefix identifiers))
               (identifier-ambiguity () nil))
          return prefix
        finally (return identifier)))
