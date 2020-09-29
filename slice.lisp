(in-package :tilde-slash)

(defun slice (string start &optional end (sharedp *sharedp*))
  "Like SUBSEQ but with displaced strings and smarter indices.

START is the inclusive start index. END is the exclusive end
index, possibly NIL (i.e. end of string).

If SHAREDP is true, the result may share storage with, or be
identical to, the input STRING; otherwise the result is a
fresh string.
 "
  (check-type start integer)
  (with-indices (start end) string
    (let ((size (- end start)))
      (cond
        ;; must return fresh string
        ((not sharedp)
         (if (plusp size)
             (subseq string start end)
             (copy-seq "")))
        ;; return input if the slice covers the whole string
        ((and (= start 0) (= end (length string)))
         string)
        (t
         (make-array (max size 0)
                     :element-type (array-element-type string)
                     :displaced-to string
                     :displaced-index-offset start))))))
