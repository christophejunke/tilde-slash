(defpackage #:tilde-slash
  (:nicknames ts)
  (:use :cl)
  (:import-from #:alexandria
                alexandria:once-only
                alexandria:ensure-list)
  (:export #:fmt
           #:sub
           #:compose

           #:slice
           #:index
           #:with-indices
           #:*colonp*
           #:*atsignp*))

(in-package #:tilde-slash)

(defvar *colonp*)
(defvar *atsignp*)

(defvar *sharedp* t)

(defun fn (*standard-output* data *colonp* *atsignp* function &rest args)
  "Format DATA using FUNCTION

Apply FUNCTION with DATA and ARGS in a dynamic context where *STANDARD-OUTPUT*
is bound to the format destination stream, and where *COLONP* and *ATSIGNP*
are bound to T or NIL according to the modifiers given in the control string.

For example:

  (use-package :local-time)

  (format nil
          \"~v@/ts:fn/\"
          (lambda (date)
            (let ((format (cond
                            ((and *atsignp* *colonp*) +iso-week-date-format+)
                            (*atsignp* +iso-8601-date-format+)
                            (*colonp* +iso-8601-time-format+)
                            (t +iso-8601-format+))))
              (format-timestring t date :format format)))
          (now))
"
  (apply function data args))

(declaim (inline index))

(defun index (string pos)
  "Modular/saturating position in string.

Returns an index between 0 and (LENGTH STRING), inclusive.

If POS is NIL, the returned value is the length of STRING,
to be consistent with exclusive end indices.

Otherwise POS must be an integer.

If POS is positive, it is clamped to be at most the length
of the string.

If POS is negative and its magnitude is lower than the
length of string, the returned index is meant to represent
an offset from the end.

Otherwise, if POS is negative and its magnitude is greater
than the length, the returned index is 0.

EXAMPLES:

   (flet ((is (pos res) (assert (= (index \"abcde\" pos) res))))
     ;; nil case
     (is nil  5)

     ;; positive
     (is 3  3)
     (is 4  4)
     (is 5  5)
     (is 10 5)

     ;; negative
     (is -1   4)
     (is -2   3)
     (is -3   2)
     (is -4   1)
     (is -5   0)
     (is -10  0))
  "
  (check-type pos (or null integer))
  (with-accessors ((length length)) string
    (cond
      ((not pos) length)
      ((>= pos 0) (min pos length))
      ((< (abs pos) length) (+ length pos))
      (t 0))))

(defmacro with-indices ((&rest indices) string &body body)
  (once-only (string)
    (flet ((binding (index)
             (destructuring-bind (name &optional default) (ensure-list index)
               (let ((index-expr (if default `(or ,name ,default) name)))
                 `(,name (index ,string ,index-expr))))))
      `(let ,(mapcar #'binding indices)
         ,@body))))

(defun sub (stream string *colonp* *atsignp* &optional start end)
  (with-indices ((start 0) end) string
    (when (< start end)
      (write-string string stream :start start :end end))))

;; (let ((in "abcdefghijklmnopqrstuvwxyz"))
;;   (format nil
;;           "%~13<~v,v/ts:sub/...~v,v/ts:sub/~>%"
;;           0 5 in
;;           10 15 in))
;;
;; (equal (format nil "~2/ts:sub/" "abcdef") "cdef")
;; (equal (format nil "~-2/ts:sub/" "abcdef") "ef")
;; (equal (format nil "~,-1/ts:sub/" "abcdefgh") "abcdefg")
;;

;; note: see FORMATTER and functions as format-control
;; also, (format nil "~?" (formatter "~R") '(5))

(defun compose (stream data *colonp* *atsignp* formats)
  (flet ((process (format value) (format nil format value)))
    (write-string (reduce #'process formats :from-end t :initial-value data)
                  stream)))

;; (string= (format nil
;;                  "~v/ts:compose/"
;;                  '("~20a" "~@:(~r~)")
;;                  35)
;;          "THIRTY-FIVE         ")

;; fixme: just compute the indices and write the substrings
;; fixme: do not pad here, just truncate

(defun fit-string (string total-size &key (cut-at :end) (padding :right))
  "Pad or truncate STRING to TOTAL-SIZE."
  (check-type cut-at (or (real 0.0 1.0) (member :start :middle :end)))
  (check-type padding (or null (member :left :right)))
  (let* ((ellipsis "..." ;; str::*ellipsis*
                   )
         (ssize (length string))
         (esize (length ellipsis))
         (ratio (case cut-at
                  (:end 1)
                  (:start 0)
                  (:middle 1/2)
                  (t cut-at))))
    (format nil
            "~[~*~a~;~v@a~;~va~]"
            (position padding '(nil :left :right))
            total-size
            (if (<= ssize total-size)
                string
                (let* ((keep (max 0 (- total-size esize)))
                       (cut (- ssize keep))
                       (sep (round (* keep ratio))))
                  (let ((*sharedp* t))
                    (format nil
                            "~a~a~a"
                            (slice string 0 sep)
                            ellipsis
                            (slice string (+ sep cut)))))))))

(defun fit (stream string colonp atsignp &optional total-size (cut-point :end))
  "FORMAT control format for FIT-STRING function."
  (assert total-size (total-size) "Size argument is mandatory")
  (princ (fit-string string
                     total-size
                     :cut-at (etypecase cut-point
                               (symbol cut-point)
                               ((real 0 1) cut-point)
                               ((integer 0 100) (/ cut-point 100)))
                     :padding (unless colonp (if atsignp :left :right)))
         stream))


