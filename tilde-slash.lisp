(defpackage #:tilde-slash
  (:nicknames ts)
  (:use :cl)
  (:import-from #:str
                str:with-indices)
  (:import-from #:alexandria
                alexandria:once-only
                alexandria:ensure-list)
  (:export #:sub
           #:fun
           #:rec
           #:pos
           #:var
           #:fit
           #:rep

           #:*colonp*
           #:*atsignp*))

(in-package #:tilde-slash)

(defvar *colonp*)
(defvar *atsignp*)

(defvar *ellipsis* "…")

(defgeneric store-position (item position)
  (:method ((v vector) p)
    (vector-push-extend p v (length v)))
  (:method ((f function) p)
    (funcall f p)))

(defun rep (stream count *colonp* *atsignp* &optional (data #\space))
  (loop repeat count do (princ data stream)))

(defun pos (stream target *colonp* *atsignp*)
  (store-position target (file-position stream)))

(defmacro var (s &aux (p (gensym)))
  `(lambda (,p) (setf ,s ,p)))

(defun fun (*standard-output* data *colonp* *atsignp* function &rest args)
  "Format DATA using FUNCTION

Apply FUNCTION with DATA and ARGS in a dynamic context where *STANDARD-OUTPUT*
is bound to the format destination stream, and where *COLONP* and *ATSIGNP*
are bound to T or NIL according to the modifiers given in the control string.

For example:

  (use-package :local-time)

  (format nil
          \"~v@/ts:fun/\"
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

(defun sub (stream string *colonp* *atsignp* &optional start end)
  (with-indices ((start (or start 0)) end) string
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

(defun chain (stream data *colonp* *atsignp* formats)
  (flet ((process (format value) (format nil format value)))
    (write-string (reduce #'process formats :from-end t :initial-value data)
                  stream)))

;; (string= (format nil
;;                  "~v/ts:chain/"
;;                  '("~20a" "~@:(~r~)")
;;                  35)
;;          "THIRTY-FIVE         ")

;; fixme: just compute the indices and write the substrings
;; fixme: do not pad here, just truncate

(defvar *default-cut* :end)

(defun fit (stream object *colonp* *atsignp*
            &optional
              total-size
              padding-pos
              cut-at
              padding-char
              ellipsis)
  "FORMAT control format for FIT-STRING function."
  (assert total-size (total-size) "Size argument is mandatory")
  (let* ((string (if *colonp*
                     (with-output-to-string (out)
                       (write object :stream out :escape t :readably t))
                     object))
         (ellipsis (string (or ellipsis *ellipsis*)))
         (cut-at (etypecase cut-at
                   (symbol (or cut-at *default-cut*))
                   ((real 0 1) cut-at)
                   ((integer 0 100) (/ cut-at 100))))
         (padding-char (or padding-char #\space))
         (ssize (length string))
         (esize (length ellipsis))
         (extra (- total-size ssize))
         (ratio (case cut-at
                  (:end 1)
                  (:start 0)
                  (:middle 1/2)
                  (t cut-at))))
    (if (<= 0 extra)
        (multiple-value-bind (left right) (case padding-pos
                                            (:left (values extra 0))
                                            (:right (values 0 extra))
                                            (t (values 0 0)))
          (format stream
                  "~v/ts:rep/~a~v/ts:rep/"
                   padding-char left
                   string
                   padding-char right))
        (let* ((keep (max 0 (- total-size esize)))
               (cut (- ssize keep))
               (sep (round (* keep ratio))))
          (format stream
                  "~v,v/ts:sub/~a~v/ts:sub/"
                  0 sep string
                  ellipsis
                  (+ sep cut) string)))))


;; (format nil
;;         "~@{| ~v,v@/ts:fit/ ~}|"
;;         10 :left "azldkmazkdazlkdamzlkd"
;;         10 :left "azldkmazkda"
;;         10 :right "zlkdamzlkd"
;;         25 :left  "alkdz"
;;         20 :right "alkmazkdkdamzlkd")
;;
;; "| azldkmazk… | azldkmazk… | zlkdamzlkd |                     alkdz | alkmazkdkdamzlkd     |"
