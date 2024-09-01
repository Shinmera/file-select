(in-package #:org.shirakumo.file-select.yad)

(defclass yad (backend)
  ((program-name :initform "yad" :initarg :program-name :accessor program-name)))

(defun yad (&key title default filter multiple save (program "yad"))
  (handler-case
      (let ((parts (org.shirakumo.file-select::split
                    #\Linefeed
                    (org.shirakumo.file-select::run
                     program
                     "--file" ; Identical to zenity, they just decided to call it --file for whatever reason.
                     "--separator=
"
                     (format NIL "--title=~a" title)
                     (when save "--save")
                     (when multiple "--multiple")
                     (when (eq filter :directory) "--directory")
                     (when default (format NIL "--filename=~a" (uiop:native-namestring default)))
                     (loop for (name . type) in (etypecase filter
                                                  ((eql :directory))
                                                  (string `(("" ,filter)))
                                                  (list filter))
                           collect (format NIL "--file-filter=~a |~{ *.~a~}" name type))))))
        (cond ((null parts) (values NIL NIL))
              (multiple (values parts T))
              (T (values (first parts) T))))
    (error ()
      (values NIL NIL))))

(defmethod new-with ((backend yad) &rest args)
  (apply #'yad :program (program-name backend) :save T args))

(defmethod existing-with ((backend yad) &rest args)
  (apply #'yad :program (program-name backend) args))
