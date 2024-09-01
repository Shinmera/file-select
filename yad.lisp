(in-package #:org.shirakumo.file-select.yad)

(defclass yad (backend)
  ((program-name :initform "yad" :initarg :program-name :accessor program-name)))

(defmethod finalize ((backend yad)))

(defun yad (program &rest args)
  (uiop:run-program (list* program (remove NIL args))
                    :output :string :external-format :utf-8))

(defun yad* (&key title default filter multiple save backend)
  (handler-case
      (let ((parts (org.shirakumo.file-select::split
                    #\Linefeed
                    (apply #'yad
                           (program-name backend)
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
  (apply #'yad* :backend backend :save T args))

(defmethod existing-with ((backend yad) &rest args)
  (apply #'yad* :backend backend args))
