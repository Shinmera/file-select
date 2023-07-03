(in-package #:org.shirakumo.file-select.zenity)

(defclass zenity (backend)
  ((program-name :initform "zenity" :initarg :program-name :accessor program-name)))

(defmethod finalize ((backend zenity)))

(defun zenity (program &rest args)
  (uiop:run-program (list* program (remove NIL args))
                    :output :string :external-format :utf-8))

(defun zenity* (&key title default filter multiple save backend)
  (handler-case
      (let ((parts (org.shirakumo.file-select::split
                    #\Linefeed
                    (apply #'zenity
                           (program-name backend)
                           "--file-selection"
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

(defmethod new-with ((backend zenity) &rest args)
  (apply #'zenity* :backend backend :save T args))

(defmethod existing-with ((backend zenity) &rest args)
  (apply #'zenity* :backend backend args))
