(in-package #:org.shirakumo.file-select.zenity)

(defclass zenity (backend)
  ((program-name :initform "zenity" :initarg :program-name :accessor program-name)))

(defun zenity (&key title default filter multiple save (program "zenity"))
  (handler-case
      (let ((parts (org.shirakumo.file-select::split
                    #\Linefeed
                    (org.shirakumo.file-select::run
                     program
                     "--file-selection"
                     "--separator=
"
                     (format NIL "--title=~a" title)
                     (when save "--save")
                     (when multiple "--multiple")
                     (when (eq filter :directory) "--directory")
                     (when default (format NIL "--filename=~a" (pathname-utils:native-namestring default)))
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
  (apply #'zenity :program (program-name backend) :save T args))

(defmethod existing-with ((backend zenity) &rest args)
  (apply #'zenity :program (program-name backend) args))
