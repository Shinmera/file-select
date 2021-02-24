#|
 This file is a part of file-select
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-select.zenity)

(defclass zenity (backend)
  ())

(defmethod finalize ((backend zenity)))

(defun zenity (&rest args)
  (uiop:run-program (list* "zenity" (remove NIL args))
                    :output :string :external-format :utf-8))

(defun zenity* (&key title default filter multiple save backend)
  (declare (ignore backend))
  (handler-case
      (let ((parts (org.shirakumo.file-select::split
                    #\Linefeed
                    (apply #'zenity
                           "--file-selection"
                           "--separator=
"
                           (format NIL "--title=~a" title)
                           (when save "--save")
                           (when multiple "--multiple")
                           (when (eq filter :directory) "--directory")
                           (when default (format NIL "--filename=~a" (file-namestring default)))
                           (loop for (name type) in (etypecase filter
                                                      ((eql :directory))
                                                      (string `(("" ,filter)))
                                                      (list filter))
                                 collect (format NIL "--file-filter=~a | *.~a" name type))))))
        (cond ((null parts) (values NIL NIL))
              (multiple (values parts T))
              (T (values (first parts) T))))
    (error ()
      (values NIL NIL))))

(defmethod new-with ((backend zenity) &rest args)
  (apply #'zenity* :save T args))

(defmethod existing-with ((backend zenity) &rest args)
  (apply #'zenity* args))
