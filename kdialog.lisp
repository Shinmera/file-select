(defpackage #:org.shirakumo.file-select.kdialog
  (:use #:cl #:org.shirakumo.file-select)
  (:export #:kdialog))

(in-package #:org.shirakumo.file-select.kdialog)

(defclass kdialog (backend)
  ())

(defmethod finalize ((backend kdialog)))

(defun kdialog (&rest args)
  (uiop:run-program (list* "kdialog" (remove NIL args))
                    :output :string :external-format :utf-8))

(defun kdialog* (&key title default filter multiple save backend)
  (declare (ignore backend))
  (handler-case
      (let ((parts (org.shirakumo.file-select::split
                    #\Linefeed
                    (apply #'kdialog
			               (cond ((eq filter :directory) "--getexistingdirectory")
                                 (save "--getsavefilename")
			                     (T "--getopenfilename"))
			               (if default (namestring default) "./")
			               (format NIL "--title=~a" title)
                           (when multiple "--multiple")
			               (etypecase filter
			                 ((eql :directory))
			                 (string (format nil "~a (*.~a)" filter filter))
			                 (list (format nil "~{~{~a (*.~a)~}~^|~}" filter)))
			               NIL))))
        (cond ((null parts) (values NIL NIL))
              (multiple (values parts T))
              (T (values (first parts) T))))
    (error ()
      (values NIL NIL))))

(defmethod new-with ((backend kdialog) &rest args)
  (apply #'kdialog* :save T args))

(defmethod existing-with ((backend kdialog) &rest args)
  (apply #'kdialog* args))
