#|
This file is backend for the file-select package, adapted from zenity.lisp
(c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Original Author: Nicolas Hafner <shinmera@tymoon.eu>
File Author: Andrew Valentine
|#

;;;; This backend is not included in the default backends that are selected from.
;;;; It was originally written because Zenity had troubles propogating file filters,
;;;; however this is no longer an issue.

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
			   (if (eq filter :directory) "--getexistingdirectory"
			       (if save "--getsavefilename"
				   "--getopenfilename"))
			   (format nil "~a" (if default (namestring default) "./"))
			   (format NIL "--title=~a" title)
                           (when multiple "--multiple")
			   (etypecase filter
			     ((eql :directory))
			     (string (format nil "~a (*.~a)" filter filter))
			     (list (format nil "~{~{~a (*.~a)~}~^|~}" filter)))
			   nil
                           ))))
        (cond ((null parts) (values NIL NIL))
              (multiple (values parts T))
              (T (values (first parts) T))))
    (error ()
      (values NIL NIL))
    ))

(defmethod new-with ((backend kdialog) &rest args)
  (apply #'kdialog* :save T args))

(defmethod existing-with ((backend kdialog) &rest args)
  (apply #'kdialog* args))
