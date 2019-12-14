#|
 This file is a part of file-select
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-select)

(defvar *backend-cache* (make-hash-table :test 'eq))
(defvar *default-backend*)

(define-condition file-select-error (error)
  ())

(defun existing (&rest args &key title default filter multiple (backend *default-backend*))
  (apply #'existing-with backend args))

(defun new (&rest args &key title default filter multiple (backend *default-backend*))
  (apply #'new-with backend args))

(defclass backend () ())

(defun get-backend (class)
  (or (gethash class *backend-cache*)
      (setf (gethash class *backend-cache*)
            (make-instance class))))

(defgeneric finalize (backend))
(defgeneric new-with (backend &key title default filter multiple &allow-other-keys))
(defgeneric existing-with (backend &key title default filter multiple &allow-other-keys))

(defmethod new-with ((backend symbol) &rest args)
  (apply #'new-with (find-class backend) args))

(defmethod existing-with ((backend symbol) &rest args)
  (apply #'existing-with (find-class backend) args))

(defmethod new-with ((backend class) &rest args &key (title "New File"))
  (apply #'new-with (get-backend backend) :title title args))

(defmethod existing-with ((backend class) &rest args &key (title "Select File"))
  (apply #'existing-with (get-backend backend) :title title args))

(defmethod finalize :after ((backend backend))
  (remhash (class-of backend) *backend-cache*))

(defmethod finalize ((all (eql T)))
  (mapc #'finalize (loop for v being the hash-values of *backend-cache* collect v)))
