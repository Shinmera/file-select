(in-package #:org.shirakumo.file-select)

(defvar *backend-cache* (make-hash-table :test 'eq))
(defvar *default-backend*)

(define-condition file-select-error (error)
  ())

(defun default-backend ()
  (if (boundp '*default-backend*)
      *default-backend*
      (setf *default-backend* (determine-default-backend))))

(defun existing (&rest args &key title default filter multiple backend)
  (restart-case (apply #'existing-with (or backend (default-backend)) args)
    (continue ()
      :report "Cancel the call by returning NIL"
      (values NIL NIL))))

(defun new (&rest args &key title default filter multiple backend)
  (restart-case (apply #'new-with (or backend (default-backend)) args)
    (continue ()
      :report "Cancel the call by returning NIL"
      (values NIL NIL))))

(defclass backend () ())

(defun get-backend (class)
  (or (gethash class *backend-cache*)
      (setf (gethash class *backend-cache*)
            (make-instance class))))

(defgeneric finalize (backend))
(defgeneric new-with (backend &key title default filter &allow-other-keys))
(defgeneric existing-with (backend &key title default filter multiple &allow-other-keys))

(defmethod finalize ((backend symbol))
  (finalize (find-class backend)))

(defmethod new-with ((backend symbol) &rest args)
  (apply #'new-with (find-class backend) args))

(defmethod existing-with ((backend symbol) &rest args)
  (apply #'existing-with (find-class backend) args))

(defmethod finalize ((backend class))
  (let ((instance (gethash backend *backend-cache*)))
    (when instance (finalize instance))))

(defmethod new-with ((backend class) &rest args &key (title "New File"))
  (apply #'new-with (get-backend backend) :title title args))

(defmethod existing-with ((backend class) &rest args &key (title "Select File"))
  (apply #'existing-with (get-backend backend) :title title args))

(defmethod finalize ((backend backend)))

(defmethod finalize :after ((backend backend))
  (remhash (class-of backend) *backend-cache*))

(defmethod finalize ((all (eql T)))
  (mapc #'finalize (loop for v being the hash-values of *backend-cache* collect v)))

(defun native-namestring (path)
  #+sbcl (sb-ext:native-namestring path)
  #+ccl (ccl:native-translated-namestring path)
  #+cmucl (ext:unix-namestring path NIL)
  #-(or sbcl ccl cmucl) (namestring path))

(defun parse-native-namestring (path)
  #+sbcl (sb-ext:parse-native-namestring path)
  #+ccl (ccl:native-to-pathname path)
  #-(or sbcl ccl) (parse-namestring path))

(defun run (program &rest args)
  (handler-case
      (multiple-value-bind (output error status)
          (uiop:run-program (list* program (remove NIL args))
                            :output :string
                            :ignore-error-status T
                            :external-format :utf-8)
        (declare (ignore error))
        (values output status))
    (error ()
      (error 'file-select-error))))
