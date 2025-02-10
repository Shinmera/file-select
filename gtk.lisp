(in-package #:org.shirakumo.file-select.gtk)

(cffi:define-foreign-library gtk
  (:linux (:or "libgtk-3.so.0" "libgtk-3.so" "libgtk-4.so.0" "libgtk-4.so"))
  (T (:or (:default "libgtk-3") (:default "libgtk-4"))))

(cffi:define-foreign-library glib
  (:linux (:or "libglib-2.0.so.0" "libglib-2.0.so"))
  (T (:default "libglib-2.0")))

(cffi:define-foreign-library gio
  (:linux (:or "libgio-2.0.so.0" "libgio-2.0.so"))
  (T (:default "libgio-2.0")))

(cffi:define-foreign-library gmodule
  (:linux (:or "libgmodule-2.0.so.0" "libgmodule-2.0.so"))
  (T (:default "libgmodule-2.0")))

(set 'cl-user::*foreign-system-libraries*
     (union (when (boundp 'cl-user::*foreign-system-libraries*)
              (symbol-value 'cl-user::*foreign-system-libraries*))
            '(gtk glib gio gmodule)))

(defclass gtk (backend)
  ((closure :accessor closure)))

(defmethod initialize-instance :after ((backend gtk) &key)
  (unless (cffi:foreign-library-loaded-p 'gtk)
    (cffi:use-foreign-library glib)
    (cffi:use-foreign-library gmodule)
    (cffi:use-foreign-library gio)
    (cffi:use-foreign-library gtk)))

(defmethod finalize ((backend gtk))
  (slot-makunbound backend 'closure))

(defmethod new-with ((backend gtk) &key title default filter multiple &allow-other-keys)
  (show* backend title (if (eq filter :directory)
                  :create-folder
                  :save)
        default filter multiple))

(defmethod existing-with ((backend gtk) &key title default filter multiple &allow-other-keys)
  (show* backend title (if (eq filter :directory)
                  :select-folder
                  :open)
         default filter multiple))

(defun show* (backend &rest args)
  (let ((application (new-application "org.shirakumo.file-select" 0))
        values)
    (unwind-protect
         (float-features:with-float-traps-masked T
           (g-signal-connect application "activate" (cffi:callback activate) (cffi:null-pointer) (cffi:null-pointer) 0)
           (setf (closure backend) (lambda () (setf values (multiple-value-list (apply #'show args)))))
           (g-application-run application 0 (cffi:null-pointer)))
      (g-unref application))
    (values-list values)))

(cffi:defcallback activate :void ((application :pointer) (user-data :pointer))
  (declare (ignore application user-data))
  (with-simple-restart (abort "Abort the file dialog")
    (funcall (closure (org.shirakumo.file-select::get-backend (find-class 'gtk))))))

(defun show (title mode default filter multiple)
  (let ((dialog (new-file-chooser title (cffi:null-pointer) mode
                                  "Cancel" :cancel
                                  "Select" :accept
                                  (cffi:null-pointer))))
    (set-select-multiple dialog multiple)
    (cond ((null default))
          ((or (pathname-name default) (pathname-type default))
           (set-filename dialog (native-namestring default)))
          (T
           (set-current-folder dialog (native-namestring default))))
    (etypecase filter
      (string
       (let ((f (new-filter)))
         (add-pattern f (format NIL "*.~a" filter))
         (add-filter dialog f)))
      (list
       (loop for (name type) in filter
             for f = (new-filter)
             do (set-name f name)
                (add-pattern f (format NIL "*.~a" type))
                (add-filter dialog f)))
      ((eql :directory)))
    (unwind-protect
         (case (dialog-run dialog)
           (:accept
            (values
             (if multiple
                 (let* ((files (get-files dialog))
                        (head files))
                   (unwind-protect
                        (loop with file
                              until (cffi:null-pointer-p head)
                              do (setf file (g-slist-data head))
                              collect (parse-native-namestring (g-get-path file))
                              do (g-unref file)
                                 (setf head (g-slist-next head)))
                     (g-slist-free files)))
                 (parse-native-namestring (get-filename dialog)))
             T))
           (T
            (values NIL NIL)))
      (dostrey dialog))))

(cffi:defcenum file-chooser-action
  :open
  :save
  :select-folder
  :create-folder)

(cffi:defcenum response-type
  (:none -1)
  (:reject -2)
  (:accept -3)
  (:delete-event -4)
  (:ok -5)
  (:cancel -6)
  (:close -7)
  (:yes -8)
  (:no -9)
  (:apply -10)
  (:help -11))

(cffi:defcfun ("gtk_application_new" new-application) :pointer
  (name :string)
  (flags :int))

(cffi:defcfun ("gtk_file_chooser_dialog_new" new-file-chooser) :pointer
  (title :string)
  (parent :pointer)
  (action file-chooser-action)
  (button-a :string)
  (action-a response-type)
  (button-b :string)
  (action-b response-type)
  (delim :pointer))

(cffi:defcfun ("gtk_file_chooser_get_filenames" get-filenames) :pointer
  (chooser :pointer))

(cffi:defcfun ("gtk_file_chooser_get_files" get-files) :pointer
  (chooser :pointer))

(cffi:defcfun ("gtk_file_chooser_get_filename" get-filename) :string
  (chooser :pointer))

(cffi:defcfun ("gtk_file_chooser_set_current_folder" set-current-folder) :bool
  (chooser :pointer)
  (filename :string))

(cffi:defcfun ("gtk_file_chooser_set_filename" set-filename) :bool
  (chooser :pointer)
  (filename :string))

(cffi:defcfun ("gtk_file_chooser_set_select_multiple" set-select-multiple) :void
  (chooser :pointer)
  (multiple :bool))

(cffi:defcfun ("gtk_file_chooser_add_filter" add-filter) :void
  (chooser :pointer)
  (filter :pointer))

(cffi:defcfun ("gtk_file_filter_new" new-filter) :pointer)

(cffi:defcfun ("gtk_file_filter_set_name" set-name) :void
  (filter :pointer)
  (name :string))

(cffi:defcfun ("gtk_file_filter_add_pattern" add-pattern) :void
  (filter :pointer)
  (pattern :string))

(cffi:defcfun ("gtk_dialog_run" dialog-run) response-type
  (dialog :pointer))

(cffi:defcfun ("gtk_widget_destroy" dostrey) :void
  (widget :pointer))

(cffi:defcfun ("g_free" g-free) :void
  (object :pointer))

(cffi:defcfun ("g_object_unref" g-unref) :void
  (object :pointer))

(cffi:defcfun ("g_slist_free" g-slist-free) :void
  (list :pointer))

(cffi:defcstruct (g-slist :class g-slist :conc-name g-slist-)
  (data :pointer)
  (next :pointer))

(cffi:defcfun ("g_file_get_path" g-get-path) :string
  (file :pointer))

(cffi:defcfun ("g_application_run" g-application-run) :int
  (app :pointer)
  (argc :int)
  (argv :pointer))

(cffi:defcfun ("g_signal_connect_data" g-signal-connect) :void
  (instance :pointer)
  (signal :string)
  (handler :pointer)
  (data :pointer)
  (destroy :pointer)
  (flags :int))
