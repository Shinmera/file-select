(in-package #:org.shirakumo.file-select.macos)

(cffi:define-foreign-library cocoa
  (t (:framework "Cocoa")))

(cffi:define-foreign-library appkit
  (t (:framework "AppKit")))

(cffi:define-foreign-library foundation
  (t (:framework "Foundation")))

(defclass macos (backend)
  ())

(defmethod initialize-instance :after ((backend macos) &key)
  (unless (cffi:foreign-library-loaded-p 'cocoa)
    (cffi:use-foreign-library foundation)
    (cffi:use-foreign-library cocoa)
    (cffi:use-foreign-library appkit)))

(defmethod new-with ((backend macos) &rest args)
  (apply #'open* "NSSavePanel" "savePanel" args))

(defmethod existing-with ((backend macos) &rest args)
  (apply #'open* "NSOpenPanel" "openPanel" args))

(cffi:defctype size_t #+x86-64 :uint64 #+x86 :uint32 #-(or x86-64 x86) :long)
(cffi:defctype id :pointer)
(cffi:defctype oclass :pointer)
(cffi:defctype sel :pointer)

(cffi:defcfun (get-class "objc_getClass") oclass
  (name :string))

(cffi:defcfun (register-name "sel_registerName") sel
  (name :string))

(cffi:defcfun (set-uncaught-exception-handler "NSSetUncaughtExceptionHandler") :void
  (handler :pointer))

(cffi:defcallback exception-handler :void ((object id) (pointer :pointer))
  (error "Fuck!"))

(defmacro objc-call (self method &rest args)
  (when (stringp self)
    (setf self `(get-class ,self)))
  (when (evenp (length args))
    (setf args (append args '(id))))
  (let* ((struct (gensym "STRUCT"))
         (retval (car (last args)))
         (base-args (list* 'id self
                           'sel `(register-name ,method)
                           (loop for (type name) on (butlast args) by #'cddr
                                 collect `,type collect name))))
    (cond ((and (listp retval) (eq :struct (first retval)))
           `(cffi:with-foreign-object (,struct ',retval)
              (cffi:foreign-funcall "objc_msgSend_stret" :pointer ,struct ,@base-args :void)
              (cffi:mem-ref ,struct ',retval)))
          ((find retval '(:double :float))
           `(cffi:foreign-funcall "objc_msgSend_fpret" ,@base-args ,retval))
          (T
           `(cffi:foreign-funcall "objc_msgSend" ,@base-args ,retval)))))

(defun free-instance (id)
  (objc-call id "dealloc" :void))

(defmacro with-object ((var init) &body body)
  `(let ((,var ,init))
     (unwind-protect
          (progn ,@body)
       (free-instance ,var))))

(cffi:defcenum NSApplicationActivationPolicy
  (:regular 0)
  (:accessory 1)
  (:prohibited 2))

(cffi:defcenum NSModalResponse
  (:cancel 0)
  (:ok 1))

(cffi:defcenum NSWindowStyleMask
  (:borderless 0)
  (:titled 1)
  (:closable 2)
  (:miniaturizable 4)
  (:resizable 8)
  (:utility-window 16)
  (:doc-modal-window 32)
  (:non-activating-panel 64)
  (:unified-title-and-toolbar 4096)
  (:full-screen 16384)
  (:full-size-content-view 32768)
  (:hud-window 8192))

(cffi:defcenum NSBackingStoreType
  (:buffered 2))

(cffi:defcvar (nsapp "NSApp") :pointer)

(cffi:defcenum (NSEventMask :uint64)
  (:any #.(1- (ash 1 64))))

(cffi:defcvar (nsdefaultrunloopmode "NSDefaultRunLoopMode") :pointer)

(defun ensure-url-path (url filter)
  (let ((string (objc-call url "fileSystemRepresentation" :string)))
    (when (eq filter :directory)
      (setf string (format NIL "~a/" string)))
    (parse-native-namestring string)))

(defun process-event (app)
  (let ((event (objc-call app "nextEventMatchingMask:untilDate:inMode:dequeue:"
                          NSEventMask :any
                          :pointer (objc-call "NSDate" "distantPast")
                          :pointer nsdefaultrunloopmode
                          :bool T)))
    (unless (cffi:null-pointer-p event)
      (objc-call app "sendEvent:" :pointer event))))

(defmacro with-body-in-main-thread (args &body body)
  #+darwin `(trivial-main-thread:with-body-in-main-thread ,args ,@body)
  #-darwin `(progn ,@body))

(defun open* (class constructor &key title default filter multiple message backend)
  (declare (ignore backend))
  (with-body-in-main-thread (:blocking T)
    (float-features:with-float-traps-masked T
      (let ((strings ()))
        (unwind-protect
             (flet ((nsstring (string)
                      (car (push (objc-call "NSString" "stringWithUTF8String:" :string string) strings))))
               (set-uncaught-exception-handler (cffi:callback exception-handler))
               (let ((app (objc-call "NSApplication" "sharedApplication")))
                 (objc-call app "setActivationPolicy:" NSApplicationActivationPolicy :accessory :bool)
                 (with-object (window (objc-call (get-class class) constructor))
                   (objc-call window "setTitle:" :pointer (nsstring title))
                   (when message
                     (objc-call window "setMessage:" :pointer (nsstring message)))
                   (objc-call window "setAllowsMultipleSelection:" :bool multiple)
                   (objc-call window "setCanChooseDirectories:" :bool (eq filter :directory))
                   (objc-call window "setCanChooseFiles:" :bool (not (eq filter :directory)))
                   (when default
                     (objc-call window "setNameFieldStringValue:" :pointer (nsstring (file-namestring default)))
                     (with-object (url (objc-call "NSURL" "URLWithString:" :pointer (nsstring (native-namestring default))))
                       (objc-call window "setDirectoryURL:" :pointer url)))
                   (when (stringp filter)
                     (setf filter `(("" ,filter))))
                   (when (consp filter)
                     (cffi:with-foreign-object (list :pointer (length filter))
                       (loop for i from 0
                             for (name type) in filter
                             do (setf (cffi:mem-aref list :pointer i) (nsstring type)))
                       (with-object (array (objc-call "NSArray" "arrayWithObjects:count:" :pointer list :uint (length filter)))
                         (objc-call window "setAllowedFileTypes:" :pointer array))))
                   (ecase (unwind-protect (objc-call window "runModal" NSModalResponse)
                            ;; This is necessary to get the window to close.
                            (loop while (process-event app)))
                     (:cancel (values NIL NIL))
                     (:ok
                      (values
                       (if multiple
                           (let ((urls (objc-call window "URLs")))
                             (loop for i from 0 below (objc-call urls "count" :uint)
                                   for url = (objc-call urls "objectAtIndex:" :uint i)
                                   collect (ensure-url-path url filter)))
                           (ensure-url-path (objc-call window "URL") filter))
                       T))))))
          (mapc #'free-instance strings))))))
