#|
 This file is a part of file-select
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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

(defmethod finalize ((backend macos)))

(defmethod new-with ((backend macos) &key title default filter multiple)
  )

(defmethod existing-with ((backend macos) &key title default filter multiple)
  )

(cffi:defctype size_t #+x86-64 :uint64 #+x86 :uint32 #-(or x86-64 x86) :long)
(cffi:defctype id :pointer)
(cffi:defctype oclass :pointer)
(cffi:defctype sel :pointer)

(cffi:defcfun (get-class "objc_getClass") oclass
  (name :string))

(cffi:defcfun (register-name "sel_registerName") sel
  (name :string))

(cffi:defcfun (ns-make-rect "NSMakeRect") :pointer
  (x :int) (y :int) (w :int) (h :int))

(cffi:defcfun (set-uncaught-exception-handler "NSSetUncaughtExceptionHandler") :void
  (handler :pointer))

(cffi:defcallback exception-handler :void ((object id) (pointer :pointer))
  (error "Fuck!"))

(defmacro objc-call (self method &rest args)
  (when (stringp self)
    (setf self `(get-class ,self)))
  (let* ((struct (gensym "STRUCT"))
         (retval (or (car (last args)) :void))
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

(defmacro define-objc-method ((name method) retval &body args)
  (let ((self (gensym "SELF")))
    `(defun ,name (,self ,@(mapcar #'first args))
       (objc-call ,self ,method ,@(loop for (name type) in args collect type collect name) ,retval))))

(defun allocate-instance (name)
  (objc-call (get-class name) "alloc" id))

(defun create-instance (name)
  (objc-call (allocate-instance name) "init" id))

(defun free-instance (id)
  (objc-call id "free" :void))

(defmacro with-instance ((var class &rest init) &body body)
  `(let ((,var (objc-call (allocate-instance ,class) ,@(or init (list "init")) id)))
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

(defun test ()
  (set-uncaught-exception-handler (cffi:callback exception-handler))
  (let ((app (objc-call "NSApplication" "sharedApplication" :pointer)))
    (print (list app nsapp))
    (print :a)
    (objc-call app "setActivationPolicy" NSApplicationActivationPolicy :regular :bool)
    (print :b)
    (with-instance (window "NSWindow" "initWithContentRect"
                           :pointer (ns-make-rect 0 0 320 240)
                           NSWindowStyleMask :titled
                           NSBackingStoreType :buffered
                           :bool NIL)
      (objc-call app "runModalForWindow" :pointer window NSModalResponse))
    (print :c)))
