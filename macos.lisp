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

(defclass macos (backend)
  ())

(defmethod initialize-instance :after ((backend macos) &key)
  (unless (cffi:foreign-library-loaded-p 'cocoa)
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

(cffi:defcfun (class-create-instance "class_createInstance") id
  (class oclass)
  (bytes size_t))

(cffi:defcfun (class-construct-instance "class_constructInstance") id
  (class oclass)
  (buffer :pointer))

(cffi:defcfun (class-destruct-instance "class_destructInstance") :pointer
  (obj id))

(cffi:defcfun (object-dispose "object_dispose") id
  (obj id))

(cffi:defcfun (get-class "objc_getClass") oclass
  (name :string))

(cffi:defcfun (register-name "sel_registerName") sel
  (name :string))

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

(defun create-instance (name)
  (objc-call (objc-call (get-class name) "alloc" id) "init" id))

(defun free-instance (id)
  (objc-call id "free" :void))

(defmacro with-instance ((var class) &body body)
  `(let ((,var (create-instance ,class)))
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

(defun test ()
  (let ((app (objc-call "NSApplication" "sharedApplication" :pointer)))
    (print :a)
    (objc-call app "setActivationPolicy" NSApplicationActivationPolicy :regular :bool)
    (print :b)
    (with-instance (window "NSWindow")
      (objc-call app "runModalForWindow" :pointer (cffi:null-pointer) NSModalResponse))
    (print :c)))
