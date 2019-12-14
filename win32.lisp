#|
 This file is a part of file-select
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-select.win32)

(defconstant CP-UTF8 65001)
(defconstant CLSCTX-ALL 23)
(cffi:defctype word :uint16)
(cffi:defctype dword :uint32)
(cffi:defctype sfgaof :ulong)

(cffi:define-foreign-library ole32
  (:windows "Ole32.dll"))

(define-condition win32-error (file-select-error)
  ((function-name :initarg :function-name :reader function-name)
   (code :initarg :code :reader code))
  (:report (lambda (c s) (format s "File select operation failed!~%The call to~%  ~a~%returned with unexpected result code ~a."
                                 (function-name c) (code c)))))

(defmacro check-return (value-form &rest expected)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,value-form))
       (if (find ,value ',(or expected '(:ok)))
           ,value
           (error 'win32-error :code ,value :function-name ',(first value-form))))))

(defclass win32 (backend)
  ())

(defmethod initialize-instance :after ((backend win32) &key)
  (cffi:use-foreign-library ole32)
  (check-return
   (co-initialize (cffi:null-pointer) :multi-threaded)))

(defmethod finalize ((backend win32))
  (co-uninitialize))

(defmethod new-with ((backend win32) &key title default filter multiple &allow-other-keys)
  (open* CLSID-FILE-SAVE-DIALOG IID-IFILE-SAVE-DIALOG title default filter multiple))

(defmethod existing-with ((backend win32) &key title default filter multiple &allow-other-keys)
  (open* CLSID-FILE-OPEN-DIALOG IID-IFILE-OPEN-DIALOG title default filter multiple))

(defmacro unwind-protect* (cleanup &body body)
  `(unwind-protect (progn ,@body) ,cleanup))

(cffi:defcenum coinit
  (:apartment-threaded #x2)
  (:multi-threaded #x0)
  (:disable-ole1dde #x4)
  (:speed-over-memory #x8))

(cffi:defcenum hresult
  (:ok #x00000000)
  (:abort #x80004004)
  (:cancelled #x800704C7)
  (:access-denied #x80070005)
  (:fail #x80004005)
  (:handle #x80070006)
  (:invalid-arg #x80070057)
  (:no-interface #x80004002)
  (:not-implemented #x80004001)
  (:out-of-memory #x8007000e)
  (:pointer #x80004003)
  (:unexpected #x8000ffff)
  (:already-initialized 2290679810)
  (:bad-pointer 2147500035)
  (:bufduration-period-not-equal 2290679827)
  (:buffer-error 2290679832)
  (:buffer-operation-pending 2290679819)
  (:buffer-size-error 2290679830)
  (:buffer-size-not-aligned 2290679833)
  (:buffer-too-large 2290679814)
  (:cpuusage-exceeded 2290679831)
  (:device-in-use 2290679818)
  (:device-invalidated 2290679812)
  (:endpoint-create-failed 2290679823)
  (:exclusive-mode-not-allowed 2290679822)
  (:invalid-device-period 2290679840)
  (:invalid-size 2290679817)
  (:not-initialized 2290679809)
  (:out-of-order 2290679815)
  (:service-not-running 2290679824)
  (:unsupported-format 2290679816)
  (:wrong-endpoint-type 2290679811)
  (:class-not-registered 2147746132)
  (:no-aggregation 2147746064))

(cffi:defcenum fileopendialogoptions
  (:overwrite-prompt #x2)
  (:strict-file-types #x4)
  (:no-change-dir #x8)
  (:pick-folders #x20)
  (:force-file-system #x40)
  (:all-non-storage-items #x80)
  (:no-validate #x100)
  (:allow-multi-select #x200)
  (:path-must-exist #x800)
  (:file-must-exist #x1000)
  (:create-prompt #x2000)
  (:share-aware #x4000)
  (:no-read-only-return #x8000)
  (:no-test-file-create #x10000)
  (:hide-mru-places #x20000)
  (:hide-pinned-places #x40000)
  (:no-dereference-links #x100000)
  (:ok-button-needs-interaction #x200000)
  (:dont-add-to-recent #x2000000)
  (:force-show-hidden #x10000000)
  (:default-no-minimode #x20000000)
  (:force-preview-pane-on #x40000000)
  (:support-streamable-items #x80000000))

(cffi:defcenum fdap
  (:bottom 0)
  (:top 1))

(cffi:defcenum sigdn
  (:normal-display 0)
  (:parent-relative-parsing #x80018001)
  (:desktop-absolute-parsing #x80028000)
  (:parent-relative-editing #x80031001)
  (:desktop-absolute-editing #x8004c000)
  (:filesys-path #x80058000)
  (:url #x80068000)
  (:parent-relative-for-addressbar #x8007c001)
  (:parent-relative #x80080001)
  (:parent-relative-for-ui #x80094001))

(cffi:defcenum sichintf
  (:display 0)
  (:all-fields #x80000000)
  (:canonical #x10000000)
  (:test-filesyspath-if-not-equal #x20000000))

(cffi:defcenum siattribflags
  (:and #x1)
  (:or #x2)
  (:appcompat #x3)
  (:mask #x3)
  (:all-items #x4000))

(cffi:defcenum getpropertystoreflags
  :default
  :handler-properties-only
  :read-write
  :temporary
  :fast-properties-only
  :open-slow-item
  :delay-creation
  :best-effort
  :no-oplock
  :prefer-query-properties
  :extrinsic-properties
  :extrinsic-properties-only
  :volatile-properties
  :volatile-propertie-sonly
  :mask-valid)

(cffi:defcstruct (com :conc-name ||)
  (vtbl :pointer))

(cffi:defcstruct (guid :conc-name guid-)
  (data1 dword)
  (data2 word)
  (data3 word)
  (data4 :uint8 :count 8))

(cffi:defcfun (co-initialize "CoInitializeEx") hresult
  (nullable :pointer)
  (init coinit))

(cffi:defcfun (co-uninitialize "CoUninitialize") :void)

(cffi:defcfun (co-create-instance "CoCreateInstance") hresult
  (rclsid :pointer)
  (punkouter :pointer)
  (dwclscontext dword)
  (riid :pointer)
  (ppv :pointer))

(cffi:defcfun (create-item-from-parsing-name "SHCreateItemFromParsingName") hresult
  (path :pointer)
  (ctx :pointer)
  (riid :pointer)
  (shell-item :pointer))

(cffi:defcfun (wide-char-to-multi-byte "WideCharToMultiByte") :int
  (code-page :uint)
  (flags dword)
  (wide-char-str :pointer)
  (wide-char :int)
  (multi-byte-str :pointer)
  (multi-byte :int)
  (default-char :pointer)
  (used-default-char :pointer))

(cffi:defcfun (multi-byte-to-wide-char "MultiByteToWideChar") :int
  (code-page :uint)
  (flags dword)
  (multi-byte-str :pointer)
  (multi-byte :int)
  (wide-char-str :pointer)
  (wide-char :int))

(defun wstring->string (pointer)
  (let ((bytes (wide-char-to-multi-byte CP-UTF8 0 pointer -1 (cffi:null-pointer) 0 (cffi:null-pointer) (cffi:null-pointer))))
    (cffi:with-foreign-object (string :uchar bytes)
      (wide-char-to-multi-byte CP-UTF8 0 pointer -1 string bytes (cffi:null-pointer) (cffi:null-pointer))
      (cffi:foreign-string-to-lisp string :encoding :utf-8))))

(defun string->wstring (string)
  (cffi:with-foreign-string (string string)
    (let* ((chars (multi-byte-to-wide-char CP-UTF8 0 string -1 (cffi:null-pointer) 0))
           (pointer (cffi:foreign-alloc :uint16 :count chars)))
      (multi-byte-to-wide-char CP-UTF8 0 string -1 pointer chars)
      pointer)))

(defun com-release (pointer)
  (cffi:foreign-funcall-pointer
   (cffi:mem-aref (vtbl pointer) :pointer 2)
   ()
   :pointer pointer
   :unsigned-long))

(defun make-guid (d1 d2 d3 &rest d4)
  (let ((ptr (cffi:foreign-alloc '(:struct guid))))
    (setf (guid-data1 ptr) d1)
    (setf (guid-data2 ptr) d2)
    (setf (guid-data3 ptr) d3)
    (loop for i from 0 below 8
          for d in d4
          do (setf (cffi:mem-aref (cffi:foreign-slot-pointer ptr '(:struct guid) 'data4) :uint8 i)
                   d))
    ptr))

(defvar CLSID-FILE-SAVE-DIALOG
  (make-guid #xC0B4E2F3 #xBA21 #x4773 #x8D #xBA #x33 #x5E #xC9 #x46 #xEB #x8B))

(defvar CLSID-FILE-OPEN-DIALOG
  (make-guid #xDC1C5A9C #xE88A #x4DDE #xA5 #xA1 #x60 #xF8 #x2A #x20 #xAE #xF7))

(defvar IID-IFILE-DIALOG
  (make-guid #x42F85136 #xDB7E #x439C #x85 #xF1 #xE4 #x07 #x5D #x13 #x5F #xC8))

(defvar IID-IFILE-SAVE-DIALOG
  (make-guid #x84BCCD23 #x5FDE #x4CDB #xAE #xA4 #xAF #x64 #xB8 #x3D #x78 #xAB))

(defvar IID-IFILE-OPEN-DIALOG
  (make-guid #xD57C7288 #xD4AD #x4768 #xBE #x02 #x9D #x96 #x95 #x32 #xD9 #x60))

(defvar IID-ISHELL-ITEM
  (make-guid #x43826D1E #xE718 #x42EE #xBC #x55 #xA1 #xE2 #x61 #xC3 #x7B #xFE))

(defmacro define-comfun ((struct method &rest options) return-type &body args)
  (let* ((*print-case* (readtable-case *readtable*))
         (structg (gensym "STRUCT"))
         (name (intern (format NIL "~a-~a" struct method))))
    `(progn
       (declaim (inline ,name))
       (defun ,name (,structg ,@(mapcar #'first args))
         (cffi:foreign-funcall-pointer
          (,(intern (format NIL "%~a" name))
           (vtbl ,structg))
          ,options
          :pointer ,structg
          ,@(loop for (name type) in args
                  collect type collect name)
          ,return-type)))))

(defmacro define-comstruct (name &body methods)
  (let ((methods (list* `(query-interface hresult)
                        `(add-ref :unsigned-long)
                        `(release :unsigned-long)
                        methods)))
    `(progn
       (cffi:defcstruct (,name :conc-name ,(format NIL "%~a-" name))
         ,@(loop for method in methods
                 collect (list (first method) :pointer)))

       ,@(loop for (method return . args) in methods
               collect `(define-comfun (,name ,method) ,return
                          ,@args)))))

(define-comstruct file-dialog
  (show hresult (owner :pointer))
  (set-file-types hresult (file-types :uint) (filter-spec :pointer))
  (set-file-type-index hresult (file-type :uint))
  (get-file-type-index hresult (file-type :pointer))
  (advise hresult (events :pointer) (cookie :pointer))
  (unadvise hresult (cookie dword))
  (set-options hresult (options fileopendialogoptions))
  (get-options hresult (options :pointer))
  (set-default-folder hresult (shell-item :pointer))
  (set-folder hresult (shell-item :pointer))
  (get-folder hresult (shell-item :pointer))
  (get-current-selection hresult (shell-item :pointer))
  (set-file-name hresult (name :pointer))
  (get-file-name hresult (name :pointer))
  (set-title hresult (title :pointer))
  (set-ok-button-label hresult (text :pointer))
  (set-file-name-label hresult (label :pointer))
  (get-result hresult (shell-item :pointer))
  (add-place hresult (shell-item :pointer) (fdap fdap))
  (set-default-extension hresult (extension :pointer))
  (close hresult (result hresult))
  (set-client-guid hresult (guid :pointer))
  (clear-client-data hresult)
  (set-filter hresult (filter :pointer)))

(define-comstruct file-open-dialog
  (show hresult (owner :pointer))
  (set-file-types hresult (file-types :uint) (filter-spec :pointer))
  (set-file-type-index hresult (file-type :uint))
  (get-file-type-index hresult (file-type :pointer))
  (advise hresult (events :pointer) (cookie :pointer))
  (unadvise hresult (cookie dword))
  (set-options hresult (options fileopendialogoptions))
  (get-options hresult (options :pointer))
  (set-default-folder hresult (shell-item :pointer))
  (set-folder hresult (shell-item :pointer))
  (get-folder hresult (shell-item :pointer))
  (get-current-selection hresult (shell-item :pointer))
  (set-file-name hresult (name :pointer))
  (get-file-name hresult (name :pointer))
  (set-title hresult (title :pointer))
  (set-ok-button-label hresult (text :pointer))
  (set-file-name-label hresult (label :pointer))
  (get-result hresult (shell-item :pointer))
  (add-place hresult (shell-item :pointer) (fdap fdap))
  (set-default-extension hresult (extension :pointer))
  (close hresult (result hresult))
  (set-client-guid hresult (guid :pointer))
  (clear-client-data hresult)
  (set-filter hresult (filter :pointer))
  (get-results hresult (shell-item-array :pointer))
  (get-selected-items hresult (shell-item-array :pointer)))

(define-comstruct shell-item
  (bind-to-handler hresult (ctx :pointer) (bhid :pointer) (riid :pointer) (ppv :pointer))
  (get-parent hresult (parent :pointer))
  (get-display-name hresult (type sigdn) (name :pointer))
  (get-attributes hresult (mask sfgaof) (attributes :pointer))
  (compare hresult (shell-item :pointer) (hint sichintf) (pi-order :pointer)))

(define-comstruct shell-item-array
  (bind-to-handler hresult (ctx :pointer) (bhid :pointer) (riid :pointer) (ppv :pointer))
  (get-property-store hresult (flags getpropertystoreflags) (riid :pointer) (ppv :pointer))
  (get-property-description-list hresult (key-type :pointer) (riid :pointer) (ppv :pointer))
  (get-attributes hresult (attrib-flags siattribflags) (mask sfgaof) (attribs :pointer))
  (get-count hresult (num-items :pointer))
  (get-item-at hresult (index dword) (shell-item :pointer))
  (enum-items hresult (enumerator :pointer)))

(defun shell-item-path (item)
  (cffi:with-foreign-object (pointer :pointer)
    (shell-item-get-display-name item :filesys-path pointer)
    (wstring->string (cffi:mem-ref pointer :pointer))))

(defmacro with-deref ((var type) &body init)
  `(cffi:with-foreign-object (,var ,type)
     (check-return ,@init)
     (cffi:mem-ref ,var ,type)))

(defmacro with-com-object (var init &body body)
  `(let ((,var (with-deref (,var :pointer) ,init)))
     (unwind-protect
          (progn ,@body)
       (com-release ,var))))

;; FIXME: string conversion using windows routines.

(defun open* (clsid iid title default filter multiple)
  (let ((strings ()) defitem)
    (flet ((wstring (string)
             (car (push (string->wstring string) strings))))
      (unwind-protect* (mapc #'cffi:foreign-free strings)
        (with-com-object dialog (co-create-instance clsid (cffi:null-pointer) CLSCTX-ALL iid dialog)
          (let ((options (with-deref (options 'dword) (file-dialog-get-options dialog options))))
            (check-return
                (file-dialog-set-options dialog (logior options
                                                        (cffi:foreign-enum-value 'fileopendialogoptions :force-file-system)
                                                        (if multiple
                                                            (cffi:foreign-enum-value 'fileopendialogoptions :allow-multi-select)
                                                            0)
                                                        (if (eq :directory filter)
                                                            (cffi:foreign-enum-value 'fileopendialogoptions :pick-folders)
                                                            0)))))
          (check-return
              (file-dialog-set-title dialog (wstring title)))
          (etypecase filter
            ((eql :directory) (setf filter ()))
            (string
             (setf filter `((,filter ,filter))))
            (list))
          (when filter
            (let ((structure (cffi:foreign-alloc :pointer :count (* 2 (length filter)))))
              (push structure strings)
              (loop for i from 0 by 2
                    for (name type) in filter
                    do (setf (cffi:mem-aref structure :pointer (+ 0 i)) (wstring name))
                       (setf (cffi:mem-aref structure :pointer (+ 1 i)) (wstring type)))
              (file-dialog-set-file-types dialog (length filter) structure)))
          (when default
            (let ((filename (file-namestring default))
                  (directory (namestring (make-pathname :name NIL :type NIL :defaults default))))
              (setf defitem (with-deref (defitem :pointer) (create-item-from-parsing-name (wstring directory) (cffi:null-pointer) IID-ISHELL-ITEM defitem)))
              (check-return (file-dialog-set-folder dialog defitem))
              (check-return (file-dialog-set-file-name dialog (wstring filename)))))
          (unwind-protect* (when defitem (com-release defitem))
            (case (check-return (file-dialog-show dialog (cffi:null-pointer)) :ok :cancelled)
              (:ok
               (values
                (cond (multiple
                       (with-com-object result (file-open-dialog-get-results dialog result)
                         (loop for i from 0 below (with-deref (num 'dword) (shell-item-array-get-count result num))
                               collect (with-com-object item (shell-item-array-get-item-at result i item)
                                         (shell-item-path item)))))
                      (T
                       (with-com-object result (file-dialog-get-result dialog result)
                         (shell-item-path result))))
                T))
              (:cancelled
               (values NIL NIL)))))))))
