#|
 This file is a part of file-select
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-select.win32)

(cffi:defctype word :uint16)
(cffi:defctype dword :uint32)
(cffi:defctype sfgaof :ulong)

(com:define-guid CLSID-FILE-SAVE-DIALOG
  #xC0B4E2F3 #xBA21 #x4773 #x8D #xBA #x33 #x5E #xC9 #x46 #xEB #x8B)

(com:define-guid CLSID-FILE-OPEN-DIALOG
  #xDC1C5A9C #xE88A #x4DDE #xA5 #xA1 #x60 #xF8 #x2A #x20 #xAE #xF7)

(com:define-guid IID-IFILE-DIALOG
  #x42F85136 #xDB7E #x439C #x85 #xF1 #xE4 #x07 #x5D #x13 #x5F #xC8)

(com:define-guid IID-IFILE-SAVE-DIALOG
  #x84BCCD23 #x5FDE #x4CDB #xAE #xA4 #xAF #x64 #xB8 #x3D #x78 #xAB)

(com:define-guid IID-IFILE-OPEN-DIALOG
  #xD57C7288 #xD4AD #x4768 #xBE #x02 #x9D #x96 #x95 #x32 #xD9 #x60)

(com:define-guid IID-ISHELL-ITEM
  #x43826D1E #xE718 #x42EE #xBC #x55 #xA1 #xE2 #x61 #xC3 #x7B #xFE)

(define-condition win32-error (file-select-error)
  ((function-name :initarg :function-name :reader function-name)
   (code :initarg :code :reader code))
  (:report (lambda (c s) (format s "File select operation failed!~%The call to~%  ~a~%returned with unexpected result code ~a."
                                 (function-name c) (code c)))))

(defmacro check-return (value-form &rest expected)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,value-form))
       (if (find ,value ',(or expected '(:ok :false)))
           ,value
           (error 'win32-error :code ,value :function-name ',(first value-form))))))

(defclass win32 (backend)
  ())

(defmethod initialize-instance :after ((backend win32) &key)
  (com:init))

(defmethod new-with ((backend win32) &key title default filter multiple &allow-other-keys)
  (open* CLSID-FILE-SAVE-DIALOG IID-IFILE-SAVE-DIALOG title default filter multiple))

(defmethod existing-with ((backend win32) &key title default filter multiple &allow-other-keys)
  (open* CLSID-FILE-OPEN-DIALOG IID-IFILE-OPEN-DIALOG title default filter multiple))

(defmacro unwind-protect* (cleanup &body body)
  `(unwind-protect (progn ,@body) ,cleanup))

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

(cffi:defcfun (create-item-from-parsing-name "SHCreateItemFromParsingName") com:hresult
  (path :pointer)
  (ctx :pointer)
  (riid com:guid)
  (shell-item :pointer))

(com:define-comstruct file-dialog
  (show (owner :pointer))
  (set-file-types (file-types :uint) (filter-spec :pointer))
  (set-file-type-index (file-type :uint))
  (get-file-type-index (file-type :pointer))
  (advise (events :pointer) (cookie :pointer))
  (unadvise (cookie dword))
  (set-options (options fileopendialogoptions))
  (get-options (options :pointer))
  (set-default-folder (shell-item :pointer))
  (set-folder (shell-item :pointer))
  (get-folder (shell-item :pointer))
  (get-current-selection (shell-item :pointer))
  (set-file-name (name :pointer))
  (get-file-name (name :pointer))
  (set-title (title :pointer))
  (set-ok-button-label (text :pointer))
  (set-file-name-label (label :pointer))
  (get-result (shell-item :pointer))
  (add-place (shell-item :pointer) (fdap fdap))
  (set-default-extension (extension :pointer))
  (close (result com:hresult))
  (set-client-guid (guid :pointer))
  (clear-client-data)
  (set-filter (filter :pointer)))

(com:define-comstruct file-open-dialog
  (show (owner :pointer))
  (set-file-types (file-types :uint) (filter-spec :pointer))
  (set-file-type-index (file-type :uint))
  (get-file-type-index (file-type :pointer))
  (advise (events :pointer) (cookie :pointer))
  (unadvise (cookie dword))
  (set-options (options fileopendialogoptions))
  (get-options (options :pointer))
  (set-default-folder (shell-item :pointer))
  (set-folder (shell-item :pointer))
  (get-folder (shell-item :pointer))
  (get-current-selection (shell-item :pointer))
  (set-file-name (name :pointer))
  (get-file-name (name :pointer))
  (set-title (title :pointer))
  (set-ok-button-label (text :pointer))
  (set-file-name-label (label :pointer))
  (get-result (shell-item :pointer))
  (add-place (shell-item :pointer) (fdap fdap))
  (set-default-extension (extension :pointer))
  (close (result com:hresult))
  (set-client-guid (guid :pointer))
  (clear-client-data)
  (set-filter (filter :pointer))
  (get-results (shell-item-array :pointer))
  (get-selected-items (shell-item-array :pointer)))

(com:define-comstruct shell-item
  (bind-to-handler (ctx :pointer) (bhid :pointer) (riid :pointer) (ppv :pointer))
  (get-parent (parent :pointer))
  (get-display-name (type sigdn) (name :pointer))
  (get-attributes (mask sfgaof) (attributes :pointer))
  (compare (shell-item :pointer) (hint sichintf) (pi-order :pointer)))

(com:define-comstruct shell-item-array
  (bind-to-handler (ctx :pointer) (bhid :pointer) (riid :pointer) (ppv :pointer))
  (get-property-store (flags getpropertystoreflags) (riid :pointer) (ppv :pointer))
  (get-property-description-list (key-type :pointer) (riid :pointer) (ppv :pointer))
  (get-attributes (attrib-flags siattribflags) (mask sfgaof) (attribs :pointer))
  (get-count (num-items :pointer))
  (get-item-at (index dword) (shell-item :pointer))
  (enum-items (enumerator :pointer)))

(defun shell-item-path (item)
  (cffi:with-foreign-object (pointer :pointer)
    (shell-item-get-display-name item :filesys-path pointer)
    (parse-native-namestring (com:wstring->string (cffi:mem-ref pointer :pointer)))))

(defmacro with-deref ((var type) &body init)
  `(cffi:with-foreign-object (,var ,type)
     (check-return ,@init)
     (cffi:mem-ref ,var ,type)))

(defmacro with-com-object (var init &body body)
  `(let ((,var (with-deref (,var :pointer) ,init)))
     (unwind-protect
          (progn ,@body)
       (com:release ,var))))

(defun open* (clsid iid title default filter multiple)
  (let ((strings ()) defitem)
    (flet ((wstring (string)
             (car (push (com:string->wstring string) strings))))
      (unwind-protect* (mapc #'cffi:foreign-free strings)
        (com:with-com (dialog (com:create clsid iid))
          (let ((options (com:with-deref (options 'dword) (file-dialog-get-options dialog options))))
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
                       (setf (cffi:mem-aref structure :pointer (+ 1 i)) (wstring (format NIL "*.~a" type))))
              (file-dialog-set-file-types dialog (length filter) structure)))
          (when default
            (let ((filename (file-namestring default))
                  (directory (native-namestring (make-pathname :name NIL :type NIL :defaults default))))
              (setf defitem (com:with-deref (defitem :pointer) (create-item-from-parsing-name (wstring directory) (cffi:null-pointer) IID-ISHELL-ITEM defitem)))
              (check-return (file-dialog-set-folder dialog defitem))
              (check-return (file-dialog-set-file-name dialog (wstring filename)))))
          (unwind-protect* (when defitem (com:release defitem))
            (case (check-return (file-dialog-show dialog (cffi:null-pointer)) :ok :cancelled)
              (:ok
               (values
                (cond (multiple
                       (with-com-object result (file-open-dialog-get-results dialog result)
                         (loop for i from 0 below (com:with-deref (num 'dword) (shell-item-array-get-count result num))
                               collect (with-com-object item (shell-item-array-get-item-at result i item)
                                         (shell-item-path item)))))
                      (T
                       (with-com-object result (file-dialog-get-result dialog result)
                         (shell-item-path result))))
                T))
              (:cancelled
               (values NIL NIL)))))))))
