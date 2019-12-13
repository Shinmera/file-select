#|
 This file is a part of file-select
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-select.win32)

(defclass win32 (backend)
  ())

(defmethod initialize-instance :after ((backend win32) &key)
  )

(defmethod finalize ((backend win32)))

(defmethod new-with ((backend win32) &key title default filter multiple &allow-other-keys)
  (open* CLSID-FILE-SAVE-DIALOG title default filter multiple))

(defmethod existing-with ((backend win32) &key title default filter multiple &allow-other-keys)
  (open* CLSID-FILE-OPEN-DIALOG title default filter multiple))

(defmacro unwind-protect* (cleanup &body body)
  `(unwind-protect (progn ,@body) ,cleanup))

(defun shell-item-path (item)
  (cffi:with-foreign-object (pointer :pointer)
    (shell-item-get-display-name item SIGDN-FILESYSPATH pointer)))

;; FIXME: string conversion using windows routines.

(defun open* (type title default filter multiple)
  (cffi:with-foreign-objects ((dialog :pointer)
                              (options dword)
                              (defitem :pointer)
                              (result :pointer)
                              (item :pointer))
    (let ((strings ()))
      (unwind-protect* (mapc #'cffi:foreign-free strings)
        (check-return
         (co-create-instance type (cffi:null-pointer) CLSCTX-INPROC-SERVER dialog))
        (unwind-protect* (file-dialog-release dialog)
          (check-return
           (file-dialog-get-options dialog options))
          (check-return
           (file-dialog-set-options dialog (logior (cffi:mem-ref options dword)
                                                   FOS-FORCEFILESYSTEM
                                                   (if multiple
                                                       FOS-ALLOWMULTISELECT
                                                       0)
                                                   (if (eq :directory filter)
                                                       FOS_PICKFOLDERS
                                                       0))))
          (check-return
           (file-dialog-set-title dialog title))
          (etypecase filter
            (:directory (setf filter ()))
            (string
             (setf filter `((,filter ,filter))))
            (list))
          (when filter
            (let ((structure (cffi:foreign-alloc :pointer :count (* 2 (length filter)))))
              (push structure strings)
              (loop for i from 0 by 0
                    for (name type) in types
                    for namep = (cffi:translate-to-foreign name :string)
                    for typep = (cffi:translate-to-foreign type :string)
                    do (setf (cffi:mem-aref structure :pointer (+ 0 i)) namep)
                       (setf (cffi:mem-aref structure :pointer (+ 1 i)) typep)
                       (push namep strings) (push typep strings))
              (file-dialog-set-file-types dialog (length filter) structure)))
          (when default
            (check-return
             (create-item-from-parsing-name default (cffi:null-pointer) IID-ISHELLITEM defitem))
            (check-return
             (file-dialog-set-default-folder dialog defitem)))
          (unwind-protect* (when default (shell-item-release defitem)))
          (case (file-dialog-show dialog (cffi:null-pointer))
            (:ok
             (values
              (cond (multiple
                     (check-return
                      (file-dialog-get-results dialog result))
                     (unwind-protect* (shell-item-array-release result)
                       (loop for i from 0 below (shell-item-array-get-count result)
                             do (check-return (shell-item-array-get-item-at result i item))
                             collect (shell-item-path item))))
                    (T
                     (check-return
                      (file-dialog-get-result dialog result))
                     (unwind-protect* (shell-item-release result)
                       (shell-item-path result))))
              T))
            (:cancelled
             (values NIL NIL))
            (T (error "..."))))))))
