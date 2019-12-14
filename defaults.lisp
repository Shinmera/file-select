#|
 This file is a part of file-select
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-select)

(define-condition file-select-backend-not-found (warning)
  ()
  (:report (lambda (c s) (format s "No usable backend for file-select could be found!"))))

(defun split (char string)
  (let ((paths ())
        (buffer (make-string-output-stream)))
    (flet ((maybe-commit ()
             (let ((string (get-output-stream-string buffer)))
               (when (string/= string "")
                 (push (parse-native-namestring string) paths)))))
      (loop for c across string
            do (if (char= c char)
                   (maybe-commit)
                   (write-char c buffer))
            finally (maybe-commit)))
    (nreverse paths)))

(defun find-in-path (file)
  (dolist (path (split #\: (uiop:getenv "PATH")))
    (when (probe-file (merge-pathnames file path))
      (return (merge-pathnames file path)))))

(defun determine-default-backend ()
  (cond ((find :win32 *features*)
         'org.shirakumo.file-select.win32:win32)
        ((find :darwin *features*)
         'org.shirakumo.file-select.macos:macos)
        ((find-in-path "zenity")
         'org.shirakumo.file-select.zenity:zenity)
        ((ignore-errors (cffi:load-foreign-library 'org.shirakumo.file-select.gtk:gtk))
         (cffi:close-foreign-library 'org.shirakumo.file-select.gtk:gtk)
         'org.shirakumo.file-select.gtk:gtk)
        (T
         (warn 'file-select-backend-not-found))))

(unless (boundp '*default-backend*)
  (setf *default-backend* (determine-default-backend)))
