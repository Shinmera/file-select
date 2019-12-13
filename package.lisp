#|
 This file is a part of file-select
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.file-select
  (:use #:cl)
  ;; protocol.lisp
  (:export
   #:*default-backend*
   #:new
   #:existing
   #:backend
   #:new-with
   #:existing-with))

(defpackage #:org.shirakumo.file-select.gtk
  (:use #:cl #:org.shirakumo.file-select)
  (:export #:gtk))
