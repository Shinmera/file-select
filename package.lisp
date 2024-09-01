(defpackage #:org.shirakumo.file-select
  (:use #:cl)
  ;; protocol.lisp
  (:export
   #:*default-backend*
   #:file-select-error
   #:new
   #:existing
   #:backend
   #:finalize
   #:new-with
   #:existing-with
   #:native-namestring
   #:parse-native-namestring)
  ;; defaults.lisp
  (:export
   #:no-backend-found
   #:determine-default-backend))

(defpackage #:org.shirakumo.file-select.gtk
  (:use #:cl #:org.shirakumo.file-select)
  (:export #:gtk))

(defpackage #:org.shirakumo.file-select.win32
  (:use #:cl #:org.shirakumo.file-select)
  #+windows (:local-nicknames (#:com #:org.shirakumo.com-on))
  (:export #:win32))

(defpackage #:org.shirakumo.file-select.zenity
  (:use #:cl #:org.shirakumo.file-select)
  (:export #:zenity))

(defpackage #:org.shirakumo.file-select.macos
  (:use #:cl #:org.shirakumo.file-select)
  (:export #:macos)
  (:shadow #:allocate-instance))

(defpackage #:org.shirakumo.file-select.kdialog
  (:use #:cl #:org.shirakumo.file-select)
  (:export #:kdialog))

(defpackage #:org.shirakumo.file-select.yad
  (:use #:cl #:org.shirakumo.file-select)
  (:export #:yad))
