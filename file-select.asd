#|
 This file is a part of file-select
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem file-select
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A library to invoke the native file selection dialogs to open or save files."
  :homepage "https://shinmera.github.io/file-select/"
  :bug-tracker "https://github.com/shinmera/file-select/issues"
  :source-control (:git "https://github.com/shinmera/file-select.git")
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "gtk")
               (:file "zenity")
               (:file "macos")
               (:file "win32")
               (:file "defaults")
               (:file "documentation"))
  :depends-on (:cffi
               :float-features
               :trivial-features
               :documentation-utils))
