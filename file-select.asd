(asdf:defsystem file-select
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library to invoke the native file selection dialogs to open or save files."
  :homepage "https://shinmera.github.io/file-select/"
  :bug-tracker "https://github.com/shinmera/file-select/issues"
  :source-control (:git "https://github.com/shinmera/file-select.git")
  :serial T
  :defsystem-depends-on (:trivial-features)
  :components ((:file "package")
               (:file "protocol")
               ;(:file "gtk" :if-feature :linux)
               (:file "zenity" :if-feature :linux)
               (:file "yad" :if-feature :linux)
               (:file "kdialog" :if-feature :linux)
               (:file "macos" :if-feature :darwin)
               (:file "win32" :if-feature :windows)
               (:file "defaults")
               (:file "documentation"))
  :depends-on (:cffi
               :float-features
               (:feature :darwin :trivial-main-thread)
               (:feature :windows :com-on)
               :uiop
               :pathname-utils
               :documentation-utils))
