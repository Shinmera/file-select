(asdf:load-system :staple-markless)

(defmethod staple:packages ((system (eql (asdf:find-system :file-select))))
  (mapcar #'find-package '(#:org.shirakumo.file-select
                           #:org.shirakumo.file-select.gtk
                           #:org.shirakumo.file-select.macos
                           #:org.shirakumo.file-select.win32
                           #:org.shirakumo.file-select.zenity)))
