#|
 This file is a part of file-select
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-select.macos)

(defclass macos (backend)
  ())

(defmethod initialize-instance :after ((backend macos)))

(defmethod finalize ((backend macos)))

(defmethod new-with ((backend macos) &key title default filter multiple)
  )

(defmethod existing-with ((backend macos) &key title default filter multiple)
  )
