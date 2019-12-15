#|
 This file is a part of file-select
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-select)

;; defaults.lisp
(docs:define-docs
  (type no-backend-found
    "Error signalled when no backend could be found.

This usually means that no implementation for file selection is
available. It could mean that auto-detection failed, in which case
*DEFAULT-BACKEND* should be manually set to the proper value.

See *DEFAULT-BACKEND*
See DETERMINE-DEFAULT-BACKEND")

  (function determine-default-backend
    "Attempt to determine the proper backend to use for the current system.

This will return the name of the class to use for the backend, or
signal an error of type NO-BACKEND-FOUND if no usable backend could be
found at all.

More specifically, the following detection heuristic is used:

- On WIN32, the WIN32 backend is used.
- On DARWIN, the MACOS backend is used.
- If a \"zenity\" binary is found in one of the PATH directories, the
  ZENITY backend is used.
- If the gtk3 shared library file can be loaded, the GTK backend is
  used.
- Otherwise NO-BACKEND-FOUND is signalled.

See NO-BACKEND-FOUND"))

;; protocol.lisp
(docs:define-docs
  (variable *default-backend*
    "Holds the name of the default file selection backend to use.

This should be a value suitable for the first argument of NEW-WITH and
EXISTING-WITH.

On first call of NEW or EXISTING, if this variable is still unbound,
it is set to the value of DETERMINE-DEFAULT-BACKEND.

See NEW
See EXISTING
See DETERMINE-DEFAULT-BACKEND")

  (type file-select-error
    "Superclass for any kind of error signalled during the file selection.

Typically a subclass of this error is signalled depending on the
backend used. This kind of error should only every be signalled if
critical problems occur that prevent the backend from operating
correctly.")

  (function new
    "Select a new file.

This will open a \"save file\" dialog.

Returns two values:

- The selected file as a pathname, if any
- Whether the user completed the operation successfully

If no BACKEND is specified, *DEFAULT-BACKEND* is used.

TITLE may be a string to designate the file dialog window title.

DEFAULT should be a pathname pointing to a default file or directory
that the dialog should present on opening.

FILTER can be one of the following:

  :DIRECTORY     --- Only allow selecting directories.
  STRING         --- Shorthand for ((\"\" TYPE))
  ((NAME TYPE)*) --- Restrict the file selection to the specified
                     types. NAME should be a human-readable
                     description, and TYPE should be a PATHNAME-TYPE
                     to allow for selection.

A backend may support additional arguments.

See NEW-WITH
See *DEFAULT-BACKEND*")

  (function existing
    "Select an existing file.

This will open an \"open file\" dialog.

Returns two values:

- The selected file as a pathname, or as a list if multiple.
- Whether the user completed the operation successfully

If no BACKEND is specified, *DEFAULT-BACKEND* is used.

TITLE may be a string to designate the file dialog window title.

DEFAULT should be a pathname pointing to a default file or directory
that the dialog should present on opening.

FILTER can be one of the following:

  :DIRECTORY     --- Only allow selecting directories.
  STRING         --- Shorthand for ((\"\" TYPE))
  ((NAME TYPE)*) --- Restrict the file selection to the specified
                     types. NAME should be a human-readable
                     description, and TYPE should be a PATHNAME-TYPE
                     to allow for selection.

MULTIPLE designates whether multiple files can be selected. In that
case, the first return value is always a list.

A backend may support additional arguments.

See EXISTING-WITH
See *DEFAULT-BACKEND*")
  
  (type backend
    "Superclass for all file selection backend implementations.

The user should not create instances of this class, or any of its
subclasses. The system will automatically construct singleton
instances of the class when used with NEW-WITH and EXISTING-WITH.

When creating a subclass, you may use INITIALIZE-INSTANCE to perform
one-time setup, such as loading and initialising foreign libraries.

In order to uninitialize the libraries, you should implement a
FINALIZE method.

The user may call FINALIZE on a backend instance or name. The instance
will be invalidated afterwards, and a repeat call on NEW-WITH or
EXISTING-WITH with the backend name will create a new instance.

See FINALIZE
See NEW-WITH
See EXISTING-WITH")

  (function finalize
    "Finalize the backend and free any resources it might have allocated.

This will NOT close any foreign libraries it might have opened.
After calling FINALIZE on a backend, the backend instance is
invalidated and may not be used again.

See BACKEND")
  
  (function new-with
    "Open a \"save file\" dialog with the specified backend.

The implementation must adhere to the protocol specified in NEW.

see NEW
See BACKEND")
  
  (function existing-with
    "Open an \"open file\" dialog with the specified backend.

The implementation must adhere to the protocol specified in EXISTING.

See EXISTING
See BACKEND"))

;; backends
(docs:define-docs
  (type org.shirakumo.file-select.gtk:gtk
    "Implementation using the GTK3 framework.

Note that this will create and destroy a new GTK application on every
use, and requires the GTK3 libraries to be present.

See BACKEND")

  (type org.shirakumo.file-select.macos:macos
    "Implementation using the AppKit/Cocoa framework (OSX 10.0 and above).

Note that MacOS requires all UI to happen in Thread 0. To this end,
this implementation makes use of TRIVIAL-MAIN-THREAD to schedule the
file selection in the main thread, if it isn't already.

See BACKEND")

  (type org.shirakumo.file-select.win32:win32
    "Implementation using the Win32 IFileDialog framework (Win Vista and above).

See BACKEND")

  (type org.shirakumo.file-select.zenity:zenity
    "Implementation using the Zenity GTK dialog utility.

This is preferable over the native GTK backend as it does not require
loading the foreign libraries or constructing a GTK application
in-process. UIOP:RUN-PROGRAM is used to launch the Zenity process.

See BACKEND"))
