(asdf:defsystem #:signal-handler
 :description "listen to the USR1 kill signal and read the killer's message"
 :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "description.org"))
 :author "Oleg Shalaev"
 :mailto "oleg@chalaev.com"
 :licence "MIT"
 :version (:read-file-line "version.org")
 :defsystem-depends-on ("asdf-package-system")
 :depends-on (:iolib :cffi :simple-log :shalaev)
 :serial t
 :components ((:file "def-SH") (:file "macros") (:file "signal-handler")))

(asdf:defsystem "signal-handler/example"
:depends-on (:signal-handler :simple-log :shalaev :bordeaux-threads)

:build-operation  "program-op"
:build-pathname "example.bin"
:entry-point "signal-handler/example:main"

:description "an example for signal-handler"
:author "Oleg Shalaev"
:mailto "oleg@chalaev.com"
:licence "MIT"
:version (:read-file-line "version.org")
:serial t
:components ((:file "def-example") (:file "macros") (:file "example")))
