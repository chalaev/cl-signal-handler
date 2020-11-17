(asdf:defsystem "signal-handler"
 :description "listen to the USR1 kill signal and read the killer's message"
 :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "description.org"))
 :author "Oleg Shalaev"
 :mailto "oleg@chalaev.com"
 :licence "MIT"
 :version "0"
 :depends-on (:iolib :cffi :simple-log :uiop)
;; :in-order-to ((test-op (test-op :signal-handler/tests)))
:serial t
:components (
(:file "goodies/macros")
(:file "goodies/functions")
(:file "goodies/file-functions")
(:file "signal-handler")))

(asdf:defsystem "signal-handler/example"
:depends-on (:signal-handler)

:build-operation  "program-op"
:build-pathname "example.bin"
:entry-point "signal-handler/example:main"

:description "an example for signal-handler"
:author "Oleg Shalaev"
:mailto "oleg@chalaev.com"
:licence "MIT"
:version "0"
:components ((:file "example")))
