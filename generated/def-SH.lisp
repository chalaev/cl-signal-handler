;;(declaim (optimize (speed 3) (safety 0)))
(uiop:define-package :signal-handler
    (:nicknames "SH") ;(:nicknames :SH)
    (:shadow cl:log)
    (:use :cl :shalaev/macros :shalaev/files)
    (:export :register :forget :forget-all :lock-dir :start :stop))
