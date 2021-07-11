;; generated from .org
;;(declaim (optimize (speed 3) (safety 0)))
(uiop:define-package :signal-handler/example
    (:shadow cl:log)
    (:export :main)
    (:use :cl :shalaev/macros :shalaev/files))
