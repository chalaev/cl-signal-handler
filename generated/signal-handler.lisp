;; generated from signal-handler.org
;;(declaim (optimize (speed 3) (safety 0)))
(defpackage :signal-handler
    ;;(:nicknames :SH)
    (:nicknames "SH")
    (:use :cl)
    (:export :register :forget :forget-all :lock-dir :start))
(in-package :signal-handler)

(eval-when (:compile-toplevel :load-toplevel);  :execute
(let ((goodies/ (uiop:ensure-directory-pathname "goodies")))
(mapcar #'(lambda(FN) (load (merge-pathnames FN goodies/)))
'(#p"macros.lisp" #p"functions.lisp" #p"file-functions.lisp"))))

(defvar *started* nil)
(defvar lock-dir (ensure-directories-exist

(uiop:ensure-directory-pathname (merge-pathnames #p"sbcl.lock" (uiop:temporary-directory))))
"common directory for all lockers")
(defvar hooks nil)
(defvar *clients* nil)

(defun log-SH (type &rest message)
  (apply #'sl:log
    (cons type (cons (concat "SH " (car message)) (cdr message)))))

(cffi:defcstruct saType ; typedef for sigaction
  (handler :pointer)
  (sigaction :pointer); unused
  (mask  :unsigned-long)
  (flags  :int))

(cffi:defcallback sighandler :void ((signum :int) (info :pointer) (ptr :pointer))
  (declare (integer signum))
  (when (= 10 signum)
(needs ((active-hooks (remove-if-not #'(lambda(h) (directory (merge-paths lock-dir (car h)))) hooks) (log-SH sl:debug "hook not found")))
  (dolist (hook active-hooks)
    (let ((hf (cdr hook)) (FN (merge-paths lock-dir (car hook) "by")))

(log-SH sl:info "found the directory ~a, will now read the file" (car hook))
(with-open-file (s FN :if-does-not-exist :create)
  (ifn-let ((mes (read-line s nil))) (log-SH sl:warning "could not find the file ~a" FN)
    (funcall hf mes)
    (log-SH sl:debug "received the message ~s, erasing the file ~a" mes FN)))
(delete-file FN))))))

(defun start()
(iff *started* (log-SH sl:debug "already started, won't start for the second time")
(cffi:with-foreign-object (act '(:struct saType))
  (setf (cffi:foreign-slot-value act '(:struct saType) 'handler) (cffi:callback sighandler))
  (setf (cffi:foreign-slot-value act '(:struct saType) 'mask) iolib.syscalls:sa-siginfo)
  (iolib.syscalls:sigaction iolib.syscalls:sigusr1 act (cffi-sys:null-pointer)))
  (log-SH sl:debug "Started, now waiting for the kill signals.")
(setf *started* t)))

(defun register (name hook-function)

(declare (string name))
(unless *started* (start))
(log-SH sl:debug "registering the hook ~s" name)
(if (member name (mapcar #'car hooks))
 (log-SH sl:error "I have already registered the hook for ~s, please change the name or forget" name)
 (push (cons name hook-function) hooks)))

(defun forget (name)
  (drop-if name hooks :key #'car :test #'string=))
(defun forget-all()
  (setf hooks nil))
