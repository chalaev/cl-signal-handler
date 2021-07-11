(in-package :signal-handler)
(defvar hooks nil)

(defun log(type format-str &rest params)
  (apply #'sl:log (cons type (cons (concat "SH " format-str) params))))

(cffi:defcstruct saType ; typedef for sigaction
  (handler :pointer)
  (sigaction :pointer); unused
  (mask  :unsigned-long)
  (flags  :int))

(let(started lock-dir)
(flet((pid-FN ()
  (ifn lock-dir (log sl:error "pid-FN: undefined lock-dir")
     (merge-pathnames "pid" lock-dir))))
(defun start(&optional (root-directory (uiop:temporary-directory)))
(setf lock-dir (uiop:ensure-directory-pathname root-directory))
(ensure-directories-exist lock-dir)
;;(log sl:debug "lock-dir= ~S" lock-dir); lock-dir= #P"/tmp/sbcl.lock/"
(iff started (log sl:warning "will not restart already active sigkill handler")

(cffi:defcallback sighandler :void ((signum :int) (info :pointer) (ptr :pointer))
  (declare(integer signum))
  (when (= 10 signum)
    (dolist(hook hooks)
      (let((hook-dir(uiop:ensure-directory-pathname(merge-pathnames (car hook) lock-dir))))
        (when(uiop:directory-exists-p hook-dir)
          (funcall (cdr hook) hook-dir)
	  T))))); must return T

(cffi:with-foreign-object (act '(:struct saType))
  (setf (cffi:foreign-slot-value act '(:struct saType) 'handler) (cffi:callback sighandler))
  (setf (cffi:foreign-slot-value act '(:struct saType) 'mask) iolib.syscalls:sa-siginfo)
  (iolib.syscalls:sigaction iolib.syscalls:sigusr1 act (cffi-sys:null-pointer)))

(echo-to-file (pid-FN) (format nil "~d ok" (sb-posix:getpid)))
(log sl:info ":signal-handler is now ready to treat kill signals.")
(setf started t)))
(defun stop()
  (delete-file (pid-FN))
  (log sl:info "erased ~a" (pid-FN)))))

(defun register (name hook-function)
(declare (string name))

(if(find name hooks :key  #'car)
 (log sl:warning "will not re-register already active hook for ~s" name)
 (push (cons name hook-function) hooks)))

(defun forget(name)
  (drop-if name hooks :key #'car :test #'string=))
(defun forget-all()
  (setf hooks nil))
