(defvar *started* nil)
(defvar hooks nil)
(defvar *clients* nil)

(defun log(type format-str &rest params)
  (apply #'sl:log (cons type (cons (concat "SH " format-str) params))))

(cffi:defcstruct saType ; typedef for sigaction
  (handler :pointer)
  (sigaction :pointer); unused
  (mask  :unsigned-long)
  (flags  :int))

(defvar lock-dir nil)
(cffi:defcallback sighandler :void ((signum :int) (info :pointer) (ptr :pointer))
  (declare(integer signum))
;;(ifn lock-dir (log sl:error "lock-dir is undefined")
  (when (= 10 signum)
    (needs ((active-hooks (remove-if-not #'(lambda(h) (directory (merge-paths lock-dir (car h)))) hooks) (log sl:debug "hook not found")))
	   (dolist (hook active-hooks)
	     (let ((hf (cdr hook)) (FN (merge-paths lock-dir (car hook) "by")))

(log sl:debug "found the directory ~a, will now read the file" (car hook))
(with-open-file(s FN :if-does-not-exist :create)
  (ifn-let((mes(read-line s nil))) (log sl:error "could not find the file ~a" FN)
    (funcall hf mes)
    (log sl:debug "received the message ~s, erasing the file ~a" mes FN)))
(delete-file FN))))))

(defun pid-FN()
(ifn lock-dir (log sl:error "pid-FN: undefined lock-dir")
(merge-paths lock-dir "pid")))
(defun start(&optional (root-directory (uiop:temporary-directory)))
(iff *started* (log sl:debug "sigkill handler already started")
(setf lock-dir (ensure-directories-exist (uiop:ensure-directory-pathname root-directory)))
(cffi:with-foreign-object (act '(:struct saType))
  (setf (cffi:foreign-slot-value act '(:struct saType) 'handler) (cffi:callback sighandler))
  (setf (cffi:foreign-slot-value act '(:struct saType) 'mask) iolib.syscalls:sa-siginfo)
  (iolib.syscalls:sigaction iolib.syscalls:sigusr1 act (cffi-sys:null-pointer)))

(echo-to-file (pid-FN) (format nil "~d ok" (sb-posix:getpid))))
(log sl:info ":signal-handler is now ready to treat kill signals.")
(setf *started* t))
(defun stop()
(delete-file (pid-FN))
(log sl:info "erased ~a" (pid-FN))
(setf *started* nil))

(defun register (name hook-function)

(declare (string name))
(if(member name (mapcar #'car hooks))
 (log sl:error "hook for ~s is already registered" name)
 (push (cons name hook-function) hooks)))

(defun forget(name)
  (drop-if name hooks :key #'car :test #'string=))
(defun forget-all()
  (setf hooks nil))
