(defun logE (type &rest message)
    (apply #'sl:log
      (cons type (cons (concat "SHE " (car message)) (cdr message)))))

(defun wachter(message)
  "An example kill-signal handler. Its argument is a message from the killer."
  (logE sl:info "my killer told me: ~s" message) T)

(defvar time-interval 3)
(defun main()
(sh:start (merge-pathnames (make-pathname :directory "tmp")  "sbcl.lock"))

(sh:register "acceptor" #'wachter)
(logE sl:debug "started simple-handler service")

(setf sl:out-streams (list *standard-output*))
(logE sl:info "will accept messages for the next ~d seconds" time-interval)
(sleep time-interval)
(logE sl:info "time is up, hook is removed, messages are ignored from now")
(sleep 1)
(sh:stop))
