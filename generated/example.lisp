(defun logE (type &rest message)
    (apply #'sl:log
      (cons type (cons (concat "SHE " (car message)) (cdr message)))))

(defvar working nil)
(defvar stop-the-server (bt:make-condition-variable))
(defun stop() 
  (setf working nil)
  (bt:condition-notify stop-the-server))

(defun wachter(message)
  "An example kill-signal handler. Its argument is a message from the killer."
(logE sl:info "my killer told me: ~s" message)
(when (string= message "bye")
  (logE sl:info "stopping the server")
  (stop))
 T)

(defun main()
(setf sl:out-streams (list *standard-output*))
(setf working t)
(sl:start); starting the log server
(logE sl:info "starting kill-signal server")
(sh:start (make-pathname :directory '(:absolute "tmp")  :name "sbcl" :type "lock"))

(sh:register "acceptor" #'wachter)
(logE sl:info "will accept messages until someone tells me \"bye\"")

(loop while working do
(let((SL-lock(bt:make-lock)))
  (bt:with-lock-held(SL-lock)
  (bt:condition-wait stop-the-server SL-lock)
  (logE sl:debug "unlocked"))))

(logE sl:info "stopping the server")
(sh:stop); stopping signal handler
(sl:stop)); stopping log server after flusshing remaining log messages
