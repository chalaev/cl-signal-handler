(in-package :signal-handler/example)
(defun log (type &rest message)
    (apply #'sl:log
      (cons type (cons (concat "SHE " (car message)) (cdr message)))))

(defvar working nil)
(defvar stop-the-server (bt:make-condition-variable))
(defun stop() 
  (setf working nil)
  (bt:condition-notify stop-the-server))

(defun wachter(request-directory)
  "An example kill-signal handler. Its argument is a message from the killer."
(let((FN(merge-pathnames "by" request-directory)) (work-time(/ (random 50) 100.0)))
  (with-open-file(s FN)
    (ifn-let((message(read-line s nil))) (log sl:error "could not find the file ~a" FN)
       (iff(string= message "bye") (progn (delete-file FN) (stop))
	  (log sl:info "my killer told me: ~s, now let me digest that..." message)
	  (sleep work-time)
	  (log sl:info "I worked hard for ~A seconds, and now I am ready for more requests" work-time)
	  (delete-file FN))))))

(defun main()
(setf sl:out-streams (list *standard-output*))
(setf working t)
(sl:start); starting the log server
(log sl:info "starting kill-signal server...")
(sh:start (make-pathname :directory '(:absolute "tmp"); ← have to use /tmp because cannot write to /var/lock
:name "sbcl" :type "lock"))

(sh:register "acceptor" #'wachter)
(log sl:info "will accept messages until someone tells me \"bye\"")

(loop while working do
(let((SL-lock(bt:make-lock)))
  (bt:with-lock-held(SL-lock)
  (bt:condition-wait stop-the-server SL-lock)
  (log sl:debug "unlocked"))))

(log sl:info "stopping the server")
(sh:stop); stopping signal handler
(sl:stop)); stopping log server after flusshing remaining log messages
