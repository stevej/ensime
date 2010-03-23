;;; ensime.el --- ENhanced Scala Interaction Mode for Emacs
;;

(eval-and-compile
  (when (<= emacs-major-version 21)
    (error "Ensime requires an Emacs version of 21, or above")))
(require 'thingatpt)
(require 'comint)
(require 'timer)
(require 'pp)
(require 'hideshow)
(require 'font-lock)
(require 'easymenu)
(eval-when (compile)
  (require 'arc-mode)
  (require 'apropos)
  (require 'outline)
  (require 'etags)
  (require 'compile))

(eval-and-compile 
  (defvar ensime-path
    (let ((path (or (locate-library "ensime") load-file-name)))
      (and path (file-name-directory path)))
    "Directory containing the Ensime package.
     This is used to load the supporting Scala server."))

(defvar ensime-protocol-version nil)


(defgroup ensime-ui nil
  "Interaction with the ENhanced Scala Environment."
  :prefix "ensime-"
  :group 'ensime)

(defcustom ensime-kill-without-query-p nil
  "If non-nil, kill ENSIME processes without query when quitting Emacs."
  :type 'boolean
  :group 'ensime-ui)


(defgroup ensime-server nil
  "Server configuration."
  :prefix "ensime-"
  :group 'ensime)

(defcustom ensime-connected-hook nil
  "List of functions to call when ENSIME connects to Lisp."
  :type 'hook
  :group 'ensime-server)

(defcustom ensime-server-host "127.0.0.1"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'ensime-server)

(defcustom ensime-port 9999
  "Port to use as the default for `ensime-connect'."
  :type 'integer
  :group 'ensime-server)


(defvar ensime-protocol-version "0.0.1")

(defvar ensime-mode-indirect-map (make-sparse-keymap)
  "Empty keymap which has `ensime-mode-map' as it's parent.
This is a hack so that we can reinitilize the real ensime-mode-map
more easily. See `ensime-init-keymaps'.")

(define-minor-mode ensime-mode
  "\\<ensime-mode-map>\
ENSIME: The ENhanced Scala Interaction Mode for Emacs (minor-mode).

Full set of commands:
\\{ensime-mode-map}"
  nil
  nil
  ensime-mode-indirect-map
  (ensime-recompute-modelines))

(defun ensime-recompute-modelines ()
  (force-mode-line-update t))

;;;;;; Modeline

(add-to-list 'minor-mode-alist
             `(ensime-mode ,'(ensime-modeline-string)))


(defun ensime-modeline-string ()
  "Return the string to display in the modeline.
\"Ensime\" only appears if we aren't connected.  If connected,
include package-name, connection-name, and possibly some state
information."
  "Ensime")


;;;; Framework'ey bits
;;;
;;; This section contains some standard ENSIME idioms: basic macros,
;;; ways of showing messages to the user, etc. All the code in this
;;; file should use these functions when applicable.
;;;
;;;;; Syntactic sugar

(defmacro* when-let ((var value) &rest body)
  "Evaluate VALUE, if the result is non-nil bind it to VAR and eval BODY.

\(fn (VAR VALUE) &rest BODY)"
  `(let ((,var ,value))
     (when ,var ,@body)))


(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
	 ,@(mapcar (lambda (clause)
                     (if (eq (car clause) t)
                         `(t ,@(cdr clause))
                       (destructuring-bind ((op &rest rands) &rest body) clause
                         `(,op (destructuring-bind ,rands ,operands
                                 . ,body)))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "Elisp destructure-case failed: %S" ,tmp))))))))


(defmacro ensime-define-keys (keymap &rest key-command)
  "Define keys in KEYMAP. Each KEY-COMMAND is a list of (KEY COMMAND)."
  `(progn . ,(mapcar (lambda (k-c) `(define-key ,keymap . ,k-c))
		     key-command)))


(defmacro* with-struct ((conc-name &rest slots) struct &body body)
  "Like with-slots but works only for structs.
\(fn (CONC-NAME &rest SLOTS) STRUCT &body BODY)"
  (flet ((reader (slot) (intern (concat (symbol-name conc-name)
					(symbol-name slot)))))
    (let ((struct-var (gensym "struct")))
      `(let ((,struct-var ,struct))
	 (symbol-macrolet
	     ,(mapcar (lambda (slot)
			(etypecase slot
			  (symbol `(,slot (,(reader slot) ,struct-var)))
			  (cons `(,(first slot) (,(reader (second slot)) 
						 ,struct-var)))))
		      slots)
	   . ,body)))))


(defvar ensime-net-processes nil
  "List of processes (sockets) connected to Lisps.")

(defvar ensime-net-process-close-hooks '()
  "List of functions called when a ensime network connection closes.
The functions are called with the process as their argument.")


(defun ensime-net-connect (host port)
  "Establish a connection with a CL."
  (let* ((inhibit-quit nil)
         (proc (open-network-stream "ENSIME Scala" nil host port))
         (buffer (ensime-make-net-buffer " *ensime-connection*")))
    (push proc ensime-net-processes)
    (set-process-buffer proc buffer)
    (set-process-filter proc 'ensime-net-filter)
    (set-process-sentinel proc 'ensime-net-sentinel)
    (ensime-set-query-on-exit-flag proc)

    ;; TODO make this smart like slime?
    (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)

    proc))

(defun ensime-set-query-on-exit-flag (process)
  "Set PROCESS's query-on-exit-flag to `ensime-kill-without-query-p'."
  (when ensime-kill-without-query-p
    ;; avoid byte-compiler warnings
    (let ((fun (if (fboundp 'set-process-query-on-exit-flag)
                   'set-process-query-on-exit-flag
                 'process-kill-without-query)))
      (funcall fun process nil))))

(defun ensime-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (set (make-local-variable 'kill-buffer-query-functions) nil))
    buffer))

(defun ensime-net-send (sexp proc)
  "Send a SEXP to Lisp over the socket PROC.
   This is the lowest level of communication. The sexp will be read and interpreted
   by the Ensime Server."
  (let* ((msg (concat (ensime-prin1-to-string sexp) "\n"))
         (string (concat (ensime-net-encode-length (length msg)) msg))
         (coding-system (cdr (process-coding-system proc))))
    (ensime-log-event sexp)
    (process-send-string proc string)))

(defun ensime-net-close (process &optional debug)
  (setq ensime-net-processes (remove process ensime-net-processes))
  (when (eq process ensime-default-connection)
    (setq ensime-default-connection nil))
  (cond (debug         
         (set-process-sentinel process 'ignore)
         (set-process-filter process 'ignore)
         (delete-process process))
        (t
         (run-hook-with-args 'ensime-net-process-close-hooks process)
         ;; killing the buffer also closes the socket
         (kill-buffer (process-buffer process)))))

(defun ensime-net-sentinel (process message)
  (message "Server connection closed unexpectedly: %s" message)
  (ensime-net-close process))

;;; Socket input is handled by `ensime-net-filter', which decodes any
;;; complete messages and hands them off to the event dispatcher.

(defun ensime-net-filter (process string)
  "Accept output from the socket and process all complete messages."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (ensime-process-available-input process))

(defun ensime-process-available-input (process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (ensime-net-have-input-p)
      (let ((event (ensime-net-read-or-lose process))
            (ok nil))
        (ensime-log-event event)
        (unwind-protect
            (save-current-buffer
              (ensime-dispatch-event event process)
              (setq ok t))
          (unless ok
            (ensime-run-when-idle 'ensime-process-available-input process)))))))

(defun ensime-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (ensime-net-decode-length))))

(defun ensime-run-when-idle (function &rest args)
  "Call FUNCTION as soon as Emacs is idle."
  (apply #'run-at-time 
         (if (featurep 'xemacs) itimer-short-interval 0) 
         nil function args))

(defun ensime-net-read-or-lose (process)
  (condition-case error
      (ensime-net-read)
    (error
     (debug 'error error)
     (ensime-net-close process t)
     (error "net-read error: %S" error))))

(defun ensime-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (ensime-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length)))
    (assert (plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (current-buffer)))
      (delete-region (point-min) end))))

(defun ensime-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun ensime-net-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

(defun ensime-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length 
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))



;;;;; Event logging to *ensime-events*
;;;
;;; The *ensime-events* buffer logs all protocol messages for debugging
;;; purposes. Optionally you can enable outline-mode in that buffer,
;;; which is convenient but slows things down significantly.

(defvar ensime-log-events t
  "*Log protocol events to the *ensime-events* buffer.")

(defvar ensime-outline-mode-in-events-buffer nil
  "*Non-nil means use outline-mode in *ensime-events*.")

(defvar ensime-event-buffer-name "*ensime-events*"
  "The name of the ensime event buffer.")

(defun ensime-log-event (event)
  "Record the fact that EVENT occurred."
  (when ensime-log-events
    (with-current-buffer (ensime-events-buffer)
      ;; trim?
      (when (> (buffer-size) 100000)
        (goto-char (/ (buffer-size) 2))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (save-excursion
        (ensime-pprint-event event (current-buffer)))
      (when (and (boundp 'outline-minor-mode)
                 outline-minor-mode)
        (hide-entry))
      (goto-char (point-max)))))

(defun ensime-pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(defun ensime-events-buffer ()
  "Return or create the event log buffer."
  (or (get-buffer ensime-event-buffer-name)
      (let ((buffer (get-buffer-create ensime-event-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (set (make-local-variable 'outline-regexp) "^(")
          (set (make-local-variable 'comment-start) ";")
          (set (make-local-variable 'comment-end) "")
          (when ensime-outline-mode-in-events-buffer
            (outline-minor-mode)))
        buffer)))



;;; Connection-local variables:

(defmacro ensime-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `ensime-connection'."
  (let ((real-var (intern (format "%s:connlocal" varname))))
    `(progn
       ;; Variable
       (make-variable-buffer-local
        (defvar ,real-var ,@initial-value-and-doc))
       ;; Accessor
       (defun ,varname (&optional process)
         (ensime-with-connection-buffer (process) ,real-var))
       ;; Setf
       (defsetf ,varname (&optional process) (store)
         `(ensime-with-connection-buffer (,process)
					 (setq (\, (quote (\, real-var))) (\, store))
					 (\, store)))
       '(\, varname))))

(put 'ensime-def-connection-var 'lisp-indent-function 2)
(put 'ensime-indulge-pretty-colors 'ensime-def-connection-var t)

(ensime-def-connection-var ensime-connection-number nil
  "Serial number of a connection.
Bound in the connection's process-buffer.")

(ensime-def-connection-var ensime-server-features '()
  "The symbol-names of Lisp's *FEATURES*.
This is automatically synchronized from Lisp.")

(ensime-def-connection-var ensime-pid nil
  "The process id of the Lisp process.")

(ensime-def-connection-var ensime-server-implementation-type nil
  "The implementation type of the Lisp process.")

(ensime-def-connection-var ensime-server-implementation-version nil
  "The implementation type of the Lisp process.")

(ensime-def-connection-var ensime-server-implementation-name nil
  "The short name for the Lisp implementation.")

(ensime-def-connection-var ensime-server-implementation-program nil
  "The argv[0] of the process running the Lisp implementation.")

(ensime-def-connection-var ensime-connection-name nil
  "The short name for connection.")

(ensime-def-connection-var ensime-inferior-process nil
  "The inferior process for the connection if any.")

(ensime-def-connection-var ensime-communication-style nil
  "The communication style.")

(ensime-def-connection-var ensime-machine-instance nil
  "The name of the (remote) machine running the Lisp process.")


;;;;; Connection setup

(defvar ensime-dispatching-connection nil
  "Network process currently executing.
This is dynamically bound while handling messages from Lisp; it
overrides `ensime-buffer-connection' and `ensime-default-connection'.")

(make-variable-buffer-local
 (defvar ensime-buffer-connection nil
   "Network connection to use in the current buffer.
This overrides `ensime-default-connection'."))

(defvar ensime-default-connection nil
  "Network connection to use by default.
Used for all Lisp communication, except when overridden by
`ensime-dispatching-connection' or `ensime-buffer-connection'.")


(defvar ensime-connection-counter 0
  "The number of ENSIME connections made. For generating serial numbers.")

(defun ensime-current-connection ()
  "Return the connection to use for Lisp interaction.
Return nil if there's no connection."
  (or ensime-dispatching-connection
      ensime-buffer-connection
      ensime-default-connection))

(defun ensime-connection ()
  "Return the connection to use for Lisp interaction.
   Signal an error if there's no connection."
  (let ((conn (ensime-current-connection)))
    (cond ((and (not conn) ensime-net-processes)
           (or (ensime-auto-select-connection)
               (error "No default connection selected.")))
          ((not conn)
           (or (ensime-auto-connect)
               (error "Not connected.")))
          ((not (eq (process-status conn) 'open))
           (error "Connection closed."))
          (t conn))))

(defun ensime-setup-connection (process)
  "Make a connection out of PROCESS."
  (let ((ensime-dispatching-connection process))
    (ensime-init-connection-state process)
    (ensime-select-connection process)
    process))

(defun ensime-select-connection (process)
  "Make PROCESS the default connection."
  (setq ensime-default-connection process))

(defmacro* ensime-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `ensime-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  `(with-current-buffer
       (process-buffer (or ,process (ensime-connection)
                           (error "No connection")))
     ,@body))

(defun ensime-connect (host port)
  "Connect to a running Swank server. Return the connection."
  (interactive (list (read-from-minibuffer "Host: " ensime-server-host)
                     (read-from-minibuffer "Port: " (format "%d" ensime-port)
                                           nil t)))
  (when (and (interactive-p) ensime-net-processes
             (y-or-n-p "Close old connections first? "))
    (ensime-disconnect-all))
  (message "Connecting to Swank on port %S.." port)
  (let ()
    (message "Connecting to Swank on port %S.." port)
    (let* ((process (ensime-net-connect host port))
           (ensime-dispatching-connection process))
      (ensime-setup-connection process))))


(defun ensime-init-connection-state (proc)
  "Initialize connection state in the process-buffer of PROC."
  ;; To make life simpler for the user: if this is the only open
  ;; connection then reset the connection counter.
  (when (equal ensime-net-processes (list proc))
    (setq ensime-connection-counter 0))
  (ensime-with-connection-buffer 
   () (setq ensime-buffer-connection proc))
  (setf (ensime-connection-number proc) (incf ensime-connection-counter))
  ;; We do the rest of our initialization asynchronously. The current
  ;; function may be called from a timer, and if we setup the REPL
  ;; from a timer then it mysteriously uses the wrong keymap for the
  ;; first command.
  (ensime-eval-async '(swank:connection-info)
		     (ensime-curry #'ensime-set-connection-info proc)))


(defun ensime-set-connection-info (connection info)
  "Initialize CONNECTION with INFO received from Lisp."
  (let ((ensime-dispatching-connection connection))
    (destructuring-bind (&key pid style server-implementation machine
                              features package version modules
                              &allow-other-keys) info
      (ensime-check-version version connection)
      (setf (ensime-pid) pid
            (ensime-communication-style) style
            (ensime-server-features) features)
      (destructuring-bind (&key type name version program) server-implementation
        (setf (ensime-server-implementation-type) type
              (ensime-server-implementation-version) version
              (ensime-server-implementation-name) name
              (ensime-server-implementation-program) program
              (ensime-connection-name) (ensime-generate-connection-name name)))
      (destructuring-bind (&key instance type version) machine
        (setf (ensime-machine-instance) instance)))
    (let ((args (when-let (p (ensime-inferior-process))
                  (ensime-inferior-lisp-args p))))
      (when-let (name (plist-get args ':name))
        (unless (string= (ensime-server-implementation-name) name)
          (setf (ensime-connection-name)
                (ensime-generate-connection-name (symbol-name name)))))
      ;; TODO
      ;;(ensime-load-contribs)
      (run-hooks 'ensime-connected-hook)
      (when-let (fun (plist-get args ':init-function))
        (funcall fun)))
    (message "Connected. %s" (ensime-random-words-of-encouragement))))


(defun ensime-check-version (version conn)
  (or (equal version ensime-protocol-version)
      (equal ensime-protocol-version 'ignore)
      (y-or-n-p 
       (format "Versions differ: %s (ensime) vs. %s (swank). Continue? "
               ensime-protocol-version version))
      (ensime-net-close conn)
      (top-level)))

(defun ensime-generate-connection-name (server-name)
  (loop for i from 1
        for name = server-name then (format "%s<%d>" server-name i)
        while (find name ensime-net-processes 
                    :key #'ensime-connection-name :test #'equal)
        finally (return name)))

(defun ensime-connection-close-hook (process)
  (when (eq process ensime-default-connection)
    (when ensime-net-processes
      (ensime-select-connection (car ensime-net-processes))
      (message "Default connection closed; switched to #%S (%S)"
               (ensime-connection-number)
               (ensime-connection-name)))))

(add-hook 'ensime-net-process-close-hooks 'ensime-connection-close-hook)



;;; `ensime-rex' is the RPC primitive which is used to implement both
;;; `ensime-eval' and `ensime-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(defmacro* ensime-rex ((&rest saved-vars)
		       (sexp &optional 
			     (package '(ensime-current-package)))
		       &rest continuations)
  "(ensime-rex (VAR ...) (SEXP &optional PACKAGE) CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

PACKAGE is evaluated and Lisp binds *BUFFER-PACKAGE* to this package.
The default value is (ensime-current-package).

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (let ((result (gensym)))
    `(lexical-let ,(loop for var in saved-vars
                         collect (etypecase var
                                   (symbol (list var var))
                                   (cons var)))
       (ensime-dispatch-event 
        (list :emacs-rex ,sexp ,package
              (lambda (,result)
                (destructure-case ,result
                  ,@continuations)))))))

(put 'ensime-rex 'lisp-indent-function 2)

;;; Interface
(defun ensime-current-package ()
  "Return the Common Lisp package in the current context.
If `ensime-buffer-package' has a value then return that, otherwise
search for and read an `in-package' form."
  "mmmmmhhhh?")


(defun ensime-eval-async (sexp &optional cont package)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (ensime-rex (cont (buffer (current-buffer)))
      (sexp (or package (ensime-current-package)))
    ((:ok result)
     (when cont
       (set-buffer buffer)
       (funcall cont result)))
    ((:abort)
     (message "Evaluation aborted.")))
  ;; Guard against arbitrary return values which once upon a time
  ;; showed up in the minibuffer spuriously (due to a bug in
  ;; ensime-autodoc.)  If this ever happens again, returning the
  ;; following will make debugging much easier:
  :ensime-eval-async)

;;;;; Commands on connections

(defun ensime-disconnect ()
  "Close the current connection."
  (interactive)
  (ensime-net-close (ensime-connection)))

(defun ensime-disconnect-all ()
  "Disconnect all connections."
  (interactive)
  (mapc #'ensime-net-close ensime-net-processes))

(defun ensime-connection-port (connection)
  "Return the remote port number of CONNECTION."
  (if (featurep 'xemacs)
      (car (process-id connection))
    (cadr (process-contact connection))))

(defun ensime-process (&optional connection)
  "Return the Lisp process for CONNECTION (default `ensime-connection').
Return nil if there's no process object for the connection."
  (let ((proc (ensime-inferior-process connection)))
    (if (and proc 
             (memq (process-status proc) '(run stop)))
        proc)))


;; Commonly used functions

(defun ensime-curry (fun &rest args)
  "Partially apply FUN to ARGS.  The result is a new function.
This idiom is preferred over `lexical-let'."
  `(lambda (&rest more) (apply ',fun (append ',args more))))

(defun ensime-rcurry (fun &rest args)
  "Like `ensime-curry' but ARGS on the right are applied."
  `(lambda (&rest more) (apply ',fun (append more ',args))))


;;;;; Protocol event handler (the guts)
;;;
;;; This is the protocol in all its glory. The input to this function
;;; is a protocol event that either originates within Emacs or arrived
;;; over the network from Lisp.
;;;
;;; Each event is a list beginning with a keyword and followed by
;;; arguments. The keyword identifies the type of event. Events
;;; originating from Emacs have names starting with :emacs- and events
;;; from Lisp don't.

(ensime-def-connection-var ensime-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(ensime-def-connection-var ensime-continuation-counter 0
  "Continuation serial number counter.")

(defvar ensime-event-hooks)

(defun ensime-dispatch-event (event &optional process)
  (let ((ensime-dispatching-connection (or process (ensime-connection))))
    (or (run-hook-with-args-until-success 'ensime-event-hooks event)
        (destructure-case event
          ((:emacs-rex form package continuation)
           (let ((id (incf (ensime-continuation-counter))))
             (ensime-send `(:emacs-rex ,form ,package ,id))
             (push (cons id continuation) (ensime-rex-continuations))
             (ensime-recompute-modelines)))
          ((:return value id)
           (let ((rec (assq id (ensime-rex-continuations))))
             (cond (rec (setf (ensime-rex-continuations)
                              (remove rec (ensime-rex-continuations)))
                        (ensime-recompute-modelines)
                        (funcall (cdr rec) value))
                   (t
                    (error "Unexpected reply: %S %S" id value)))))
          ((:compilation-result result)
           (ensime-compilation-finished result))
          ((:debug-activate thread level &optional select)
           (assert thread)
           (sldb-activate thread level select))
          ((:debug thread level condition restarts frames conts)
           (assert thread)
           (sldb-setup thread level condition restarts frames conts))
          ((:debug-return thread level stepping)
           (assert thread)
           (sldb-exit thread level stepping))
          ((:emacs-interrupt thread)
           (ensime-send `(:emacs-interrupt ,thread)))
          ((:channel-send id msg)
           (ensime-channel-send (or (ensime-find-channel id)
				    (error "Invalid channel id: %S %S" id msg))
				msg))
          ((:emacs-channel-send id msg)
           (ensime-send `(:emacs-channel-send ,id ,msg)))
          ((:read-from-minibuffer thread tag prompt initial-value)
           (ensime-read-from-minibuffer-for-swank thread tag prompt initial-value))
          ((:y-or-n-p thread tag question)
           (ensime-y-or-n-p thread tag question))
          ((:emacs-return-string thread tag string)
           (ensime-send `(:emacs-return-string ,thread ,tag ,string)))
          ((:new-features features)
           (setf (ensime-server-features) features))
          ((:indentation-update info)
           (ensime-handle-indentation-update info))
          ((:eval-no-wait fun args)
           (apply (intern fun) args))
          ((:eval thread tag form-string)
           (ensime-check-eval-in-emacs-enabled)
           (ensime-eval-for-lisp thread tag form-string))
          ((:emacs-return thread tag value)
           (ensime-send `(:emacs-return ,thread ,tag ,value)))
          ((:ed what)
           (ensime-ed what))
          ((:inspect what wait-thread wait-tag)
           (let ((hook (when (and wait-thread wait-tag)
                         (lexical-let ((thread wait-thread)
                                       (tag wait-tag))
                           (lambda ()
                             (ensime-send `(:emacs-return ,thread ,tag nil)))))))
             (ensime-open-inspector what nil hook)))
          ((:background-message message)
           (ensime-background-message "%s" message))
          ((:debug-condition thread message)
           (assert thread)
           (message "%s" message))
          ((:ping thread tag)
           (ensime-send `(:emacs-pong ,thread ,tag)))
          ((:reader-error packet condition)
           (ensime-with-popup-buffer ("*Ensime Error*")
				     (princ (format "Invalid protocol message:\n%s\n\n%S"
						    condition packet))
				     (goto-char (point-min)))
           (error "Invalid protocol message"))
          ((:invalid-rpc id message)
           (setf (ensime-rex-continuations)
                 (remove* id (ensime-rex-continuations) :key #'car))
           (error "Invalid rpc: %s" message))))))

(defun ensime-send (sexp)
  "Send SEXP directly over the wire on the current connection."
  (ensime-net-send sexp (ensime-connection)))



;;; Words of encouragement

(defun ensime-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar ensime-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the source be with you!"
    "Take this REPL, brother, and may it serve you well."
    "Lemonodor-fame is but a hack away!"
    ,(format "%s, this could be the start of a beautiful program."
             (ensime-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun ensime-random-words-of-encouragement ()
  "Return a string of hackerish encouragement."
  (eval (nth (random (length ensime-words-of-encouragement))
             ensime-words-of-encouragement)))


;; Compiler notes

(defvar ensime-note-overlays '())

(defun ensime-compilation-finished (result)
  (ensime-remove-old-overlays)
  (destructuring-bind (&key notes &allow-other-keys) result
    (dolist (note notes)
      (destructuring-bind 
	  (&key severity msg beg end line col file &allow-other-keys) note
	(when-let (buf (find-buffer-visiting file))
	  (switch-to-buffer buf)
	  (save-excursion
	    (goto-line line)
	    (let* ((start (point-at-bol))
		   (stop (point-at-eol))
		   (ov (cond ((equal severity 'error)
			      (ensime-make-overlay
			       start stop msg 'ensime-errline nil))

			     ((equal severity 'warn)
			      (ensime-make-overlay
			       start stop msg 'ensime-warnline nil))
			     )))
	      (push ov ensime-note-overlays)
	      )))
	)
      )
    )
  )

(defface ensime-errline
  '((((class color) (background dark)) (:background "Firebrick4"))
    (((class color) (background light)) (:background "LightPink"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'ensime-ui)

(defface ensime-warnline
  '((((class color) (background dark)) (:background "DarkBlue"))
    (((class color) (background light)) (:background "LightBlue2"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'ensime-ui)

(defun ensime-make-overlay (beg end tooltip-text face mouse-face)
  "Allocate a ensime overlay in range BEG and END."
  (let ((ov (make-overlay beg end nil t t)))
    (overlay-put ov 'face           face)
    (overlay-put ov 'mouse-face     mouse-face)
    (overlay-put ov 'help-echo      tooltip-text)
    (overlay-put ov 'ensime-overlay  t)
    (overlay-put ov 'priority 100)
    ov)
  )

(defun ensime-remove-old-overlays ()
  "Delete the existing note overlays."
  (mapc #'delete-overlay ensime-note-overlays)
  (setq ensime-note-overlays '()))



;; Test compile current file

(defun ensime-compile-current-file ()
  (interactive)
  (ensime-eval-async `(swank:compile-file ,buffer-file-name) #'identity))

(provide 'ensime)