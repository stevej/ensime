;;; ensime.el --- ENhanced Scala Interaction Mode for Emacs
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This file includes code from slime.el of the SLIME project
;;     (also licensend under the GNU General Public License.) The
;;     following copyrights therefore apply:
;;     
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;     Copyright (C) 2007,2008,2009  Helmut Eller, Tobias C. Rittweiler
;;
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


(eval-and-compile
  (when (<= emacs-major-version 21)
    (error "Ensime requires an Emacs version of 21, or above")))

(eval-and-compile
  (require 'cl))

(require 'thingatpt)
(require 'comint)
(require 'timer)
(require 'tooltip)
(require 'pp)
(require 'hideshow)
(require 'font-lock)
(require 'auto-complete)
(require 'auto-complete-ensime)
(require 'ido)
(eval-when (compile)
  (require 'apropos)
  (require 'compile))


(defgroup ensime-ui nil
  "Interaction with the ENhanced Scala Environment."
  :prefix "ensime-"
  :group 'ensime)

(defcustom ensime-truncate-lines t
  "Set `truncate-lines' in popup buffers.
  This applies to buffers that present lines as rows of data, such as
  debugger backtraces and apropos listings."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-kill-without-query-p nil
  "If non-nil, kill ENSIME processes without query when quitting Emacs."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-tooltip-hints t
  "If non-nil, mouse tooltips are activated."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-tooltip-type-hints t
  "If non-nil, type-inspecting tooltips are activated."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-graphical-tooltips nil
  "If non-nil, show graphical bubbles for tooltips."
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

(defcustom ensime-default-server-host "127.0.0.1"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'ensime-server)

(defcustom ensime-default-port 9999
  "Port to use as the default for `ensime-connect'."
  :type 'integer
  :group 'ensime-server)

(defcustom ensime-default-server-cmd "bin/server.sh"
  "Command to launch server process."
  :type 'string
  :group 'ensime-server)

(defcustom ensime-default-server-root "/home/aemon/src/misc/ensime/"
  "Location of ENSIME server library."
  :type 'string
  :group 'ensime-server)

(defvar ensime-protocol-version "0.0.1")

(defvar ensime-prefer-noninteractive nil 
  "State variable used for regression testing.")

;;;;; ensime-mode

(defgroup ensime-mode nil
  "Settings for ensime-mode scala source buffers."
  :prefix "ensime-"
  :group 'ensime)

(defun ensime-scala-mode-hook ()
  "Conveniance hook function that just starts ensime-mode."
  (ensime-mode 1))

(defun ensime-after-save-hook ()
  "When a buffer is saved, ask the ENSIME server for a type-check."
  (ensime-typecheck-current-file))

(defun ensime-save-buffer-no-hook ()
  "Just save the buffer per usual, don't type-check!"
  (remove-hook 'after-save-hook 'ensime-after-save-hook t)
  (save-buffer)
  (add-hook 'after-save-hook 'ensime-after-save-hook nil t))

(defvar ensime-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t") 'ensime-inspect-type)
    (define-key map (kbd "C-c p") 'ensime-inspect-package)
    (define-key map (kbd "C-c o") 'ensime-inspect-project-package)
    (define-key map (kbd "C-c c") 'ensime-typecheck-current-file)
    map)
  "Keymap for `ensime-mode'.")


(define-minor-mode ensime-mode
  "ENSIME: The ENhanced Scala Interaction Mode for Emacs (minor-mode)."
  nil
  nil
  ensime-mode-map

  (if ensime-mode
      (progn
	(ac-ensime-enable)
	(add-hook 'after-save-hook 'ensime-after-save-hook nil t)
	(when ensime-tooltip-hints
	  (add-hook 'tooltip-functions 'ensime-tooltip-handler)
	  (make-local-variable 'track-mouse)
	  (setq track-mouse t)
	  (make-local-variable 'tooltip-delay)
	  (setq tooltip-delay 1.0)
	  (define-key ensime-mode-map [mouse-movement] 'ensime-mouse-motion))
	(define-key ensime-mode-map [double-mouse-1] 'ensime-mouse-1-double-click))
    (progn
      (ac-ensime-disable)
      (remove-hook 'after-save-hook 'ensime-after-save-hook t)
      (remove-hook 'tooltip-functions 'ensime-tooltip-handler)
      (make-local-variable 'track-mouse)
      (setq track-mouse nil)
      (define-key ensime-mode-map [mouse-movement] 'ignore)
      (define-key ensime-mode-map [double-mouse-1] 'ignore)
      )))

;;;;;; Mouse handlers

(defun ensime-mouse-1-double-click (event)
  "Command handler for double clicks of mouse button 1.
   If the user clicks on a package declaration or import, 
   inspect that package. Otherwise, try to inspect the type
   of the thing at point."
  (interactive "e")
  (let ((pack-path (ensime-import-or-package-path-at-point)))
    (if pack-path
	(ensime-inspect-package-by-path pack-path)
      (ensime-inspect-type))))

(defun ensime-mouse-motion (event)
  "Command handler for mouse movement events in `ensime-mode-map'."
  (interactive "e")
  (tooltip-hide)
  (when (car (mouse-pixel-position))
    (setq tooltip-last-mouse-motion-event (copy-sequence event))
    (tooltip-start-delayed-tip)))


;;;;;; Tooltips


(defun ensime-tooltip-show-message (msg)
  "Display tooltip, respecting ensime tooltip options."
  (if ensime-graphical-tooltips
      (tooltip-show msg tooltip-use-echo-area)
    (message msg)))


(defun ensime-tooltip-handler (event)
  "Hook function to display a help tooltip. If an error 
   or warning overlay exists at point, show the description 
   of that error or warning. Otherwise try to inspect the
   type of the expression under the cursor."
  (when (and (eventp event)
	     ensime-mode
	     (ensime-current-connection)
	     (posn-point (event-end event)))

    (let* ((point (posn-point (event-end event)))
	   (ident (tooltip-identifier-from-point point))
	   (note-overlays (ensime-overlays-at point)))

      (cond

       ;; If error or warning overlays exist, 
       ;; show that message..
       (note-overlays (progn
			(ensime-tooltip-show-message
			 (overlay-get (car note-overlays) 'help-echo))
			t))

       ;; Otherwise show a type hint..
       ((and ident ensime-tooltip-type-hints)
	(progn 
	  (ensime-eval-async 
	   `(swank:type-at-point ,buffer-file-name ,point)
	   #'(lambda (type)
	       (when type
		 (let ((msg (format "%s" (ensime-type-full-name type))))
		   (ensime-tooltip-show-message msg)
		   ))))
	  t
	  )))
      )))




;;;;;; Modeline

;; Setup the custom ensime modeline handler
(add-to-list 'minor-mode-alist
	     '(ensime-mode (:eval (ensime-modeline-string))))

(defun ensime-modeline-string ()
  "Return the string to display in the modeline.
  \"ENSIME\" only appears if we aren't connected.  If connected, include 
  connection-name, and possibly some state
  information."
  (let ((conn (ensime-current-connection)))
    ;; Bail out early in case there's no connection, so we won't
    ;; implicitly invoke `ensime-connection' which may query the user.
    (if (not conn)
	(and ensime-mode " ENSIME")
      (let ((local (eq conn ensime-buffer-connection)))
	(concat " "
		(if local "{" "[")
		(ignore-errors (ensime-connection-name conn))
		(ensime-modeline-state-string conn)
		(if local "}" "]"))))))


(defun ensime-modeline-state-string (conn)
  "Return a string possibly describing CONN's state."
  (cond ((not (eq (process-status conn) 'open))
	 (format " %s" (process-status conn)))
	((let ((pending (length (ensime-rex-continuations conn))))
	   (cond ((zerop pending) nil)
		 (t (format " %s" pending)))))))

;; Startup

(defun ensime ()
  "Read config file for settings. Then start an inferior 
   ENSIME server and connect to its Swank server."
  (interactive)
  (when (not ensime-mode) 
    (ensime-mode 1))
  (let* ((config (ensime-find-and-load-config))
	 (cmd (or (plist-get config :server-cmd) 
		  ensime-default-server-cmd))
	 (env (plist-get config :server-env))
	 (dir (or (plist-get config :server-root)
		  ensime-default-server-root))
	 (buffer "*inferior-ensime-server*")
	 (args (list (ensime-swank-port-file))))

    (ensime-delete-swank-port-file 'quiet)
    (let ((proc (ensime-maybe-start-server cmd args env dir buffer)))
      (ensime-inferior-connect config proc))))


(defun ensime-maybe-start-server (program program-args env directory buffer)
  "Return a new or existing inferior server process."
  (cond ((not (comint-check-proc buffer))
	 (ensime-start-server program program-args env directory buffer))
	((ensime-reinitialize-inferior-server-p program program-args env buffer)
	 (when-let (conn (find (get-buffer-process buffer) ensime-net-processes 
			       :key #'ensime-inferior-process))
	   (ensime-net-close conn))
	 (get-buffer-process buffer))
	(t (ensime-start-server program program-args env directory
				(generate-new-buffer-name buffer)))))


(defun ensime-reinitialize-inferior-server-p (program program-args env buffer)
  (let ((args (ensime-inferior-server-args (get-buffer-process buffer))))
    (and (equal (plist-get args :program) program)
	 (equal (plist-get args :program-args) program-args)
	 (equal (plist-get args :env) env)
	 (not (y-or-n-p "Create an additional *inferior-server*? ")))))


(defvar ensime-inferior-process-start-hook nil
  "Hook called whenever a new process gets started.")

(defun ensime-start-server (program program-args env directory buffer)
  "Does the same as `inferior-server' but less ugly.
   Return the created process."
  (with-current-buffer (get-buffer-create buffer)
    (when directory
      (cd (expand-file-name directory)))
    (comint-mode)
    (let ((process-environment (append env process-environment))
	  (process-connection-type nil))
      (comint-exec (current-buffer) "inferior-ensime-server" program nil program-args))
    (let ((proc (get-buffer-process (current-buffer))))
      (ensime-set-query-on-exit-flag proc)
      (run-hooks 'ensime-inferior-process-start-hook)
      proc)))

(defvar ensime-inferior-server-args nil
  "A buffer local variable in the inferior proccess.
See `ensime-start'.")

(defun ensime-inferior-server-args (process)
  "Return the initial process arguments.
   See `ensime-start'."
  (with-current-buffer (process-buffer process)
    ensime-inferior-server-args))

(defun ensime-inferior-connect (config process)
  "Start a Swank server in the inferior Server and connect."
  (ensime-read-port-and-connect config process nil))

(defvar ensime-config-file-name ".ensime"
  "The default file name for ensime project configurations.")

(defun ensime-find-config-file (file-name)
  "Search up the directory tree starting at file-name 
   for a suitable config file to load, return it's path. Return nil if 
   no such file found."
  (let* ((dir (file-name-directory file-name))
	 (possible-path (concat dir ensime-config-file-name)))
    (if (file-directory-p dir)
	(if (file-exists-p possible-path)
	    possible-path
	  (if (not (equal dir (directory-file-name dir)))
	      (ensime-find-config-file (directory-file-name dir)))))))

(defun ensime-find-and-load-config ()
  "Query the user for the path to a config file, then load it."
  (let* ((default (ensime-find-config-file buffer-file-name))
	 (file (if ensime-prefer-noninteractive default
		 (read-file-name 
		  "ENSIME Project file: "
		  (if default (file-name-directory default))
		  default
		  nil
		  (if default (file-name-nondirectory default))
		  ))))
    (ensime-load-config file)))


(defun ensime-load-config (file-name)
  "Load and parse a project config file. Return the resulting plist.

   The :root-dir setting will be deduced from the location of the project file.

   The :classpath setting may be specified as a list of file-names or
   the name of a function which will be called at load time. The configuration
   plist will be passed to this function as it's only argument."
  (let ((dir (expand-file-name (file-name-directory file-name))))
    (save-excursion
      (condition-case error
	  (let ((config
		 (let ((buf (find-file-read-only file-name ensime-config-file-name))
		       (src (buffer-substring-no-properties
			     (point-min) (point-max))))
		   (kill-buffer buf)
		   (read src))))

	    ;; We use the project file's location as th project root.
	    (plist-put config :root-dir dir)

	    ;; If 'classpath' is a symbol bound to a function, call it
	    ;; to compute the classpath.
	    (let ((cp (plist-get config :classpath)))
	      (if (and cp (symbolp cp) (boundp cp))
		  (let ((val (eval cp)))
		    (if (functionp val)
			(plist-put config :classpath (funcall val config))))))

	    config)
	(error
	 '())))
    ))

(defun ensime-swank-port-file ()
  "Filename where the SWANK server writes its TCP port number."
  (ensime-temp-file-name (format "ensime_port.%S" (emacs-pid))))

(defun ensime-read-swank-port ()
  "Read the Swank server port number from the `ensime-swank-port-file'."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (ensime-swank-port-file))
      (goto-char (point-min))
      (let ((port (read (current-buffer))))
	(assert (integerp port))
	port))))

(defun ensime-temp-file-name (name)
  "Return the path of a temp file with filename 'name'."
  (concat (file-name-as-directory (ensime-temp-directory))
	  name))

(defun ensime-temp-directory ()
  "Return the directory name of the system's temporary file dump."
  (cond ((fboundp 'temp-directory) (temp-directory))
	((boundp 'temporary-file-directory) temporary-file-directory)
	(t "/tmp/")))

(defun ensime-delete-swank-port-file (&optional quiet)
  (condition-case data
      (delete-file (ensime-swank-port-file))
    (error
     (ecase quiet
       ((nil) (signal (car data) (cdr data)))
       (quiet)
       (message (message "Unable to delete swank port file %S"
			 (ensime-swank-port-file)))))))

(defun ensime-read-port-and-connect (config inferior-process retries)
  (ensime-cancel-connect-retry-timer)
  (ensime-attempt-connection config inferior-process retries 1))


(defun ensime-attempt-connection (config process retries attempt)
  ;; A small one-state machine to attempt a connection with
  ;; timer-based retries.
  (let ((host (or (plist-get config :server-host) ensime-default-server-host))
	(port-file (ensime-swank-port-file)))
    (unless (active-minibuffer-window)
      (message "Polling %S.. (Abort with `M-x ensime-abort-connection'.)" port-file))
    (cond ((and (file-exists-p port-file)
		(> (nth 7 (file-attributes port-file)) 0)) ; file size
	   (ensime-cancel-connect-retry-timer)
	   (let ((port (ensime-read-swank-port))
		 (args (ensime-inferior-server-args process)))
	     (ensime-delete-swank-port-file 'message)
	     (let ((c (ensime-connect config host port)))
	       (ensime-set-config c config)
	       (ensime-set-inferior-process c process)
	       )))
	  ((and retries (zerop retries))
	   (ensime-cancel-connect-retry-timer)
	   (message "Gave up connecting to Swank after %d attempts." attempt))
	  ((eq (process-status process) 'exit)
	   (ensime-cancel-connect-retry-timer)
	   (message "Failed to connect to Swank: inferior process exited."))
	  (t
	   (when (and (file-exists-p port-file) 
		      (zerop (nth 7 (file-attributes port-file))))
	     (message "(Zero length port file)")
	     ;; the file may be in the filesystem but not yet written
	     (unless retries (setq retries 3)))
	   (unless ensime-connect-retry-timer
	     (setq ensime-connect-retry-timer
		   (run-with-timer
		    0.3 0.3
		    #'ensime-timer-call #'ensime-attempt-connection 
		    config process (and retries (1- retries)) 
		    (1+ attempt))))))))

(defvar ensime-connect-retry-timer nil
  "Timer object while waiting for the inferior server to start.")

(defun ensime-timer-call (fun &rest args)
  "Call function FUN with ARGS, reporting all errors.

The default condition handler for timer functions (see
`timer-event-handler') ignores errors."
  (condition-case data
      (apply fun args)
    (error (debug nil (list "Error in timer" fun args data)))))

(defun ensime-cancel-connect-retry-timer ()
  (when ensime-connect-retry-timer
    (cancel-timer ensime-connect-retry-timer)
    (setq ensime-connect-retry-timer nil)))

(defun ensime-abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (cond (ensime-connect-retry-timer
	 (ensime-cancel-connect-retry-timer)
	 (message "Cancelled connection attempt."))
	(t (error "Not connecting"))))



;;;; Framework'ey bits
;;;
;;; This section contains some standard ENSIME idioms: basic macros,
;;; ways of showing messages to the user, etc. All the code in this
;;; file should use these functions when applicable.
;;;
;;;;; Syntactic sugar


(defcustom ensime-scaladoc-stdlib-url-base 
  "http://www.scala-lang.org/archives/downloads/distrib/files/nightly/docs/library/"
  "url for constructing scaladoc links."
  :type 'string
  :group 'ensime)

(defcustom ensime-scaladoc-compiler-url-base 
  "http://www.scala-lang.org/archives/downloads/distrib/files/nightly/docs/compiler/"
  "url for constructing scaladoc links."
  :type 'string
  :group 'ensime)

(defcustom ensime-javadoc-stdlib-url-base 
  "http://java.sun.com/javase/6/docs/api/"
  "url for constructing scaladoc links."
  :type 'string
  :group 'ensime)


(defun ensime-make-scaladoc-url (type &optional member)
  "Given a scala type, and optionally a type member, construct the 
   corresponding scaladoc url."
  (let* ((full-type-name (ensime-type-full-name type))
	 (is-std-lib (not (null (string-match "^scala\\." full-type-name))))
	 (is-compiler-lib (not (null (string-match "^scala\\.tools\\.nsc\\." full-type-name))))
	 (url-base (cond (is-compiler-lib ensime-scaladoc-compiler-url-base)
			 (is-std-lib ensime-scaladoc-stdlib-url-base)
			 (t nil))))
    (if (or is-std-lib is-compiler-lib)
	(concat url-base
		(replace-regexp-in-string "\\." "/" full-type-name)
		".html"
		(if member
		    (let* ((name (ensime-member-name member))
			   (type (ensime-member-type member))
			   (param-types (ensime-type-param-types type)))
		      (concat "#" full-type-name "#" name))))
      )))

(defvar ensime-javadoc-type-replacements 
  '(("^scala.Int$" . "int")
    ("^scala.Double$" . "double")
    ("^scala.Short$" . "short")
    ("^scala.Byte$" . "byte")
    ("^scala.Long$" . "long")
    ("^scala.Float$" . "float")
    ("^scala.Boolean$" . "char")
    ("^scala.Unit$" . "void"))
  "When creating javadoc urls, 
   use this mapping to replace scala types with java types.")

(defun ensime-javadoc-replace-types (str)
  "Replace scala primitive type names with jave names."
  (dolist (rep ensime-javadoc-type-replacements)
    (setq str (replace-regexp-in-string 
	       (car rep) (cdr rep) str)))
  str)

(defun ensime-make-javadoc-url (type &optional member)
  "Given a java type, and optionally a java type member, construct the
   corresponding javadoc url."
  (let* ((full-type-name (ensime-type-full-name type))
	 (is-std-lib (not (null (string-match "^java\\." full-type-name)))))
    (if is-std-lib
	(concat ensime-javadoc-stdlib-url-base 
		(replace-regexp-in-string "\\." "/" full-type-name)
		".html"
		(if member
		    (let* ((name (ensime-member-name member))
			   (type (ensime-member-type member))
			   (param-types (ensime-type-param-types type)))
		      (concat
		       "#" name
		       "("  
		       (mapconcat 
			(lambda (tpe)
			  (ensime-javadoc-replace-types 
			   (ensime-type-full-name tpe))) param-types ", ")
		       ")"))))
      )))


(defun ensime-make-code-link (start end file-path offset &optional face)
  "Make an emacs button, from start to end in current buffer, linking to file-path and offset."
  (make-button start end
	       'face (or face font-lock-keyword-face)
	       'action `(lambda (x)
			  (find-file-other-window ,file-path)
			  (goto-char ,offset)
			  )))

(defun ensime-make-code-hyperlink (start end http-path &optional face)
  "Make an emacs button, from start to end in current buffer, hyperlinking to http-path."
  (make-button start end
	       'face (or face font-lock-constant-face)
	       'action `(lambda (x)
			  (browse-url ,http-path)
			  (message "Opening documentation in browser..")
			  )))

(defun ensime-insert-link (text file-path &optional offset)
  "Insert text in current buffer and make it into an emacs 
   button, linking to file-path and offset. Intelligently decide
   whether to make a source link or an http link based on the file-path."
  (let ((start (point)))
    (cond
     ((and file-path (string-match "http://" file-path))
      (progn
	(insert text)
	(ensime-make-code-hyperlink start (point) file-path)))

     ((and file-path (integerp offset))
      (progn
	(insert text)
	(ensime-make-code-link start (point) file-path offset))))))


(defun ensime-insert-action-link (text action &optional face)
  "Insert text in current buffer and make it into an emacs 
   button, linking to file-path and offset."
  (let ((start (point)))
    (insert text)
    (make-button start (point) 'face (or face font-lock-variable-name-face) 'action action)))

(defun ensime-insert-with-face (text face)
  "Insert text in current buffer and color it with face"
  (let ((start (point)))
    (insert text)
    (set-text-properties start (point) `(face ,face))))

(defvar ensime-qualified-type-regexp 
  "^\\(?:object \\)?\\(\\(?:[a-z0-9]+\\.\\)*[a-z0-9]+\\)\\.\\(.+\\)$")

(defmacro* ensime-with-path-and-name (type-name (path name) &rest body)
  "Evaluate BODY with path bound to the dot-separated path of this type-name, and
   name bound to the final type name."
  `(let ((result (not (null (string-match 
			     ensime-qualified-type-regexp 
			     ,type-name)))))
     (let ((,path (if result (match-string 1 ,type-name) nil))
	   (,name (if result (match-string 2 ,type-name) ,type-name)))
       ,@body)))


(defun ensime-re-search-containing-point (regex limit-start limit-end &optional group-number pos)
  "A helper for finding regex matches for which the current point is contained in a specified group."
  (let ((group-number (or group-number 0))
	(pos (or pos (point))))
    (save-excursion
      (goto-char limit-end)
      (catch 'return-now
	(while (> (point) limit-start)
	  (let* ((search-result (re-search-backward regex limit-start 1))
		 (contains-result (and search-result
				       (<= (match-beginning group-number) pos)
				       (> (match-end group-number) pos))))

	    (cond ((and search-result (not contains-result))
		   (goto-char (- (match-end 0) 1)))

		  ((and search-result contains-result)
		   (throw 'return-now t)))
	    ))))))


(defun ensime-kill-txt-props (str)
  "Remove all text-properties from str and return str."
  (set-text-properties 0 (length str) nil str)
  str)


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

(ensime-def-connection-var ensime-config nil
  "The project configuration corresponding to this connection.")

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

;; FIXME: should be called auto-start
(defcustom ensime-auto-connect 'never
  "Controls auto connection when information from lisp process is needed.
This doesn't mean it will connect right after Ensime is loaded."
  :group 'ensime-mode
  :type '(choice (const never)
		 (const always)
		 (const ask)))

(defun ensime-auto-connect ()
  (cond ((or (eq ensime-auto-connect 'always)
	     (and (eq ensime-auto-connect 'ask)
		  (y-or-n-p "No connection.  Start Ensime? ")))
	 (save-window-excursion
	   (ensime)
	   (while (not (ensime-current-connection))
	     (sleep-for 1))
	   (ensime-connection)))
	(t nil)))

(defun ensime-setup-connection (config process)
  "Make a connection out of PROCESS."
  (let ((ensime-dispatching-connection process))
    (ensime-init-connection-state config process)
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

(defun ensime-connect (config host port)
  "Connect to a running Swank server. Return the connection."
  (interactive (list (read-from-minibuffer "Host: " ensime-default-server-host)
		     (read-from-minibuffer "Port: " (format "%d" ensime-default-port)
					   nil t)))
  (when (and (interactive-p) ensime-net-processes
	     (y-or-n-p "Close old connections first? "))
    (ensime-disconnect-all))
  (message "Connecting to Swank on port %S.." port)
  (let ()
    (message "Connecting to Swank on port %S.." port)
    (let* ((process (ensime-net-connect host port))
	   (ensime-dispatching-connection process))
      (ensime-setup-connection config process))))


(defun ensime-init-connection-state (config proc)
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
		     (ensime-curry #'ensime-set-connection-info config proc)))


(defun ensime-set-connection-info (config connection info)
  "Initialize CONNECTION with INFO received from Lisp."
  (ensime-test-sig :connected info)
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
		  (ensime-inferior-server-args p))))
      (when-let (name (plist-get args ':name))
	(unless (string= (ensime-server-implementation-name) name)
	  (setf (ensime-connection-name)
		(ensime-generate-connection-name (symbol-name name)))))
      ;; TODO
      ;;(ensime-load-contribs)
      (run-hooks 'ensime-connected-hook)
      (when-let (fun (plist-get args ':init-function))
	(funcall fun)))
    (message "Connected. %s" (ensime-random-words-of-encouragement))

    ;; Send the project initialization..
    (ensime-eval-async `(swank:init-project ,config) #'identity)
    ))


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

  ;; TODO should this be per-connection?
  (ensime-remove-old-overlays)

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
		       sexp
		       &rest continuations)
  "(ensime-rex (VAR ...) SEXP CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

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
	(list :emacs-rex ,sexp
	      (lambda (,result)
		(destructure-case ,result
		  ,@continuations)))))))

(put 'ensime-rex 'lisp-indent-function 2)


;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar ensime-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun ensime-eval (sexp)
  "Evaluate EXPR on the superior Lisp and return the result."
  (let* ((tag (gensym (format "ensime-result-%d-sym" 
			      (1+ (ensime-continuation-counter)))))
	 (ensime-stack-eval-tags (cons tag ensime-stack-eval-tags)))
    (apply
     #'funcall 
     (catch tag
       (ensime-rex (tag sexp)
	   sexp
	 ((:ok value)
	  (unless (member tag ensime-stack-eval-tags)
	    (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
		   tag sexp))
	  (throw tag (list #'identity value)))
	 ((:abort)
	  (throw tag (list #'error "Synchronous Lisp Evaluation aborted"))))
       (let ((debug-on-quit t)
	     (inhibit-quit nil)
	     (conn (ensime-connection)))
	 (while t 
	   (unless (eq (process-status conn) 'open)
	     (error "Lisp connection closed unexpectedly"))
	   (ensime-accept-process-output nil 0.01)))))))


(defun ensime-eval-async (sexp &optional cont)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (ensime-rex (cont (buffer (current-buffer)))
      sexp
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

;; Non-macro version to keep the file byte-compilable. 
(defun ensime-set-inferior-process (connection process)
  (setf (ensime-inferior-process connection) process))

(defun ensime-set-config (connection config)
  (setf (ensime-config connection) config))


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
	  ((:emacs-rex form continuation)
	   (let ((id (incf (ensime-continuation-counter))))
	     (ensime-send `(:emacs-rex ,form ,id))
	     (push (cons id continuation) (ensime-rex-continuations))
	     ))
	  ((:return value id)
	   (let ((rec (assq id (ensime-rex-continuations))))
	     (cond (rec (setf (ensime-rex-continuations)
			      (remove rec (ensime-rex-continuations)))
			(funcall (cdr rec) value)
			(force-mode-line-update t))
		   (t
		    (error "Unexpected reply: %S %S" id value)))))
	  ((:full-typecheck-result result)
	   (ensime-typecheck-finished result)
	   (ensime-test-sig :full-typecheck-finished result))
	  ((:quick-typecheck-result result)
	   (ensime-typecheck-finished result)
	   (ensime-test-sig :quick-typecheck-finished result))
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






;; Compiler Notes (Error/Warning overlays)

(defvar ensime-note-overlays '())

(defun ensime-typecheck-finished (result)
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

			     (t (ensime-make-overlay
				 start stop msg 'ensime-warnline nil))
			     )))
	      (push ov ensime-note-overlays)
	      )))))))

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

(defun ensime-overlays-at (point)
  "Return list of overlays of type 'ensime-overlay at point."
  (let ((ovs (overlays-at point)))
    (remove-if-not 
     (lambda (ov) (overlay-get ov 'ensime-overlay))
     ovs)
    ))

(defun ensime-remove-old-overlays ()
  "Delete the existing note overlays."
  ;; Guard against nil overlays here..
  (mapc #'delete-overlay ensime-note-overlays)
  (setq ensime-note-overlays '()))




;; Test compile current file

(defun ensime-typecheck-current-file ()
  "Send a request for re-typecheck to the ENSIME server.
   Current file is saved if it has unwritten modifications."
  (interactive)
  (if (buffer-modified-p) (ensime-save-buffer-no-hook))
  (ensime-rpc-async-typecheck-file buffer-file-name))


;; Basic RPC calls

(defun ensime-rpc-async-typecheck-file (file-name)
  (ensime-eval-async `(swank:typecheck-file ,file-name) #'identity))

(defun ensime-rpc-name-completions-at-point (&optional prefix)
  (ensime-eval 
   `(swank:scope-completion ,buffer-file-name ,(point) ,(or prefix ""))))

(defun ensime-rpc-members-for-type-at-point (&optional prefix)
  (ensime-eval 
   `(swank:type-completion ,buffer-file-name ,(point) ,(or prefix ""))))

(defun ensime-rpc-get-type-by-id (id)
  (if (and (integerp id) (> id -1))
      (ensime-eval 
       `(swank:type-by-id ,id))))

(defun ensime-rpc-get-type-at-point ()
  (ensime-eval 
   `(swank:type-at-point ,buffer-file-name ,(point))))

(defun ensime-rpc-inspect-type-at-point ()
  (ensime-eval 
   `(swank:inspect-type-at-point ,buffer-file-name ,(point))))

(defun ensime-rpc-inspect-type-by-id (id)
  (if (and (integerp id) (> id -1))
      (ensime-eval 
       `(swank:inspect-type-by-id ,id))))

(defun ensime-rpc-inspect-package-by-path (path)
  (ensime-eval 
   `(swank:inspect-package-by-path ,path)))



;; Type Inspector UI

(defvar ensime-indent-level 0
  "In inspector UI, how much to indent.")

(defun ensime-inspector-insert-linked-package-path (path &optional face)
  "For each component of the package path, insert a link to inspect
   that package."
  (let ((pieces (split-string path "\\."))
	(accum ""))
    (dolist (piece pieces)
      (setq accum (concat accum piece))
      (ensime-insert-action-link
       piece 
       `(lambda (x)
	  (ensime-inspect-package-by-path ,accum))
       (or face font-lock-type-face))
      (insert ".")
      (setq accum (concat accum "."))
      )))

(defun ensime-inspector-insert-linked-type (type &optional with-doc-link)
  "Helper utility to output a link to a type.
   Should only be invoked by ensime-inspect-type"
  (if (ensime-type-is-arrow type) 
      (ensime-inspector-insert-linked-arrow-type type)
    (let* ((type-name (ensime-type-name type)))
      (ensime-with-path-and-name 
       type-name (path name)
       (when path
	 (ensime-inspector-insert-linked-package-path path))
       (insert (make-string ensime-indent-level ?\s))
       (ensime-insert-action-link
	name
	`(lambda (x)
	   (ensime-type-inspector-show 
	    (ensime-rpc-inspect-type-by-id ,(ensime-type-id type))
	    )) font-lock-type-face))

      (when with-doc-link
	(let* ((pos (plist-get type :pos))
	       (url (or (ensime-pos-file pos)
			(ensime-make-scaladoc-url type)
			(ensime-make-javadoc-url type)
			)))
	  (ensime-insert-link " doc" url (ensime-pos-offset pos))))

      )))

(defun ensime-inspector-insert-linked-arrow-type (type)
  "Helper utility to output a link to a type.
   Should only be invoked by ensime-inspect-type"
  (let*  ((param-types (ensime-type-param-types type))
	  (last-param-type (car (last param-types)))
	  (result-type (ensime-type-result-type type)))
    (insert "(")
    (dolist (tpe param-types)
      (ensime-inspector-insert-linked-type tpe)
      (if (not (eq tpe last-param-type))
	  (insert ", ")))
    (insert ") => ")
    (ensime-inspector-insert-linked-type result-type)))


(defun ensime-inspector-insert-linked-member (owner-type m)
  "Helper utility to output a link to a type member.
   Should only be invoked by ensime-inspect-type"
  (let* ((type (ensime-member-type m))
	 (pos (ensime-member-pos m))
	 (member-name (ensime-member-name m))
	 (url (or (ensime-pos-file pos)
		  (ensime-make-scaladoc-url owner-type m)
		  (ensime-make-javadoc-url owner-type m)
		  )))
    (ensime-insert-link 
     (format "%s" member-name) url (ensime-pos-offset pos))
    (tab-to-tab-stop)
    (ensime-inspector-insert-linked-type type)
    ))

(defun ensime-inspect-type ()
  "Display a list of all the members of the type under point, sorted by
   owner type."
  (interactive)
  (ensime-type-inspector-show (ensime-rpc-inspect-type-at-point)))

(defun ensime-type-inspector-show (info)
  "Display a list of all the members of the type under point, sorted by
   owner type."
  (let* ((supers (plist-get info :supers))
	 (type (plist-get info :type))
	 (buffer-name "*Inspector*")
	 (ensime-indent-level 0))
    (if (eq (get-buffer buffer-name) (current-buffer))
	(kill-buffer-and-window))
    (ensime-with-inspector-buffer 
     (buffer-name info nil t)

     ;; We want two main columns. The first, 20 chars wide.
     (let ((tab-stop-list '(20)))
       (setq wrap-prefix (make-string 21 ?\s))

       ;; Display main type
       (let* ((full-type-name (plist-get type :name)))
	 (ensime-insert-with-face (format "%s\n" 
					  (ensime-type-declared-as-str type))
				  font-lock-comment-face)
	 (ensime-inspector-insert-linked-type type t)
	 (insert "\n")


	 ;; Display each member, arranged by owner type
	 (dolist (super supers)
	   (let* ((owner-type super)
		  (members (plist-get super :members)))

	     (ensime-insert-with-face 
	      (format "\n\n%s\n" 
		      (ensime-type-declared-as-str owner-type))
	      font-lock-comment-face)
	     (ensime-inspector-insert-linked-type owner-type t)
	     (insert "\n")
	     (insert "---------------------------\n")
	     (dolist (m members)
	       (ensime-inspector-insert-linked-member owner-type m)
	       (insert "\n")
	       )
	     ))

	 (goto-char (point-min))
	 ))
     )))



;; Package Inspector

(defun ensime-inspect-package-by-path (&optional path)
  "Display a list of all the members of the package with user provided path."
  (interactive)
  (let ((p (or path (read-string "Package path: "))))
    (ensime-package-inspector-show (ensime-rpc-inspect-package-by-path p))))

(defun ensime-import-or-package-path-at-point ()
  "Return the path of the package that is declared or imported at point."
  (let* ((case-fold-search nil)
	 (re "\\(?:package\\|import\\)\\s-+\\(\\(?:[a-z0-9]+\\.\\)*[a-z0-9]+\\)"))
    (save-excursion
      (goto-char (point-at-bol))
      (if (search-forward-regexp re (point-at-eol) t)
	  (ensime-kill-txt-props (match-string 1))))))

(defun ensime-inspect-package ()
  "Inspect the package of the current source file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp 
	 "package \\(\\(?:[a-z0-9]+\\.\\)*[a-z0-9]+\\)"
	 (point-max) t)
	(let ((path (match-string 1)))
	  (ensime-kill-txt-props path)
	  (ensime-package-inspector-show (ensime-rpc-inspect-package-by-path path)))
      (message "No package declaration found."))))


(defun ensime-inspect-project-package ()
  "Inspect the package declared as the project package in the config file."
  (interactive)
  (let* ((config (ensime-config))
	 (path (or (plist-get config :project-package)
		   (read-string "Missing :project-package in config. Enter package path: "))))
    (if (and path (> (length path) 0))
	(ensime-package-inspector-show (ensime-rpc-inspect-package-by-path path)))))

(defun ensime-inspector-insert-package (pack)
  "Helper to insert a hyper-linked package name."
  (let ((name (ensime-package-full-name pack))
	(members (ensime-package-members pack)))
    (insert (make-string ensime-indent-level ?\s))
    (ensime-inspector-insert-linked-package-path name font-lock-variable-name-face)
    (insert "\n")
    (let ((ensime-indent-level (+ ensime-indent-level 5)))
      (dolist (ea members)
	(when (not (ensime-package-p ea))
	  (ensime-inspector-insert-linked-type ea)
	  (insert "\n")))
      (dolist (ea members)
	(when (ensime-package-p ea)
	  (ensime-inspector-insert-package ea)
	  ))
      )))

(defun ensime-package-inspector-show (info)
  "Display a list of all the members of the provided package."
  (let* ((buffer-name "*Inspector*")
	 (ensime-indent-level 0))
    (if (eq (get-buffer buffer-name) (current-buffer))
	(kill-buffer-and-window))
    (ensime-with-inspector-buffer
     (buffer-name info nil t)
     (ensime-inspector-insert-package info)
     (goto-char (point-min))
     )))

(defvar ensime-inspector-history '()
  "Maintain a history of the info objects viewed in the type inspector.")

(defvar ensime-inspector-history-cursor 0
  "Where are we in the history?")

(defvar ensime-inspector-paging-in-progress nil
  "A dynamic variable to inform dynamic extant of user's intent.
   Are we moving in history, or inspecting a new info?")

(defun ensime-inspector-backward-page ()
  "Inspect the info object preceding current in history."
  (interactive)
  (setq ensime-inspector-history-cursor 
	(min (- (length ensime-inspector-history) 1)
	     (+ ensime-inspector-history-cursor 1)))
  (ensime-inspector-goto-cursor))

(defun ensime-inspector-forward-page ()
  "Inspect the info object following current in history."
  (interactive)
  (setq ensime-inspector-history-cursor 
	(max 0 (- ensime-inspector-history-cursor 1)))
  (ensime-inspector-goto-cursor))


(defun ensime-inspector-goto-cursor ()
  "Helper to jump to a specific point in history."
  (let ((info (nth ensime-inspector-history-cursor
		   ensime-inspector-history))
	(ensime-inspector-paging-in-progress t))

    (cond ((ensime-package-p info)
	   (ensime-package-inspector-show info))

	  (t (ensime-type-inspector-show info)))
    ))


(defvar ensime-popup-inspector-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'forward-button)
    (define-key map (kbd "M-n") 'forward-button)
    (define-key map (kbd "M-p") 'backward-button)
    (define-key map (kbd ".") 'ensime-inspector-forward-page)
    (define-key map (kbd ",") 'ensime-inspector-backward-page)
    map)
  "Type and package inspector specific key bindings 
   (in addition to those defined by popup-buffer-mode)")


(defmacro* ensime-with-inspector-buffer ((name object &optional connection select)
					 &body body)
  "Extend the standard popup buffer with inspector-specific bindings."
  `(ensime-with-popup-buffer
    (,name ,connection ,select)
    (use-local-map ensime-popup-inspector-map)
    (when (not ensime-inspector-paging-in-progress)

      ;; First clamp the cursor, for safety
      (setq ensime-inspector-history-cursor
	    (max 0 ensime-inspector-history-cursor))
      (setq ensime-inspector-history-cursor
	    (min (- (length ensime-inspector-history) 1) 
		 ensime-inspector-history-cursor))

      ;; Remove all elements preceding the cursor (the 'redo' history)
      (setq ensime-inspector-history
	    (subseq ensime-inspector-history
		    ensime-inspector-history-cursor))

      ;; Add the new history item
      (push ,object ensime-inspector-history)

      ;; Set cursor to point to the new item
      (setq ensime-inspector-history-cursor 0)
      
      )
    ,@body
    ))


;; Interface

(defvar ensime-message-function 'message)

(defun ensime-minibuffer-respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
	(if (fboundp 'temp-minibuffer-message) ;; XEmacs
	    (temp-minibuffer-message text)
	  (minibuffer-message text))
      (message "%s" text))))

(defun ensime-message (format &rest args)
  "Like `message' but with special support for multi-line messages.
Single-line messages use the echo area."
  (apply ensime-message-function format args))

(defun ensime-display-warning (message &rest args)
  (display-warning '(ensime warning) (apply #'format message args)))

(defvar ensime-background-message-function 'ensime-display-oneliner)


(defun ensime-background-message (format-string &rest format-args)
  "Display a message in passing.
This is like `ensime-message', but less distracting because it
will never pop up a buffer or display multi-line messages.
It should be used for \"background\" messages such as argument lists."
  (apply ensime-background-message-function format-string format-args))

(defun ensime-display-oneliner (format-string &rest format-args)
  (let* ((msg (apply #'format format-string format-args)))
    (unless (minibuffer-window-active-p (minibuffer-window))
      (message  "%s" (ensime-oneliner msg)))))

(defun ensime-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
			   (or (position ?\n string) most-positive-fixnum)
			   (1- (frame-width)))))



;; Data-structure accessors

(defun ensime-package-name (info)
  (plist-get info :name))

(defun ensime-package-full-name (info)
  (plist-get info :full-name))

(defun ensime-package-members (info)
  (plist-get info :members))

(defun ensime-package-p (info)
  (equal 'package (plist-get info :info-type)))

(defun ensime-type-name (type)
  (plist-get type :name))

(defun ensime-type-id (type)
  (plist-get type :type-id))

(defun ensime-type-full-name (type)
  (if (plist-get type :arrow-type)
      (plist-get type :name)
    (plist-get type :full-name)))

(defun ensime-type-declared-as-str (type)
  (case (plist-get type :declared-as)
    (trait "trait")
    (interface "interface")
    (class "class")
    (abstractclass "abstract class")
    (otherwise "type")
    ))

(defun ensime-type-is-arrow (type)
  (plist-get type :arrow-type))

(defun ensime-type-param-types (type)
  (plist-get type :param-types))

(defun ensime-type-result-type (type)
  (plist-get type :result-type))

(defun ensime-member-name (member)
  (plist-get member :name))

(defun ensime-member-type (member)
  (plist-get member :type))

(defun ensime-member-pos (member)
  (plist-get member :pos))

(defun ensime-pos-file (pos)
  (plist-get pos :file))

(defun ensime-pos-offset (pos)
  (or (plist-get pos :offset) -1))


;; Portability

(defvar ensime-accept-process-output-supports-floats 
  (ignore-errors (accept-process-output nil 0.0) t))

(defun ensime-accept-process-output (&optional process timeout)
  "Like `accept-process-output' but the TIMEOUT argument can be a float."
  (cond (ensime-accept-process-output-supports-floats
	 (accept-process-output process timeout))
	(t
	 (accept-process-output process 
				(if timeout (truncate timeout))
				;; Emacs 21 uses microsecs; Emacs 22 millisecs
				(if timeout (truncate (* timeout 1000000)))))))


;; Popup Buffer

;;;;; Temporary popup buffers

(defvar ensime-popup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'ensime-popup-buffer-quit-function)
    (define-key map [mouse-1] 'push-button)
    map)
  "Keymap for `ensime-popup-buffer-mode'.")

(define-minor-mode ensime-popup-buffer-mode 
  "Mode for displaying read only stuff"
  nil
  nil
  ensime-popup-buffer-mode-map)

(add-to-list 'minor-mode-alist
	     '(ensime-popup-buffer-mode (:eval (ensime-modeline-string))))

(defvar ensime-popup-restore-data nil
  "Data needed when closing popup windows.
This is used as buffer local variable.
The format is (POPUP-WINDOW SELECTED-WINDOW OLD-BUFFER).
POPUP-WINDOW is the window used to display the temp buffer.
That window may have been reused or freshly created.
SELECTED-WINDOW is the window that was selected before displaying
the popup buffer.
OLD-BUFFER is the buffer that was previously displayed in POPUP-WINDOW.
OLD-BUFFER is nil if POPUP-WINDOW was newly created.

See `view-return-to-alist' for a similar idea.")

;; keep compiler quiet
(defvar ensime-buffer-package)
(defvar ensime-buffer-connection)

;; Interface
(defmacro* ensime-with-popup-buffer ((name &optional connection select)
				     &body body)
  "Similar to `with-output-to-temp-buffer'.
Bind standard-output and initialize some buffer-local variables.
Restore window configuration when closed.

NAME is the name of the buffer to be created.
CONNECTION is the value for `ensime-buffer-connection'.
If nil, no explicit connection is associated with
the buffer.  If t, the current connection is taken.
"
  `(let* ((vars% (list ,(if (eq connection t) '(ensime-connection) connection)))
	  (standard-output (ensime-make-popup-buffer ,name vars%)))
     (with-current-buffer standard-output
       (prog1 (progn ,@body)
	 (assert (eq (current-buffer) standard-output))
	 (ensime-init-popup-buffer vars%)
	 (setq buffer-read-only t)
	 (set-window-point (ensime-display-popup-buffer ,(or select 'nil))
			   (point))))))


(defun ensime-make-popup-buffer (name buffer-vars)
  "Return a temporary buffer called NAME.
The buffer also uses the minor-mode `ensime-popup-buffer-mode'."
  (with-current-buffer (get-buffer-create name)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (set-syntax-table lisp-mode-syntax-table)
    (ensime-init-popup-buffer buffer-vars)
    (current-buffer)))

(defun ensime-init-popup-buffer (buffer-vars)
  (ensime-popup-buffer-mode 1)
  (multiple-value-setq (ensime-buffer-package ensime-buffer-connection)
    buffer-vars))

(defun ensime-display-popup-buffer (select)
  "Display the current buffer.
   Save the selected-window in a buffer-local variable, so that we
   can restore it later."
  (let ((selected-window (selected-window))
	(old-windows))
    (walk-windows (lambda (w) (push (cons w (window-buffer w)) old-windows))
		  nil t)
    (let ((new-window (display-buffer (current-buffer))))
      (unless ensime-popup-restore-data
	(set (make-local-variable 'ensime-popup-restore-data)
	     (list new-window
		   selected-window
		   (cdr (find new-window old-windows :key #'car)))))
      (when select
	(select-window new-window))
      new-window)))

(defun ensime-close-popup-window ()
  (when ensime-popup-restore-data
    (destructuring-bind (popup-window selected-window old-buffer)
	ensime-popup-restore-data
      (kill-local-variable 'ensime-popup-restore-data)
      (bury-buffer)
      (when (eq popup-window (selected-window))
	(cond ((and (not old-buffer) (not (one-window-p)))
	       (delete-window popup-window))
	      ((and old-buffer (buffer-live-p old-buffer))
	       (set-window-buffer popup-window old-buffer))
	      ))
      (when (window-live-p selected-window)
	(select-window selected-window)))
    ))


(defmacro ensime-save-local-variables (vars &rest body)
  (let ((vals (make-symbol "vals")))
    `(let ((,vals (mapcar (lambda (var)
			    (if (ensime-local-variable-p var)
				(cons var (eval var))))
			  ',vars)))
       (prog1 (progn . ,body)
	 (mapc (lambda (var+val)
		 (when (consp var+val)
		   (set (make-local-variable (car var+val)) (cdr var+val))))
	       ,vals)))))


(make-variable-buffer-local
 (defvar ensime-popup-buffer-quit-function 'ensime-popup-buffer-quit
   "The function that is used to quit a temporary popup buffer."))

(defun ensime-popup-buffer-quit-function (&optional kill-buffer-p)
  "Wrapper to invoke the value of `ensime-popup-buffer-quit-function'."
  (interactive)
  (funcall ensime-popup-buffer-quit-function kill-buffer-p))

(defun ensime-popup-buffer-quit (&optional kill-buffer-p)
  "Get rid of the current (temp) buffer without asking.
  Restore the window configuration unless it was changed since we
  last activated the buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (ensime-close-popup-window)
    (when kill-buffer-p
      (kill-buffer buffer))))


;;;;; Connection listing

(define-derived-mode ensime-connection-list-mode fundamental-mode
  "Ensime-Connections"
  "ENSIME Connection List Mode.

\\{ensime-connection-list-mode-map}
\\{ensime-popup-buffer-mode-map}"
  (when ensime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

(ensime-define-keys ensime-connection-list-mode-map
		    ("d"         'ensime-connection-list-make-default)
		    ("g"         'ensime-update-connection-list)
		    ((kbd "C-k") 'ensime-quit-connection-at-point)
		    ("R"         'ensime-restart-connection-at-point))

(defun ensime-connection-at-point ()
  (or (get-text-property (point) 'ensime-connection)
      (error "No connection at point")))

(defun ensime-quit-connection-at-point (connection)
  (interactive (list (ensime-connection-at-point)))
  (let ((ensime-dispatching-connection connection)
	(end (time-add (current-time) (seconds-to-time 3))))
    (ensime-quit-lisp t)
    (while (memq connection ensime-net-processes)
      (when (time-less-p end (current-time))
	(message "Quit timeout expired.  Disconnecting.")
	(delete-process connection))
      (sit-for 0 100)))
  (ensime-update-connection-list))

(defun ensime-restart-connection-at-point (connection)
  (interactive (list (ensime-connection-at-point)))
  (let ((ensime-dispatching-connection connection))
    (ensime-restart-inferior-lisp)))

(defun ensime-connection-list-make-default ()
  "Make the connection at point the default connection."
  (interactive)
  (ensime-select-connection (ensime-connection-at-point))
  (ensime-update-connection-list))

(defvar ensime-connections-buffer-name "*ENSIME Connections*")

(defun ensime-list-connections ()
  "Display a list of all connections."
  (interactive)
  (ensime-with-popup-buffer (ensime-connections-buffer-name)
			    (ensime-connection-list-mode)
			    (ensime-draw-connection-list)))

(defun ensime-update-connection-list ()
  "Display a list of all connections."
  (interactive)
  (let ((pos (point))
	(inhibit-read-only t))
    (erase-buffer)
    (ensime-draw-connection-list)
    (goto-char pos)))

(defun ensime-draw-connection-list ()
  (let ((default-pos nil)
	(default ensime-default-connection)
	(fstring "%s%2s  %-10s  %-17s  %-7s %-s\n"))
    (insert (format fstring " " "Nr" "Name" "Port" "Pid" "Type")
	    (format fstring " " "--" "----" "----" "---" "----"))
    (dolist (p (reverse ensime-net-processes))
      (when (eq default p) (setf default-pos (point)))
      (ensime-insert-propertized 
       (list 'ensime-connection p)
       (format fstring
	       (if (eq default p) "*" " ")
	       (ensime-connection-number p)
	       (ensime-connection-name p)
	       (or (process-id p) (process-contact p))
	       (ensime-pid p)
	       (ensime-server-implementation-type p))))
    (when default 
      (goto-char default-pos))))




;; Interface Helpers

(defmacro ensime-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(defun ensime-add-face (face string)
  (add-text-properties 0 (length string) (list 'face face) string)
  string)

(defsubst ensime-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (ensime-propertize-region props (apply #'insert args)))

(defmacro ensime-with-rigid-indentation (level &rest body)
  "Execute BODY and then rigidly indent its text insertions.
Assumes all insertions are made at point."
  (let ((start (gensym)) (l (gensym)))
    `(let ((,start (point)) (,l ,(or level '(current-column))))
       (prog1 (progn ,@body)
	 (ensime-indent-rigidly ,start (point) ,l)))))

(put 'ensime-with-rigid-indentation 'lisp-indent-function 1)

(defun ensime-indent-rigidly (start end column)
  ;; Similar to `indent-rigidly' but doesn't inherit text props.
  (let ((indent (make-string column ?\ )))
    (save-excursion
      (goto-char end)
      (beginning-of-line)
      (while (and (<= start (point))
		  (progn
		    (insert-before-markers indent)
		    (zerop (forward-line -1))))))))

(defun ensime-insert-indented (&rest strings)
  "Insert all arguments rigidly indented."
  (ensime-with-rigid-indentation nil
    (apply #'insert strings)))

(defun ensime-property-bounds (prop)
  "Return two the positions of the previous and next changes to PROP.
PROP is the name of a text property."
  (assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))



(provide 'ensime)