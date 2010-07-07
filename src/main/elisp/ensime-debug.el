;;; ensime-debug.el
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


(defcustom ensime-db-cmd-template 
  '("jdb" "-classpath" :classpath "-sourcepath" :sourcepath :debug-class :debug-args)
  "The command to launch the debugger. Keywords will be replaced
with data loaded from server."
  :type 'string
  :group 'ensime-db)

(defcustom ensime-db-buffer-name "*ensime-db*"
  "Buffer name for debug process"
  :type 'string :group 'ensime-db)

(defcustom ensime-db-default-cmd-line '("jdb")
  "Default command to launch the debugger, used when not connected to an ENSIME
server."
  :type 'string
  :group 'ensime-db)

(defface ensime-breakpoint-face
  '((((class color) (background dark)) (:background "DarkGreen"))
    (((class color) (background light)) (:background "LightGreen2"))
    (t (:bold t)))
  "Face used for marking lines with breakpoints."
  :group 'ensime-ui)


(defvar ensime-db-history nil
  "History of argument lists passed to jdb.")

(defvar ensime-db-output-acc "")

(defvar ensime-db-output-acc-max-length 50000)

(defvar ensime-db-filter-funcs
  '(("Deferring breakpoint \\(.+\\):\\([0-9]+\\)\n>" . 
     ensime-db-handle-deferred-breakpoint)

    ("Set breakpoint \\(.+\\):\\([0-9]+\\)\n>" . 
     ensime-db-handle-set-breakpoint)

    ("Removed: breakpoint \\(.+\\):\\([0-9]+\\)\n>" . 
     ensime-db-handle-removed-breakpoint)

    ("Not found: breakpoint \\(.+\\):\\([0-9]+\\)\n>" . 
     ensime-db-handle-not-found-breakpoint)

    ("Breakpoints set:\\(?:[ \t\n]+breakpoint \\(.+\\):\\([0-9]+\\)\\)*\n>" . 
     ensime-db-handle-breakpoints-list)

    ("No breakpoints set.\n>" . 
     ensime-db-handle-empty-breakpoints-list)
    ))

(defun ensime-db-get-cmd-line ()
  "Get the command needed to launch a debugger, including all
the current project's dependencies. Returns list of form (cmd [arg]*)"
  (if (ensime-connected-p)
      (let* ((conf (ensime-rpc-debug-config)))
	(ensime-replace-keywords ensime-db-cmd-template conf))
    ensime-db-default-cmd-line))

(defun ensime-db-handle-deferred-breakpoint (str)
  (let ((class (match-string 1 str))
	(line (string-to-number (match-string 2 str))))
    (message "Deferred breakpoint: %s : %s" class line)))

(defun ensime-db-handle-set-breakpoint (str)
  (let ((class (match-string 1 str))
	(line (string-to-number (match-string 2 str))))
    (message "Set breakpoint: %s : %s" class line)))

(defun ensime-db-handle-removed-breakpoint (str)
  (let ((class (match-string 1 str))
	(line (string-to-number (match-string 2 str))))
    (message "Removed breakpoint: %s : %s" class line)))

(defun ensime-db-handle-not-found-breakpoint (str)
  (let ((class (match-string 1 str))
	(line (string-to-number (match-string 2 str))))
    (message "No breakpoint to clear at: %s : %s" class line)))

(defun ensime-db-handle-empty-breakpoints-list (str)
  (ensime-db-clear-breakpoint-overlays))

(defun ensime-db-handle-breakpoints-list (str)
  "If we find a listing of breakpoint locations, use that information
to refresh the buffer overlays."
  (ensime-db-clear-breakpoint-overlays)
  (let* ((start (match-beginning 0))
	 (end (match-end 0))
	 (pos start)
	 (bp-list '()))

    ;; Build a list of class,line pairs
    (while (and (< pos end) 
		(integerp (string-match "breakpoint \\(.+\\):\\([0-9]+\\)" str pos)))
      (setq pos (match-end 0))
      (let ((class (match-string 1 str))
	    (line (string-to-number (match-string 2 str))))
	(push (list class line) bp-list)))

    ;; Lookup source locations of class,line pairs.
    ;; Create buffer overlays...
    (let ((bp-locs (ensime-rpc-debug-class-locs-to-source-locs bp-list)))
      (dolist (bp bp-locs)
	(let ((file (car bp))
	      (line (cadr bp)))
	  (when (and (stringp file) (integerp line))
	    (when-let (ov (ensime-make-overlay-at 
			   file line nil nil 
			   "Breakpoint" 
			   'ensime-breakpoint-face))
	      (push ov ensime-db-breakpoint-overlays))))))

    ))

(defvar ensime-db-breakpoint-overlays '())

(defun ensime-db-clear-breakpoint-overlays ()
  "Remove all overlays that ensime-debug has created."
  (mapc #'delete-overlay ensime-db-breakpoint-overlays)
  (setq ensime-db-breakpoint-overlays '()))

(defun ensime-db-refresh-breakpoint-overlays ()
  "Cause debugger to output a list of all active breakpoints.
Output filter will grab this output and use it to update overlays."
  (ensime-db-send-str "clear"))


(defun ensime-db-find-first-handler (str filters)
  "Find the car in filters that matches str earliest in the text. 
Return that car's corresponding cdr (a filter function). Guarantees that
match-end, match-beginning, match-string are set correctly on return."
  (let ((best-start (length str))
	(best-filter))
    
    (dolist (filter filters)
      (let ((regexp (car filter)))
	(when (and (integerp (string-match regexp str))
		   (< (match-beginning 0) best-start))
	  (setq best-start (match-beginning 0))
	  (setq best-filter filter))))

    ;; Make sure match-end, match-beginning, match-string
    ;; are correct on return
    (when best-filter
      (string-match (car best-filter) str)
      (cdr best-filter))))


(defun ensime-db-output-filter (string)
  ;; Build up the accumulator.
  (setq ensime-db-output-acc (concat ensime-db-output-acc string))

  ;; We process STRING from left to right.  Each time through the
  ;; following loop we process at most one marker. After we've found a
  ;; marker, delete ensime-db-output-acc up to and including the match

  ;; Markers are consumed by filter functions. We assume that 
  ;; markers do not overlap.
  (catch 'done
    (while
	(let ((handler
	       (ensime-db-find-first-handler 
		ensime-db-output-acc ensime-db-filter-funcs)))
	  (if handler
	      (let ((match-end-index (match-end 0))) ;; <- save in case changed in handler
		(funcall handler ensime-db-output-acc)
		(setq ensime-db-output-acc 
		      (substring ensime-db-output-acc match-end-index)))
	    (progn
	      (throw 'done t))))))

  ;; Do not allow accumulator to grow without bound. 
  (when (> (length ensime-db-output-acc)
	   ensime-db-output-acc-max-length)
    (setq ensime-db-output-acc
	  (substring ensime-db-output-acc
		     (- (/ (* ensime-db-output-acc-max-length 3) 4)))))

  ;; We don't filter any debugger output so 
  ;; just return what we were given.
  string)



(defun ensime-db-set-break (f line)
  "Set a breakpoint in the current source file at point."
  (interactive (list buffer-file-name (line-number-at-pos (point))))
  (let* ((info (ensime-rpc-debug-unit-info (file-name-nondirectory f) line)))
    (if info
	(let ((class (plist-get info :full-name)))
	  (ensime-db-send-str (format "stop at %s:%s" class line)))
      (message "Could not find class information for given position.")))
  (ensime-db-refresh-breakpoint-overlays))


(defun ensime-db-clear-break (f line)
  "Set a breakpoint in the current source file at point."
  (interactive (list buffer-file-name (line-number-at-pos (point))))
  (let* ((info (ensime-rpc-debug-unit-info (file-name-nondirectory f) line)))
    (if info
	(let ((class (plist-get info :full-name)))
	  (ensime-db-send-str (format "clear %s:%s" class line)))
      (message "Could not find class information for given position.")))
  (ensime-db-refresh-breakpoint-overlays))


(defun ensime-db-list-breakpoints (f line)
  "Cause debugger to output a list of all active breakpoints. Note, this will
cause the output filter to refresh the breakpoint overlays."
  (ensime-db-send-str "clear"))

(defun ensime-db-send-str (str &optional no-newline)
  "Sends a string to the debug process. Automatically append a newline."
  (interactive)
  (comint-send-string (get-buffer ensime-db-buffer-name) 
		      (concat str (if no-newline "" "\n"))))


(defun ensime-db-start ()
  "Run a Scala interpreter in an Emacs buffer"
  (interactive)

  (let* ((conn (or (ensime-current-connection)
		   (ensime-prompt-for-connection)))
	 (root-path (or (ensime-configured-project-root) "."))
	 (cmd-line (ensime-db-get-cmd-line)))

    (switch-to-buffer-other-window 
     (get-buffer-create ensime-db-buffer-name))

    (comint-mode)

    (set (make-local-variable 'comint-prompt-regexp) "^> \\|^[^ ]+\\[[0-9]+\\] ")
    (set (make-local-variable 'comint-process-echoes) nil)
    (set (make-local-variable 'comint-scroll-to-bottom-on-output) t)
    (set (make-local-variable 'comint-prompt-read-only) t)
    (set (make-local-variable 'comint-output-filter-functions)
	 '(ensime-db-output-filter comint-postoutput-scroll-to-bottom))

    (setq ensime-db-output-acc "")
    (setq ensime-buffer-connection conn)

    (add-hook 'kill-buffer-hook 'ensime-db-clear-breakpoint-overlays nil t)
    (ensime-db-clear-breakpoint-overlays)

    (cd root-path)
    (comint-exec (current-buffer) 
		 "ensime-debug-cmd" 
		 (car cmd-line)
		 nil (cdr cmd-line))

    (let ((proc (get-buffer-process (current-buffer))))
      (ensime-set-query-on-exit-flag proc))))



(provide 'ensime-debug)