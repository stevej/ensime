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


(require 'gud)

(defcustom gud-ensime-db-cmd-template 
  '("jdb" "-classpath" :classpath "-sourcepath" :sourcepath :debug-class :debug-args)
  "The command to launch the debugger. Keywords will be replaced
with data loaded from server."
  :type 'string
  :group 'gud-ensime-db)


(defcustom gud-ensime-db-default-cmd-line '("jdb")
  "Default command to launch the debugger, used when not connected to an ENSIME
server."
  :type 'string
  :group 'gud-ensime-db)


(defun gud-ensime-db-make-cmd-string (cmd-line)
  "Concatenate the elements of cmd-line to create the command line.
Due to expectations of gud-mode, flags with leading '-' must be concatenated to 
their values without intervening space."
  (let ((str "")
	(prev ""))
    (dolist (ea cmd-line)
      (cond
       ((integerp (string-match "^-" prev)) (setq str (concat str ea)))
       (t (setq str (concat str " " (or ea "")))))
      (setq prev ea))
    str
    ))

(defun gud-ensime-db-get-cmd-line ()
  "Get the command needed to launch a debugger, including all
the current project's dependencies. Returns list of form (cmd [arg]*)"
  (if (ensime-connected-p)
      (let* ((conf (ensime-rpc-debug-config)))
	(ensime-replace-keywords gud-ensime-db-cmd-template conf))
    gud-ensime-db-default-cmd-line))


(defvar gud-ensime-db-history nil
  "History of argument lists passed to jdb.")

;; Note: Reset to this value every time a prompt is seen
(defvar gud-ensime-db-lowest-stack-level 999)

;; See comentary for other debugger's marker filters - there you will find
;; important notes about STRING.
(defun gud-ensime-db-marker-filter (string)

  ;; Build up the accumulator.
  (setq gud-marker-acc
	(if gud-marker-acc
	    (concat gud-marker-acc string)
	  string))

  ;; We process STRING from left to right.  Each time through the
  ;; following loop we process at most one marker. After we've found a
  ;; marker, delete gud-marker-acc up to and including the match
  (let (file-found)
    ;; Process each complete marker in the input.
    (while

	;; Do we see a marker?
	(string-match
	 ;; jdb puts out a string of the following form when it
	 ;; hits a breakpoint:
	 ;;
	 ;;	<fully-qualified-class><method> (<class>:<line-number>)
	 ;;
	 ;; <fully-qualified-class>'s are composed of Java ID's
	 ;; separated by periods.  <method> and <class> are
	 ;; also Java ID's.  <method> begins with a period and
	 ;; may contain less-than and greater-than (constructors,
	 ;; for instance, are called <init> in the symbol table.)
	 ;; Java ID's begin with a letter followed by letters
	 ;; and/or digits.  The set of letters includes underscore
	 ;; and dollar sign.
	 ;;
	 ;; The first group matches <fully-qualified-class>,
	 ;; the second group matches <class> and the third group
	 ;; matches <line-number>.  We don't care about using
	 ;; <method> so we don't "group" it.
	 ;;
	 ;; FIXME: Java ID's are UNICODE strings, this matches ASCII
	 ;; ID's only.
         ;;
         ;; The ".," in the last square-bracket are necessary because
         ;; of Sun's total disrespect for backwards compatibility in
         ;; reported line numbers from jdb - starting in 1.4.0 they
         ;; print line numbers using LOCALE, inserting a comma or a
         ;; period at the thousands positions (how ingenious!).

	 "\\(\\[[0-9]+] \\)*\\([a-zA-Z0-9.$_]+\\)\\.[a-zA-Z0-9$_<>(),]+ \
\\(([a-zA-Z0-9.$_]+:\\|line=\\)\\([0-9.,]+\\)"
	 gud-marker-acc)

      ;; A good marker is one that:
      ;; 1) does not have a "[n] " prefix (not part of a stack backtrace)
      ;; 2) does have an "[n] " prefix and n is the lowest prefix seen
      ;;    since the last prompt
      ;; Figure out the line on which to position the debugging arrow.
      ;; Return the info as a cons of the form:
      ;;
      ;;     (<file-name> . <line-number>) .
      (if (if (match-beginning 1)
	      (let (n)
		(setq n (string-to-number (substring
					   gud-marker-acc
					   (1+ (match-beginning 1))
					   (- (match-end 1) 2))))
		(if (< n gud-ensime-db-lowest-stack-level)
		    (progn (setq gud-ensime-db-lowest-stack-level n) t)))
	    t)
	  (if (setq file-found
		    (gud-ensime-db-find-source (match-string 2 gud-marker-acc)))
	      (setq gud-last-frame
		    (cons file-found
			  (string-to-number
			   (let
                               ((numstr (match-string 4 gud-marker-acc)))
                             (if (string-match "[.,]" numstr)
                                 (replace-match "" nil nil numstr)
                               numstr)))))
	    (message "Could not find source file.")))

      ;; Set the accumulator to the remaining text.
      (setq gud-marker-acc (substring gud-marker-acc (match-end 0))))

    (if (string-match comint-prompt-regexp gud-marker-acc)
	(setq gud-ensime-db-lowest-stack-level 999)))

  ;; Do not allow gud-marker-acc to grow without bound. If the source
  ;; file information is not within the last 3/4
  ;; gud-marker-acc-max-length characters, well,...
  (if (> (length gud-marker-acc) gud-marker-acc-max-length)
      (setq gud-marker-acc
	    (substring gud-marker-acc
		       (- (/ (* gud-marker-acc-max-length 3) 4)))))

  ;; We don't filter any debugger output so just return what we were given.
  string)


(defun gud-ensime-db-stop-here ()
  "Set a breakpoint in the current source file at point."
  (interactive)
  (gud-ensime-db-stop buffer-file-name (line-number-at-pos (point))))

(defun gud-ensime-db-stop (f line)
  (let* ((info (ensime-rpc-debug-unit-info (file-name-nondirectory f) line)))
    (when info
      (let ((class (plist-get info :full-name)))
	(gud-basic-call (format "stop at %s:%s" class line))))
    ))

(defun gud-ensime-db-start ()
  "Run a Scala interpreter in an Emacs buffer"
  (interactive)
  (let* ((root-path (or (ensime-configured-project-root) "."))
	 (cmd-line (gud-ensime-db-get-cmd-line))
	 (command-line (gud-ensime-db-make-cmd-string cmd-line)))

    (gud-common-init command-line 'gud-jdb-massage-args 'gud-ensime-db-marker-filter)
    (set (make-local-variable 'gud-minor-mode) 'ensime-db)

    (gud-def gud-break  "stop at %c:%l" "\C-b" "Set breakpoint at current line.")
    (gud-def gud-remove "clear %c:%l"   "\C-d" "Remove breakpoint at current line")
    (gud-def gud-step   "step"          "\C-s" "Step one source line with display.")
    (gud-def gud-next   "next"          "\C-n" "Step one line (skip functions).")
    (gud-def gud-cont   "cont"          "\C-r" "Continue with display.")
    (gud-def gud-finish "step up"       "\C-f" "Continue until current method returns.")
    (gud-def gud-up     "up\C-Mwhere"   "<"    "Up one stack frame.")
    (gud-def gud-down   "down\C-Mwhere" ">"    "Up one stack frame.")
    (gud-def gud-run    "run"           nil    "Run the program.") ;if VM start using jdb
    (gud-def gud-print  "print %e"  "\C-p" "Evaluate Java expression at point.")

    (setq comint-prompt-regexp "^> \\|^[^ ]+\\[[0-9]+\\] ")
    (setq paragraph-start comint-prompt-regexp)

    ))


(provide 'ensime-debug)