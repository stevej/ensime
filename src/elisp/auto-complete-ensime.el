(require 'auto-complete)

(defun ac-ensime-move-point-back-to-call-target (prefix)
  "Assuming the point is in a member prefix, move the point back so it's
   at the last char of the call target.
  "
  (backward-char (length prefix))
  (re-search-backward "[^\\. ]" (point-at-bol) t))

(defun ac-ensime-member-candidates (prefix)
  "Return candidate list."
  (ensime-save-buffer-no-hook)
  (save-excursion
    (ac-ensime-move-point-back-to-call-target prefix)
    (let ((members (ensime-rpc-members-for-type-at-point prefix)))
      (mapcar (lambda (m)
		(let* ((type-name (plist-get m :type-name))
		       (type-id (plist-get m :type-id))
		       (name (plist-get m :name))
		       (candidate (concat name ":" type-name)))
		  ;; Save the type for later display
		  (propertize candidate
			      'symbol-name name
			      'scala-type-name type-name 
			      'scala-type-id type-id))
		) members))))

(defun ac-ensime-name-candidates (prefix)
  "Return candidate list."
  (ensime-save-buffer-no-hook)
  (let ((names (ensime-rpc-name-completions-at-point prefix)))
    (mapcar (lambda (m)
	      (let* ((type-name (plist-get m :type-name))
		     (type-id (plist-get m :type-id))
		     (name (plist-get m :name))
		     (candidate (concat name ":" type-name)))
		;; Save the type for later display
		(propertize candidate
			    'symbol-name name
			    'scala-type-name type-name 
			    'scala-type-id type-id))
	      ) names)))


(defun ac-ensime-get-doc (item)
  "Return doc for given item."
  (get-text-property 0 'scala-type-name item))

(defun ac-ensime-member-prefix ()
  "Starting at current point. Find the point of completion for a member access. 
   Return nil if we are not currently looking at a member access."
  (let ((point (re-search-backward "[\\. ]+\\([^\\. ]*\\)?" (point-at-bol) t)))
    (if point (1+ point))))

(defun ac-ensime-name-prefix ()
  "Starting at current point. Find the point of completion for a symbol.
   Return nil if we are not currently looking at a symbol."
  (let ((pt-at-end-of-prev-line
	 (save-excursion (forward-line -1)(point-at-eol))))
    (if (looking-back "[(\\[\\,\\;\\}\\{\n]\\s-*\\(?:new\\)?\\s-*\\(\\w+\\)" pt-at-end-of-prev-line)
	(let ((point (- (point) (length (match-string 1)))))
	  (goto-char point)
	  point
	  ))))

(defun ac-ensime-complete-action ()
  "Defines action to perform when user selects a completion candidate.
   In this case, if the candidate is a method name, fill in place-holder
   arguments."
  (let* ((candidate candidate) ;;Grab from dynamic environment..
	 (type-id (get-text-property 0 'scala-type-id candidate))
	 (type (ensime-rpc-get-type-by-id type-id))
	 (name (get-text-property 0 'symbol-name candidate))
	 (param-types (ensime-type-param-types type)))
    (kill-backward-chars (length candidate))
    (let ((name-start-point (point)))
      (insert name)
      (if (and type (ensime-type-is-arrow type) param-types)
	  (let* ((param-list 
		  (mapcar 
		   (lambda(pt)
		     (list "arg" (ensime-type-name pt))) param-types)))

	    ;; Save param list information as a text property..
	    (add-text-properties name-start-point 
				 (+ name-start-point (length name))
				 (list 'param-list param-list))

	    ;; Insert space or parens depending on the nature of the
	    ;; call
	    (save-excursion
	      (if (and (= 1 (length param-types))
		       (null (string-match "[A-z]" name)))
		  ;; Probably an operator..
		  (insert " ")
		;; Probably a normal method call
		(insert "()" )))

	    ;; Setup hook function to show param help later..
	    (add-hook 'post-command-hook 'ensime-ac-update-param-help nil t)

	    ;; This command should trigger help hook..
	    (forward-char)
	    )))))


(defun ensime-ac-get-active-param-list ()
  "Search backward from point for the param list that 
   we are currently completing."
  (save-excursion
    (catch 'return 
      (let ((lbound (point-at-bol)) ;; TODO <-- what about multiline param lists
	    (balance 0))
	(backward-char 1)
	(while (> (point) lbound)
	  (cond
	   ((ensime-in-string-or-comment (point)) nil)
	   ((looking-at "\\s)") (decf balance))
	   ((looking-at "\\s(") (incf balance))
	   (t
	    (let ((prop (get-text-property (point) 'param-list)))
	      (if (and (or (= 1 balance)) prop)
		  (throw 'return (list (point) prop))))))
	  (backward-char 1))))))


(defun ensime-ac-update-param-help ()
  "When entering the arguments to a call, display a tooltip
   with the param names and types of the call."
  (let ((desc (ensime-ac-get-active-param-list)))
    (if desc
	(let* ((name-end (car desc))
	       (param-list (cadr desc))
	       (param-str (mapconcat
			   (lambda (p)
			     (format "%s:%s"(car p)(cadr p)))
			   param-list ", ")))
	  (message (concat "( " param-str " )")))
      (remove-hook 'post-command-hook 'ensime-ac-update-param-help t))))


(ac-define-source ensime-members
  '((document . ac-ensime-get-doc)
    (candidates . (ac-ensime-member-candidates ac-prefix))
    (prefix . ac-ensime-member-prefix)
    (action . ac-ensime-complete-action)
    (requires . 0)
    (symbol . "f")
    (cache . t)
    ))

(ac-define-source ensime-scope-names
  '((document . ac-ensime-get-doc)
    (candidates . (ac-ensime-name-candidates ac-prefix))
    (prefix . ac-ensime-name-prefix)
    (action . ac-ensime-complete-action)
    (requires . 0)
    (symbol . "s")
    (cache . t)
    ))

(defun ac-ensime-enable ()
  (make-local-variable 'ac-sources)
  (setq ac-sources '(ac-source-ensime-scope-names
		     ac-source-ensime-members ))

  (make-local-variable 'ac-quick-help-delay)
  (setq ac-quick-help-delay 1.0)

  (make-local-variable 'ac-auto-start)
  (setq ac-auto-start nil)

  (make-local-variable 'ac-trigger-key)
  (ac-set-trigger-key "TAB")

  (auto-complete-mode 1)
  )

(defun ac-ensime-disable ()
  (auto-complete-mode 0)
  )

(provide 'auto-complete-ensime)