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
    (if (looking-back "[(\\[\\,\\;\\}\\{\n]\\s-*\\(\\w+\\)" pt-at-end-of-prev-line)
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
    (insert name)
    (if (and type
	     (ensime-type-is-arrow type) 
	     param-types)
	(let* ((i -1)
	       (arg-str (mapconcat 
			 (lambda(p)(progn
				     (incf i)
				     (format 
				      "arg%s:%s" 
				      i (ensime-type-name p))))
			 param-types
			 ", "
			 )))
	  (save-excursion
	    (if (and (= 1 (length param-types)) 
		     (null (string-match "[A-z]" name)))
		;; Probably an operator..
		(insert (concat " " arg-str))
	      ;; Probably a normal method call
	      (insert (concat "(" arg-str ")" ))))
	  (forward-char)
	  ))))



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