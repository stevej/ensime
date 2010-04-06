(require 'auto-complete)

(defun ac-ensime-move-point-back-to-call-target (prefix)
  "Assuming the point is in a member prefix, move the point back so it's
   at the last char of the call target.
  "
  (backward-char (length prefix))
  (re-search-backward "[^\\. ]" (point-at-bol) t))

(defun ac-ensime-candidates (prefix)
  "Return candidate list."
  (ensime-save-buffer-no-hook)
  (save-excursion
    (ac-ensime-move-point-back-to-call-target prefix)
    (let ((members (ensime-members-for-type-at-point prefix)))
      (mapcar (lambda (m)
		(let ((name (plist-get m :name))
		      (type-name (plist-get m :type-name))
		      (type-id (plist-get m :type-id)))
		  ;; Save the type for later display
		  (propertize name 'scala-type-name type-name 'scala-type-id type-id))
		) members))))

(defun ac-ensime-get-member-doc (item)
  "Return doc for given item."
  (get-text-property 0 'scala-type-name item))

(defun ac-ensime-member-prefix ()
  "C-like languages dot(.) prefix."
  (let ((point (re-search-backward "[\\. ]+\\([^\\. ]*\\)?" nil t)))
    (if point (1+ point))))

(defun ac-ensime-member-complete-action ()
  "Defines action to perform when user selects a completion candidate.
   In this case, if the candidate is a method name, fill in place-holder
   arguments."
  (let* ((candidate candidate) ;;Grab from dynamic environment..
	 (type-id (get-text-property 0 'scala-type-id candidate))
	 (type (ensime-get-type-by-id type-id)))
    (if (and type
	     (ensime-type-is-arrow type) 
	     (ensime-type-param-types type))
	(let* ((i -1)
	       (arg-str (mapconcat 
			 (lambda(p)(progn
				     (incf i)
				     (format 
				      "arg%s:%s" 
				      i (ensime-type-name p))))
			 (ensime-type-param-types type)
			 ", "
			 )))
	  (save-excursion
	    (insert (concat "(" arg-str ")" )))
	  (forward-char)
	  ))))

(ac-define-source ensime
  '((document . ac-ensime-get-member-doc)
    (candidates . (ac-ensime-candidates ac-prefix))
    (prefix . ac-ensime-member-prefix)
    (action . ac-ensime-member-complete-action)
    (requires . 0)
    (symbol . "f")
    (cache . t)
    ))

(defun ac-ensime-enable ()
  (setq ac-sources '(ac-source-ensime))
  (setq ac-quick-help-delay 1.0)
  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB")
  (auto-complete-mode 1)
  )

(defun ac-ensime-disable ()
  (setq ac-quick-help-delay 1.5)
  (auto-complete-mode 0)
  )

(provide 'auto-complete-ensime)