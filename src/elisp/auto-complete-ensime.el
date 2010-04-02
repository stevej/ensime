(require 'auto-complete)

(defun ac-ensime-move-point-back-to-call-target ()
  "Assuming the point is in a member prefix, move the point back so it's
   at the last char of the call target.
  "
  ;; TODO Make this smarter!
  (re-search-backward "\\." (point-at-bol) t)
  ;;  (skip-syntax-backward "w_")
  (goto-char (1- (point))))

(defun ac-ensime-candidates (prefix)
  "Return candidate list."
  (ensime-save-buffer-no-hook)
  (save-excursion
    (ac-ensime-move-point-back-to-call-target)
    (let ((members (ensime-members-for-type-at-point prefix)))
      (mapcar (lambda (m)
		(let ((name (plist-get m :name))
		      (type (plist-get m :type)))
		  ;; Save the type for later display
		  (propertize name 'scala-type type))
		) members))))

(defun ac-ensime-get-member-doc (item)
  "Return doc for given item."
  (get-text-property 0 'scala-type item))

(ac-define-source ensime
  '((document . ac-ensime-get-member-doc)
    (candidates . (ac-ensime-candidates ac-prefix))
    (prefix . c-dot)
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