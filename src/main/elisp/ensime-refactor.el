;;; ensime-refactor.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
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


(defvar ensime-refactor-id-counter 0
  "Each refactoring is given a unique id.")

(defvar ensime-refactor-info-buffer-name "*Ensime Refactoring*")

(defvar ensime-refactor-info-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") (lambda()(interactive)
				(funcall continue-refactor)
				(ensime-popup-buffer-quit-function)
				))
    (define-key map (kbd "q") (lambda()(interactive)
				(funcall cancel-refactor)
				(ensime-popup-buffer-quit-function)
				))
    map)
  "Type and package inspector specific key bindings 
   (in addition to those defined by popup-buffer-mode)")


(defmacro* ensime-refactor-with-info-buffer ((&optional select) &body body)
  "Extend the standard popup buffer with inspector-specific bindings."
  `(ensime-with-popup-buffer
    (,ensime-refactor-info-buffer-name t ,select)
    (use-local-map ensime-refactor-info-map)
    ,@body
    ))


(defun ensime-refactor-notify-failure (result)
  (message "Refactoring failed: %s" (plist-get result :reason)))


(defun ensime-refactor-organize-imports ()
  "Do a syntactic organization of the imports in the current buffer."
  (interactive)
  (ensime-refactor-perform 
   'organizeImports 
   `(file ,buffer-file-name start ,(point-min) end ,(point-max))))


(defun ensime-refactor-rename ()
  "Rename a symbol, project-wide."
  (interactive)
  (let ((start nil)
	(end nil))
    (save-excursion
      (search-backward-regexp "\\W" (point-at-bol))
      (setq start (+ (point) 1)))
    (save-excursion
      (search-forward-regexp "\\W" (point-at-eol))
      (setq end (- (point) 1)))

    (let* ((old-name (buffer-substring-no-properties start end))
	   (name (read-string (format "Rename '%s' to: " old-name))))

      (ensime-refactor-perform 
       'rename 
       `(file ,buffer-file-name start ,start end ,end newName ,name)))))


(defun ensime-refactor-perform (refactor-type params)
  (ensime-assert-buffer-saved-interactive
   (incf ensime-refactor-id-counter)
   (message "Please wait...")
   (ensime-rpc-refactor-perform 
    ensime-refactor-id-counter
    refactor-type
    params
    'ensime-refactor-perform-handler
    )))

(defun ensime-refactor-perform-handler (result)
  (let ((refactor-type (plist-get result :refactor-type))
	(status (plist-get result :status))
	(id (plist-get result :procedure-id))
	(changes (plist-get result :changes)))
    (if (equal status 'success)
	(let ((cont `(lambda () (ensime-rpc-refactor-exec
				 ,id ',refactor-type
				 'ensime-refactor-handle-result)))
	      (cancel `(lambda () (ensime-rpc-refactor-cancel ,id))))

	  (ensime-refactor-with-info-buffer
	   (t)
	   (set (make-local-variable 'cancel-refactor) cancel)
	   (set (make-local-variable 'continue-refactor) cont)
	   (ensime-refactor-populate-confirmation-buffer 
	    refactor-type changes)
	   (goto-char (point-min))
	   ))
      
      (ensime-refactor-notify-failure result)
      )))


(defun ensime-refactor-handle-result (result)
  (let ((touched (plist-get result :touched-files)))
    (dolist (f touched)
      (when-let (buf (find-buffer-visiting f))
	(with-current-buffer buf
	  (revert-buffer t t)
	  (ensime-typecheck-current-file))
	))))


(defun ensime-refactor-populate-confirmation-buffer (refactor-type changes)
  (let ((header 
	 "Please review the proposed changes."))

    (ensime-insert-with-face 
     (concat header " (c to confirm, q to cancel)")
     'font-lock-constant-face)

    (ensime-insert-with-face "\n----------------------------------------\n\n"
			     'font-lock-comment-face)
    (dolist (ch changes)
      (let* ((file (plist-get ch :file))
	     (text (plist-get ch :text))
	     (from (plist-get ch :from))
	     (to (plist-get ch :to))
	     (len (- to from)))

	(ensime-insert-with-face file 'font-lock-comment-face)
	(insert "\n")
	(insert "....\n")
	(let* ((p (point))
	       (result (ensime-refactor-file-text-range
			file (- from 200) (+ to 200)))
	       (expanded-text (plist-get result :text))
	       (real-start (plist-get result :real-start))
	       (real-end (plist-get result :real-end)))
	  (insert expanded-text)
	  (goto-char (+ p (- from real-start)))
	  (delete-char (min len (- (point-max) (point))))
	  (ensime-insert-with-face text 'font-lock-keyword-face))
	(goto-char (point-max))
	(insert "\n....")
	(insert "\n\n")))))


  (defun ensime-refactor-file-text-range (file-name start end)
    "Return the text of the given file from start to end."
    (with-temp-buffer 
      (insert-file-contents file-name)
      (let* ((real-start (max start (point-min)))
	     (real-end (min end (point-max)))
	     (text (buffer-substring-no-properties real-start real-end)))
	`(:text ,text :real-start ,real-start :real-end real-end))))



  (provide 'ensime-refactor)