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


(defun ensime-refactor-notify-failure (result)
  (message "Refactoring failed: %s" (plist-get result :reason)))

(defun ensime-refactor-organize-imports (&optional file)
  (interactive)
  (let ((filename (or file (buffer-file-name))))
    (incf ensime-refactor-id-counter)
    (ensime-rpc-refactor-prep 
     ensime-refactor-id-counter
     'organizeImports
     `(file ,filename)
     'ensime-refactor-organize-imports-step-2
     )))

(defun ensime-refactor-organize-imports-step-2 (result)
  (let ((status (plist-get result :status))
	(id (plist-get result :procedure-id)))
    (if (equal status 'success)
	(ensime-rpc-refactor-perform 
	 id
	 'organizeImports
	 '()
	 'ensime-refactor-organize-imports-step-3
	 )
      (ensime-refactor-notify-failure result)
      )))

(defun ensime-refactor-organize-imports-step-3 (result)
  (let ((status (plist-get result :status))
	(id (plist-get result :procedure-id)))
    (if (equal status 'success)
	(ensime-rpc-refactor-exec 
	 id
	 'organizeImports
	 '()
	 '(lambda (result) (message "%s" result))
	 )
      (ensime-refactor-notify-failure result)
      )))



(provide 'ensime-refactor)