;;; helm-deft.el --- helm module for grepping note files over directories

;; Copyright (C) 2014 Derek Feichtinger
 
;; Author: Derek Feichtinger <derek.feichtinger@psi.ch>
;; Keywords: convenience
;; Homepage: https://github.com/dfeich/helm-deft
;; Version: TODO
;; Package-Requires: ((helm "1.7.7") (f "0.17.0") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'helm)
(require 'helm-grep)
(require 'helm-files)
(require 'f)
(require 'cl-lib)

(defgroup helm-deft nil
  "customization group for the helm-deft utility" :group 'helm :version 24.3)

(defcustom helm-deft-dir-list
  '("~/Documents")
  "list of directories in which to search recursively for candidate files"
  :group 'helm-deft
  )

(defcustom helm-deft-extension "org"
  "defines file extension for identifying candidate files to be searched for")

(defvar helm-deft-file-list ""
  "variable to store the list of candidate files")

(defvar helm-source-deft-fn
  '((name . "File Names")
    (init . (lambda ()
	      (progn (setq helm-deft-file-list (helm-deft-fname-search))
		     (with-current-buffer (helm-candidate-buffer 'local)
			  (insert (mapconcat 'identity
					     helm-deft-file-list "\n"))))))
    (candidates-in-buffer)
    ;; matching is done in the buffer when candidates-in-buffer is used
    ;; We only want against the basename and not the full path
    (match-part . (lambda (c) (helm-basename c)))
    (type . file)
    ;; Note: We override the transformer that the file type brings. We
    ;; want the file list sorted
    (candidate-transformer . (lambda (c) (sort (helm-highlight-files c)
					       (lambda (a b)
						 (string< (downcase (car a))
							  (downcase (car b)))))))
    ;; (action . (("open file" . (lambda (candidate)
    ;; 				(find-file candidate)))))
    ;;(persistent-help . "show name")    
    )
  "Source definition for matching filenames of the `helm-deft' utility")

(defvar helm-source-deft-filegrep
  '((name . "File Contents")
    (candidates-process . helm-deft-fgrep-search)
    ;; We use the action from the helm-grep module
    (action . helm-grep-action)
    (requires-pattern)
    (filter-one-by-one . helm-grep-filter-one-by-one)
    (cleanup . (lambda () (when (get-buffer "*helm-deft-proc*")
			    (let ((kill-buffer-query-functions nil))
			      (kill-buffer "*helm-deft-proc*")))))
    )
  "Source definition for matching against file contents for the
  `helm-deft' utility")

(defun helm-deft-rotate-searchkeys ()
  "rotate the words of the search pattern in the helm minibuffer"
  (interactive)
  (helm-log "Executing helm-deft-rotate-searchkeys")
  (let ((patlst (split-string helm-pattern "  *")))
    (when (and (>= (length patlst) 1)
	       (> (length (car patlst)) 0))
      (delete-minibuffer-contents)
      (insert (mapconcat #'identity
			 (append (cdr patlst) (list (car patlst)))
			 " "))
      (helm-update)))
  )

(defvar helm-deft-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-r") 'helm-deft-rotate-searchkeys)
    (delq nil map))
  "helm keymap used for helm deft sources")

(defun helm-deft-fname-search ()
  "search all preconfigured directories for matching files and return the
filenames as a list"
  (cl-assert helm-deft-extension nil "No file extension defined for helm-deft")
  (cl-assert helm-deft-dir-list nil "No directories defined for helm-deft")
  (cl-loop for dir in helm-deft-dir-list
	   do (cl-assert (file-exists-p dir) nil
			 (format "Directory %s does not exist. Check helm-deft-dir-list" dir))
	   collect (f--files dir (equal (f-ext it) helm-deft-extension) t)
	   into reslst
	   finally (return (apply #'append reslst)))  
  )

(defun helm-deft-build-cmd (ptrnstr filelst)
  "Builds a grep command where PTRNSTR may contain multiple search patterns
separated by spaces. The first pattern will be used to retrieve matching lines.
All other patterns will be used to pre-select files with matching lines.
FILELST is a list of file paths"
  (let* ((ptrnlst (remove "" (reverse (split-string ptrnstr "  *"))))
	 (firstp (pop ptrnlst))
	 (filelst (mapconcat 'identity filelst " "))
	 (innercmd (if ptrnlst
		       (cl-labels ((build-inner-cmd
				    (ptrnlst filelst)
				    (let ((pattern (pop ptrnlst)))
				      (if ptrnlst
					  (format "$(grep -Elie \"%s\" %s)" pattern
						  (build-inner-cmd ptrnlst filelst))
					(format "$(grep -Elie \"%s\" %s)"
						pattern filelst)))))
			 (build-inner-cmd ptrnlst filelst))
		     filelst)))
    (format "grep -EHine \"%s\" %s" firstp innercmd))
  )

(defun helm-deft-fgrep-search ()
  "greps for the helm search pattern in the configuration defined
file list"
  (let* ((shcmd (helm-deft-build-cmd helm-pattern helm-deft-file-list)))
    (helm-log "grep command: %s" shcmd)
    (start-process-shell-command "helm-deft-proc" "*helm-deft-proc*"
				 shcmd))
  )

;;;###autoload
(defun helm-deft ()
  "Preconfigured `helm' module for locating note files where either the
filename or the file contents match the query string. Inspired by the
emacs `deft' extension"
  (interactive)
  (helm :sources '(helm-source-deft-fn helm-source-deft-filegrep)
	:keymap helm-deft-map))

(provide 'helm-deft)
;;; helm-deft.el ends here
