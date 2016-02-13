;;; helm-deft.el --- helm module for grepping note files over directories

;; Copyright (C) 2014 Derek Feichtinger

;; Author: Derek Feichtinger <dfeich@gmail.com>
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

;;; Commentary:
;; Helm command to find files fast based on contents and filename. Inspired
;; by the great emacs deft package. It allows defining a list of input directories
;; that can be defined and that are searched recursively.
;;
;; helm-deft is composed of three search sources
;; - file names: simple match of pattern vs. file name
;; - file match: shows file names of files where all of the individual patterns
;;   match anywhere in the file
;; - file contents: show the lines where the last word in the search patterns
;;   matches

;;; Code:

(require 'helm)
(require 'helm-grep)
(require 'helm-files)
(require 'f)
(require 'cl-lib)
(require 'subr-x)

(defgroup helm-deft nil
  "customization group for the helm-deft utility" :group 'helm :version 24.3)

(defcustom helm-deft-dir-list
  '("~/Documents")
  "List of directories in which to search recursively for candidate files."
  :group 'helm-deft
  )

(defcustom helm-deft-extension "org"
  "Defines file extension for identifying candidate files to be searched for.")

(defvar helm-deft-file-list nil
  "Variable to store the list of candidate files.
This is constant over the invocation of one helm-deft.")

(defvar helm-deft-matching-files '()
  "Used for building the list of filenames that the grep matched.")

(defvar helm-source-deft-fn
  '((name . "File Names")
    (header-line . "C-r: rotate pattern C-s/C-d: set/delete (marked) candidates from list")
    (init . (lambda ()
	      (progn (unless helm-deft-file-list
		       (setq helm-deft-file-list (helm-deft-fname-search)))
		     (with-current-buffer (helm-candidate-buffer 'local)
		       (insert (mapconcat 'identity
					  helm-deft-file-list "\n"))))))
    (candidates-in-buffer)
    ;; matching is done in the buffer when candidates-in-buffer is used
    ;; We only want against the basename and not the full path
    (match-part . (lambda (c) (helm-basename c)))
    ;;(type . file)
    (action . helm-find-files-actions)
    ;; We want the file list sorted. helm-highlight-files also will
    ;; transform a filename to a (basename . filename) cons
    (candidate-transformer . (lambda (c) (sort (helm-highlight-files c)
					       (lambda (a b)
						 (string< (downcase (car a))
							  (downcase (car b)))))))
    (cleanup . (lambda () (setq helm-deft-file-list nil)))
    )
  "Source definition for matching filenames of the `helm-deft' utility.")

(defun helm-deft-fname-search ()
  "Search all preconfigured directories for matching files.
Returns the filenames as a list."
  (assert helm-deft-extension nil "No file extension defined for helm-deft")
  (assert helm-deft-dir-list nil "No directories defined for helm-deft")
  (cl-loop for dir in helm-deft-dir-list
	   do (assert (file-exists-p dir) nil
		      (format "Directory %s does not exist. Check helm-deft-dir-list" dir))
	   collect (f--files dir (equal (f-ext it) helm-deft-extension) t)
	   into reslst
	   finally (return (apply #'append reslst)))
  )

(defvar helm-source-deft-filegrep
  '((name . "File Contents")
    (candidates-process . helm-deft-fgrep-search)
    ;; We use the action from the helm-grep module
    (action . helm-grep-action)
    (requires-pattern)
    (pattern-transformer . (lambda (pattern)
			     (cl-loop for ptr in (split-string pattern "  *" t)
				      if (string-prefix-p "w:" ptr)
				      collect (string-remove-prefix "w:" ptr) into cptr
				      else collect ptr into cptr
				      finally return (mapconcat 'identity cptr " "))))
    (filter-one-by-one . (lambda (candidate)
			   ;; we abuse the filter-one-by-one function
			   ;; for building the candidates list for the
			   ;; matching-files source
			   (helm-deft-matching-files-search candidate)
			   ;; we borrow the helm-grep filter function
			   (helm-grep-filter-one-by-one candidate)))
    (cleanup . (lambda () (when (get-buffer "*helm-deft-proc*")
			    (let ((kill-buffer-query-functions nil))
			      (kill-buffer "*helm-deft-proc*")))))
    )
  "Source definition for matching against file contents for the `helm-deft' utility.")

(defun helm-deft-build-cmd (ptrnstr filelst)
  "Builds a grep command based on the patterns and file list.
PTRNSTR may contain multiple search patterns separated by
spaces.  The first pattern will be used to retrieve matching
lines.  All other patterns will be used to pre-select files with
matching lines.  FILELST is a list of file paths"
  (let* ((ptrnlst (reverse (split-string ptrnstr "  *" t)))
	 (firstp (pop ptrnlst))
	 (firstaddflag (if (string-prefix-p "w:" firstp)
			   (progn
			     (setq firstp (string-remove-prefix "w:" firstp))
			     "-w")
			 ""))
	 (filelst (mapconcat 'identity filelst " "))
	 (innercmd (if ptrnlst
		       (cl-labels ((build-inner-cmd
				    (ptrnlst filelst)
				    (let* ((pattern (pop ptrnlst))
					   (addflags
					    (if (string-prefix-p "w:" pattern)
						(progn
						  (setq pattern
							(string-remove-prefix
							 "w:" pattern))
						  "-w")
					      "")))
				      (if ptrnlst
					  (format "$(grep %s -Elie '%s' %s)"
						  addflags pattern
						  (build-inner-cmd ptrnlst filelst))
					(format "$(grep %s -Elie '%s' %s)"
						addflags pattern filelst)))))
			 (build-inner-cmd ptrnlst filelst))
		     filelst)))
    (format "grep %s -EHine '%s' %s" firstaddflag firstp innercmd))
  )

(defun helm-deft-fgrep-search ()
  "Greps for the helm search pattern in the configuration defined file list."
  (setq helm-deft-matching-files '())
  ;; need to pass helm-input (the real input line) to the build
  ;; function since helm-pattern is already cleaned by the
  ;; pattern-transformer function of helm-source-deft-filegrep
  (let* ((shcmd (helm-deft-build-cmd helm-input helm-deft-file-list)))
    (helm-log "grep command: %s" shcmd)
    ;; the function must return the process object
    (prog1
	(start-process-shell-command "helm-deft-proc" "*helm-deft-proc*"
				     shcmd)
      (set-process-sentinel
       (get-process "helm-deft-proc")
       (lambda (process event)
	 (cond
	  ((string= event "finished\n")
	   (with-helm-window
	     (setq mode-line-format
		   '(" " mode-line-buffer-identification " "
		     (:eval (format "L%s" (helm-candidate-number-at-point))) " "
		     (:eval (propertize
			     ;; TODO: The count is wrong since it counts all sources
			     (format
			      "[Grep process finished - (%s results)] "
			      (max (1- (count-lines
					(point-min)
					(point-max)))
				   0))
			     'face 'helm-grep-finish))))
	     (force-mode-line-update))
	   ;; must NOT DO a targeted update here. Seems to call also this source
	   ;; and we end in an infinite loop
	   ;; (helm-update nil helm-source-deft-matching-files)
	   )
	  ;; Catch error output in log.
	  (t (helm-log
	      "Error: Grep %s"
	      (replace-regexp-in-string "\n" "" event))))
	 ))
      )
    ))

(defvar helm-source-deft-matching-files
  '((name . "Matching Files")
    (candidates . helm-deft-matching-files)
    ;;(type . file)
    ;; introducing the delayed value to always have it scheduled after
    ;; the async grep process that produces the basis for this source
    (delayed . 0.5)
    (action . helm-find-files-actions)
    ;; need to override the file type's match settings
    (match . (lambda (candidate) t))
    (candidate-transformer . (lambda (c) (sort (helm-highlight-files c)
    					       (lambda (a b)
    						 (string< (downcase (car a))
    							  (downcase (car b)))))))
    (requires-pattern)
    (volatile)
    )
  "Source definition for showing matching files from the grep buffer of the `helm-deft' utility.")

(defun helm-deft-matching-files-search (candidate)
  "Add entry to helm-deft-matching-files list from a grep CANDIDATE."
  (when (string-match "\\([^:]+\\):[0-9]+:" candidate)
    (pushnew (match-string 1 candidate) helm-deft-matching-files :test #'equal)))

;; (defun helm-deft-matching-files-search ()
;;   (when (get-buffer "*helm-deft-proc*")
;;     (with-current-buffer "*helm-deft-proc*"
;;       (beginning-of-buffer)
;;       (while (and
;; 	      (looking-at "^\\([^:]+\\):[0-9]+:")
;; 	      (not (equal (forward-line) 1)))
;; 	(push (match-string 1) helm-deft-matching-files)))
;;     (cl-remove-duplicates helm-deft-matching-files :test #'equal))
;;   )

(defun helm-deft-rotate-searchkeys ()
  "Rotate the words of the search pattern in the helm minibuffer."
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

(defun helm-deft-remove-candidate-file ()
  "Remove the file under point from the list of candidates."
  (interactive)
  ;; helm-get-selection returns current item under point
  ;; helm-marked-candidates returns all marked candidates or the item under point
  (dolist (selection (helm-marked-candidates))
    (when (string-match "\\([^:]+\\):[0-9]+:" selection)
      (setq selection (match-string 1 selection)))
    (setq helm-deft-file-list (delete selection helm-deft-file-list)))
  (helm-unmark-all)
  (helm-force-update))

(defun helm-deft-set-to-marked ()
  "Set the filelist to the marked files."
  (interactive)
  (setq helm-deft-file-list (helm-marked-candidates))
  (helm-unmark-all)
  (helm-force-update))

(defvar helm-deft-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-r") 'helm-deft-rotate-searchkeys)
    (define-key map (kbd "C-d") 'helm-deft-remove-candidate-file)
    (define-key map (kbd "C-s") 'helm-deft-set-to-marked)
    (delq nil map))
  "Helm keymap used for helm deft sources.")

;;;###autoload
(defun helm-deft ()
  "Preconfigured `helm' module for locating matching files.
Either the filename or the file contents must match the query
string.  Inspired by the Emacs `deft' extension"
  (interactive)
  (helm :sources '(helm-source-deft-fn helm-source-deft-matching-files
				       helm-source-deft-filegrep)
	:keymap helm-deft-map))

(provide 'helm-deft)
;;; helm-deft.el ends here
