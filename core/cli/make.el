;;; core/cli/make.el --- file generation commands -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load! "make/completions")
;; (load! "make/docs")
;; (load! "make/manpage")


;;
;;; Variables

;; (defvar doom-make-codeowners ()
;;   "TODO")


;;
;;; Commands

(defcli! make ()
  "(Re)Generate project files and boilerplate."
  :partial t)

;; TODO Finish and generalize me
(defstub! (make codeowners) ()
  "TODO"
  (print! (start "Generating CODEOWNERS file"))
  (let ((codeowners (doom-path doom-emacs-dir ".github/CODEOWNERS")))
    (with-temp-file codeowners
      (insert-file-contents codeowners)
      (when (re-search-forward "^# Don't edit this by hand!" nil t)
        (goto-char (line-end-position))
        (delete-region (point) (point-max))
        (insert "\n")
        (dolist (path (cdr (doom-module-load-path (list doom-modules-dir))))
          (when (string-match "/modules/\\([^/]+\\)/\\([^/]+\\)/$" path)
            (insert (format "%-35s @doomemacs/maintainers @doomemacs/%s-%s\n"
                            (concat (substring (match-string-no-properties 0 path) 1) "*")
                            (match-string-no-properties 1 path)
                            (match-string-no-properties 2 path)))))))))

;; TODO Finish me
(defstub! (make changelog))



;;
;;; Helpers

(defmacro doom-make--with-file (file &rest body)
  (declare (indent 1))
  `(let ((inhibit-read-only t))
     (with-current-buffer
         (or (get-file-buffer ,file)
             (find-file-noselect ,file))
       (save-excursion
         (goto-char (point-min))
         ,@body
         (when (buffer-modified-p)
           (save-buffer))))))

(provide 'core-cli-make)
;;; make.el ends here
