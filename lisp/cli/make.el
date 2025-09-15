;;; lisp/cli/make.el --- file generation commands -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load! "make/completions")
;; (load! "make/docs")
;; (load! "make/manpage")


;;
;;; Variables



;;
;;; Commands

(defcli! make
    ((dryrun? ("--dryrun"))
     (output ("-o" "--output" path))
     &args targets)
  "Run make targets (defined in .doom)."
  (when dryrun?
    (print! (warn "Running dry run")))
  (let* ((root (doom-git-toplevel))
         (rc (doom-rcfile-read `(project make) root)))
    (dolist (target targets)
      (with-temp-buffer
        (let* ((outfile
                (unless (equal output "-")
                  (expand-file-name target)))
               (name (file-relative-name (file-truename target) root))
               (rule (cdr (assoc name rc))))
          (if (null rule)
              (print! (warn "No known make rule for: %s" name))
            (dolist (entry rule)
              (cond ((stringp entry)
                     (insert entry "\n"))
                    ((functionp entry)
                     (funcall entry rc))))
            (if (null outfile)
                (print! "%s" (buffer-string))
              (print! (start "Wrote %s") name)
              (unless dryrun?
                (write-region (buffer-string) nil outfile)))))))))

;; TODO Finish me
(defcli-stub! (make changelog))



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

(provide 'doom-cli-make)
;;; make.el ends here
