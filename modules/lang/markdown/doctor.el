;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/markdown/doctor.el

(when (require 'markdown-mode nil t)
  (cond ((eq markdown-command #'+markdown-compile)
         (dolist (cmd (list (cons "marked" '+markdown-compile-marked)
                            (cons "pandoc" '+markdown-compile-pandoc)
                            (cons "markdown" '+markdown-compile-markdown)))
           (when (and (memq (cdr cmd) +markdown-compile-functions)
                      (not (executable-find (car cmd))))
             (warn! "Couldn't find %S. markdown-preview command won't work"
                    (car cmd)))))
        ((stringp markdown-command)
         (let ((cmd (car (split-string markdown-command " "))))
           (unless (executable-find cmd)
             (warn! "Couldn't find %S. markdown-preview command won't work"
                    cmd))))))
