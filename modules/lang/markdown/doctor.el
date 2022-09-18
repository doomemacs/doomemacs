;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/markdown/doctor.el

(when (require 'markdown-mode nil t)
  (cond ((eq markdown-command #'+markdown-compile)
         (unless (cl-loop for (exe . cmd) in (list (cons "marked" '+markdown-compile-marked)
                                           (cons "pandoc" '+markdown-compile-pandoc)
                                           (cons "markdown" '+markdown-compile-markdown)
                                           (cons "multimarkdown" '+markdown-compile-multimarkdown))
                          if (and (memq cmd +markdown-compile-functions)
                                  (executable-find exe))
                          return t)
           (warn! "Couldn't find a markdown compiler, `markdown-preview' won't work")))
        ((stringp markdown-command)
         (let ((cmd (car (split-string markdown-command " "))))
           (unless (executable-find cmd)
             (warn! "Couldn't find %S. markdown-preview command won't work"
                    cmd))))))

(when (modulep! +grip)
  (unless (executable-find "grip")
    (warn! "Couldn't find grip. grip-mode will not work")))
