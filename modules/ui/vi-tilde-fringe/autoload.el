;;; ui/vi-tilde-fringe/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
           #'vi-tilde-fringe-mode)
