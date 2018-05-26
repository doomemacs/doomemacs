;;; ui/vi-tilde-fringe/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-hook! (prog-mode text-mode conf-mode) #'vi-tilde-fringe-mode)
