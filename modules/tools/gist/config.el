;;; tools/gist/config.el -*- lexical-binding: t; -*-

;; NOTE On occasion, the cache gets corrupted, causing wrong-type-argument
;; errors. If that happens, try `+gist/kill-cache'. You may have to restart
;; Emacs.

(after! gist
  (set-evil-initial-state! 'gist-list-mode 'normal)

  (defun +gist*list-render (orig-fn &rest args)
    (funcall orig-fn (car args) t)
    (unless (cadr args)
      (pop-to-buffer (current-buffer))))
  (advice-add #'gist-list-render :around #'+gist*list-render))
