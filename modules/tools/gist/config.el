;;; tools/gist/config.el -*- lexical-binding: t; -*-

;; NOTE On occasion, the cache gets corrupted, causing wrong-type-argument
;; errors. If that happens, try `+gist/kill-cache'. You may have to restart
;; Emacs.

(after! gist
  (set-evil-initial-state! 'gist-list-mode 'normal)

  (set-popup-rule! "^\\*gist-" :ignore t)

  (def-advice! +gist-list-render-a (orig-fn &rest args)
    :around #'gist-list-render
    (funcall orig-fn (car args) t)
    (unless (cadr args)
      (pop-to-buffer (current-buffer)))))
