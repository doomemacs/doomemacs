;;; tools/gist/config.el -*- lexical-binding: t; -*-

;; NOTE On occasion, the cache gets corrupted, causing wrong-type-argument
;; errors. If that happens, try `+gist/kill-cache'. You may have to restart
;; Emacs.

(after! gist
  (set-evil-initial-state! 'gist-list-mode 'normal)

  (set-popup-rule! "^\\*gist-" :ignore t)

  (defadvice! +gist--open-in-popup-a (orig-fn &rest args)
    :around #'gist-list-render
    (funcall orig-fn (car args) t)
    (unless (cadr args)
      (pop-to-buffer (current-buffer))))

  (map! :map gist-list-menu-mode-map
        :n "go"  #'gist-browse-current-url
        :n "gr"  #'gist-list-reload
        :n "c"   #'gist-add-buffer
        :n "d"   #'gist-kill-current
        :n "e"   #'gist-edit-current-description
        :n "f"   #'gist-fork
        :n "q"   #'kill-current-buffer
        :n "s"   #'gist-star
        :n "S"   #'gist-unstar
        :n "y"   #'gist-print-current-url))
