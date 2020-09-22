;;; config/default/autoload/deferred.el -*- lexical-binding: t; -*-

;; TODO generalize this
;;;###autoload
(defun +default/lsp-command-map ()
  "Lazily invoke `lsp-command-map'."
  (interactive)
  (require 'lsp-mode)
  (map! :leader "c l" lsp-command-map)
  (dolist (leader-key (list doom-leader-key doom-leader-alt-key))
    (let ((lsp-keymap-prefix (concat leader-key " c l")))
      (lsp-enable-which-key-integration)))
  (setq prefix-arg current-prefix-arg
        unread-command-events
        (mapcar (lambda (e) (cons t e))
                (vconcat (when (bound-and-true-p evil-this-operator)
                           (where-is-internal evil-this-operator
                                              evil-normal-state-map
                                              t))
                         (this-command-keys)))))
