;;; module-db.el

(use-package sql-mode
  :mode "\\.sql$"
  :config
  (def-popup! "\\*SQL.*\\*" :align below :size 0.4 :noselect t :regexp t)
  (evil-set-initial-state 'sql-interactive-mode 'emacs)
  (push 'sql-interactive-mode doom-popup-protect-modes)
  ;; For my local development environment
  (setq sql-pop-to-buffer-after-send-region nil
        sql-server "docker.dev"
        sql-user "root"
        sql-password ""))

;; extract these
(evil-define-command doom:db-select (product)
  (interactive "<a>")
  (sql-set-product (intern product))
  (message "sql-product set to %s" product))

(evil-define-operator doom:db (beg end &optional bang product)
  "Open a db connection, reopen an old one, or send the selected region to the
open comint."
  :type inclusive
  (interactive "<r><!><a>")
  (let ((sql-buf (get-buffer sql-buffer)))
    (when (and bang sql-buf)
      (message "Restarting connection")
      (kill-buffer sql-buf))
    (if sql-buf
        (if (or (get-buffer-window sql-buf)
                (evil-visual-state-p))
            (sql-send-region beg end)
          (doom/popup-buffer sql-buf))
      (let ((product (if product (intern product) sql-product)))
        (unless product
          (user-error "No SQL product is set"))
        (sql-set-product product)
        (sql-product-interactive product 0)
        (message "Started new %s connection" product)))))

(provide 'module-db)
;;; module-db.el ends here
