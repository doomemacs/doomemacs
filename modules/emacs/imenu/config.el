;;; emacs/imenu/config.el -*- lexical-binding: t; -*-

;; `imenu-anywhere'
(setq imenu-anywhere-delimiter ": ")


(after! imenu-list
  (setq imenu-list-idle-update-delay 0.5)

  (set! :popup "^\\*Ilist"
    '((side . right) (size . 35))
    '((quit . current) (select) (transient . 0)))

  (defun +imenu|cleanup-on-popup-close ()
    "Clean up after `imenu-list-minor-mode' when killing the list window."
    (unless +popup-buffer-mode
      (when imenu-list--displayed-buffer
        (with-current-buffer imenu-list--displayed-buffer
          (imenu-list-minor-mode -1)))
      (when (equal (buffer-name) imenu-list-buffer-name)
        (kill-buffer (get-buffer imenu-list-buffer-name)))))
  (add-hook '+popup-buffer-mode-hook #'+imenu|cleanup-on-popup-close))
