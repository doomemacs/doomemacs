;;; tools/imenu/config.el -*- lexical-binding: t; -*-

(def-package! imenu-anywhere
  :commands (ido-imenu-anywhere ivy-imenu-anywhere helm-imenu-anywhere)
  :config (setq imenu-anywhere-delimiter ": "))


(def-package! imenu-list
  :commands imenu-list-minor-mode
  :config
  (setq imenu-list-focus-after-activation t)
  (set! :popup imenu-list-buffer-name :size 35 :align 'right)

  ;; use popups
  (defun doom*imenu-list-show ()
    (doom-popup-buffer (get-buffer imenu-list-buffer-name)))
  (advice-add #'imenu-list-show :override #'doom*imenu-list-show)
  (advice-add #'imenu-list-show-noselect :override #'doom*imenu-list-show)

  ;; auto kill imenu-list on deactivation
  (defun doom|kill-imenu-list ()
    (when (and (not imenu-list-minor-mode)
               (get-buffer imenu-list-buffer-name))
      (kill-buffer (get-buffer imenu-list-buffer-name))))
  (add-hook 'imenu-list-minor-mode-hook #'doom|kill-imenu-list))
