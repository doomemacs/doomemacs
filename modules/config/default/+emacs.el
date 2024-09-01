;;; config/default/+emacs.el -*- lexical-binding: t; -*-

(require 'projectile) ; we need its keybinds immediately


;;
;;; Reasonable defaults

(setq shift-select-mode t)
(delete-selection-mode +1)

(use-package! expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word)
  :init
  (setq expand-region-fast-keys-enabled nil)
  :config
  ;; Configure the transient map called when expanding a region
  (setq +er-transient-map
        (let ((t-map (make-sparse-keymap)))
          (define-key t-map (kbd "C-=") #'er/expand-region)
          (define-key t-map (kbd "=") #'er/expand-region)
          (define-key t-map (kbd "C--") #'er/contract-region)
          (define-key t-map (kbd "-") #'er/contract-region)
          (define-key t-map (kbd "C-0") (cmd! (er/expand-region 0)))
          (define-key t-map (kbd "0") (cmd! (er/expand-region 0)))
          t-map))
  ;; Custom expansion quitting
  (defadvice! doom--quit-expand-region-a (&rest _)
    "Properly abort an expand-region region."
    :before '(evil-escape doom/escape)
    (when (memq last-command '(er/expand-region er/contract-region))
      (er/contract-region 0))))


;;
;;; Keybinds

(when (modulep! +bindings)
  (load! "+emacs-bindings"))
