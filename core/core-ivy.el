;;; core-ivy.el
;; see defuns/defuns-ivy.el

(use-package ivy
  :init
  (setq projectile-completion-system 'ivy
        ivy-height 15
        ivy-do-completion-in-region nil
        ivy-wrap t)

  :config
  (after! magit
    (setq magit-completing-read-function 'ivy-completing-read))

  (ivy-mode +1)
  (map! :map ivy-minibuffer-map
        [escape] 'keyboard-escape-quit
        "C-r" 'evil-paste-from-register
        "M-v" 'clipboard-yank
        "C-w" 'backward-kill-word
        "C-u" 'backward-kill-sentence
        "C-b" 'backward-word
        "C-f" 'forward-word)
  ;; Fix display glitches
  (advice-add 'ivy-done :after 'redraw-display)

  ;;
  (require 'counsel)

  (add-hook! doom-popup-mode
    (when (eq major-mode 'ivy-occur-grep-mode)
      (ivy-wgrep-change-to-wgrep-mode)))

  (advice-add 'counsel-ag-function :override 'doom*counsel-ag-function)
  (define-key counsel-ag-map [backtab] 'ivy-occur)

  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

(use-package counsel-projectile :after projectile)

(provide 'core-ivy)
;;; core-ivy.el ends here
