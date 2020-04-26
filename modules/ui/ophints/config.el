;;; ui/ophints/config.el -*- lexical-binding: t; -*-

(use-package! evil-goggles
  :when (featurep! :editor evil)
  :after-call pre-command-hook
  :init
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil ; too slow
        ;; evil-goggles provides a good indicator of what has been affected.
        ;; delete/change is obvious, so I'd rather disable it for these.
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil)
  :config
  (pushnew! evil-goggles--commands
            '(evil-magit-yank-whole-line
              :face evil-goggles-yank-face
              :switch evil-goggles-enable-yank
              :advice evil-goggles--generic-async-advice)
            '(+evil:yank-unindented
              :face evil-goggles-yank-face
              :switch evil-goggles-enable-yank
              :advice evil-goggles--generic-async-advice)
            '(+eval:region
              :face evil-goggles-yank-face
              :switch evil-goggles-enable-yank
              :advice evil-goggles--generic-async-advice))
  (evil-goggles-mode +1))


(use-package! volatile-highlights
  :unless (featurep! :editor evil)
  :after-call pre-command-hook
  :config
  (after! undo-fu
    (vhl/define-extension 'undo-fu 'undo-fu-only-undo 'undo-fu-only-redo)
    (vhl/install-extension 'undo-fu)))
