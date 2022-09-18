;;; ui/ophints/config.el -*- lexical-binding: t; -*-

(use-package! evil-goggles
  :when (modulep! :editor evil)
  :hook (doom-first-input . evil-goggles-mode)
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
  (when (modulep! :editor lispy)
    (pushnew! evil-goggles--commands
              '(lispyville-delete
                :face evil-goggles-delete-face
                :switch evil-goggles-enable-delete
                :advice evil-goggles--generic-blocking-advice)
              '(lispyville-delete-line
                :face evil-goggles-delete-face
                :switch evil-goggles-enable-delete
                :advice evil-goggles--delete-line-advice)
              '(lispyville-yank
                :face evil-goggles-yank-face
                :switch evil-goggles-enable-yank
                :advice evil-goggles--generic-async-advice)
              '(lispyville-yank-line
                :face evil-goggles-yank-face
                :switch evil-goggles-enable-yank
                :advice evil-goggles--generic-async-advice)
              '(lispyville-change
                :face evil-goggles-change-face
                :switch evil-goggles-enable-change
                :advice evil-goggles--generic-blocking-advice)
              '(lispyville-change-line
                :face evil-goggles-change-face
                :switch evil-goggles-enable-change
                :advice evil-goggles--generic-blocking-advice)
              '(lispyville-change-whole-line
                :face evil-goggles-change-face
                :switch evil-goggles-enable-change
                :advice evil-goggles--generic-blocking-advice)
              '(lispyville-indent
                :face evil-goggles-indent-face
                :switch evil-goggles-enable-indent
                :advice evil-goggles--generic-async-advice)
              '(lispyville-join
                :face evil-goggles-join-face
                :switch evil-goggles-enable-join
                :advice evil-goggles--join-advice))))


(use-package! volatile-highlights
  :unless (modulep! :editor evil)
  :hook (doom-first-input . volatile-highlights-mode)
  :config
  (after! undo-fu
    (vhl/define-extension 'undo-fu 'undo-fu-only-undo 'undo-fu-only-redo)
    (vhl/install-extension 'undo-fu)))
