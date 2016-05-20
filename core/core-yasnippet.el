;;; core-yasnippet.el

(use-package yasnippet
  :mode ("emacs\\.d/private/\\(snippets\\|templates\\)/.+$" . snippet-mode)
  :commands (yas-minor-mode
             yas-minor-mode-on
             yas-expand
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file)
  :init
  (defvar yas-minor-mode-map (make-sparse-keymap))
  (setq yas-verbosity 0
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t
        yas-wrap-around-region nil
        ;; Only load personal snippets
        yas-snippet-dirs narf-snippet-dirs
        yas-prompt-functions '(yas-ido-prompt yas-no-prompt))

  (add-hook! (text-mode prog-mode snippet-mode markdown-mode org-mode)
    'yas-minor-mode-on)

  :config
  (yas-reload-all)
  (map! :map yas-keymap
        "C-e"           'narf/yas-goto-end-of-field
        "C-a"           'narf/yas-goto-start-of-field
        "<M-right>"     'narf/yas-goto-end-of-field
        "<M-left>"      'narf/yas-goto-start-of-field
        "<S-tab>"       'yas-prev-field
        "<M-backspace>" 'narf/yas-clear-to-sof
        "<escape>"      'evil-normal-state
        [backspace]     'narf/yas-backspace
        "<delete>"      'narf/yas-delete)

  ;; Exit snippets on ESC in normal mode
  (advice-add 'evil-force-normal-state :before 'yas-exit-all-snippets)
  ;; Fix an issue with smartparens interfering with yasnippet keybindings
  (advice-add 'yas-expand :before 'sp-remove-active-pair-overlay)
  ;; Once you're in normal mode, you're out
  (add-hook 'evil-normal-state-entry-hook 'yas-abort-snippet)
  ;; Strip out whitespace before a line selection
  (add-hook 'yas-before-expand-snippet-hook 'narf|yas-before-expand)
  ;; Fix previous hook persisting yas-selected-text between expansions
  (add-hook 'yas-after-exit-snippet-hook 'narf|yas-after-expand))

(use-package auto-yasnippet
  :commands (aya-create aya-expand aya-open-line aya-persist-snippet)
  :config
  (setq aya-persist-snippets-dir (concat narf-private-dir "auto-snippets/")))

(provide 'core-yasnippet)
;;; core-yasnippet.el ends here
