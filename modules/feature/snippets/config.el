;;; feature/snippets/config.el

(@def-package yasnippet
  :commands (yas-minor-mode
             yas-minor-mode-on
             yas-expand
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file)
  :preface
  (defvar yas-minor-mode-map (make-sparse-keymap))
  :init
  (setq yas-verbosity 0
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t
        yas-prompt-functions '(yas-completing-prompt yas-ido-prompt yas-no-prompt)
        yas-snippet-dirs '(yas-installed-snippets-dir))

  (@add-hook (text-mode prog-mode snippet-mode markdown-mode org-mode)
    'yas-minor-mode-on)

  :config
  (yas-reload-all)
  (@map (:map yas-keymap
          "C-e"           '+snippets/goto-end-of-field
          "C-a"           '+snippets/goto-start-of-field
          "<M-right>"     '+snippets/goto-end-of-field
          "<M-left>"      '+snippets/goto-start-of-field
          "<S-tab>"       'yas-prev-field
          "<M-backspace>" '+snippets/delete-to-start-of-field
          "<escape>"      'evil-normal-state
          [backspace]     '+snippets/delete-backward-char
          "<delete>"      '+snippets/delete-forward-char-or-field)

        (:map yas-minor-mode-map
          :i [tab] 'yas-expand
          :v [tab] '+snippets/expand-on-region))

  ;; Fix an error caused by smartparens interfering with yasnippet bindings
  (advice-add 'yas-expand :before 'sp-remove-active-pair-overlay)

  ;; Exit snippets on ESC in normal mode
  (advice-add 'evil-force-normal-state :before 'yas-exit-all-snippets)

  ;; Once you're in normal mode, you're out
  (add-hook 'evil-normal-state-entry-hook 'yas-abort-snippet)

  ;; Strip out whitespace before a line selection
  (defun +snippets|yas-before-expand ()
    "Strip out the shitespace before a line selection."
    (when (and (evil-visual-state-p)
               (eq (evil-visual-type) 'line))
      (setq-local
       yas-selected-text
       (replace-regexp-in-string
        "\\(^ *\\|\n? $\\)" ""
        (buffer-substring-no-properties (region-beginning)
                                        (1- (region-end)))))))
  (add-hook 'yas-before-expand-snippet-hook '+snippets|yas-before-expand)

  (defun +snippets|yas-after-expand ()
    "Fix previous hook persisting yas-selected-text between expansions."
    (setq yas-selected-text nil))
  (add-hook 'yas-after-exit-snippet-hook '+snippets|yas-after-expand))


(@def-package auto-yasnippet
  :commands (aya-create aya-expand aya-open-line aya-persist-snippet)
  :config
  (setq aya-persist-snippets-dir (concat doom-local-dir "auto-snippets/")))

