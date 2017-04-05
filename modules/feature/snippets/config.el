;;; feature/snippets/config.el

;; Snippets! I've thrown together a few hacks to make `yasnippet' and `evil'
;; behave together.

(def-package! yasnippet
  :commands (yas-minor-mode yas-minor-mode-on yas-expand yas-expand-snippet
             yas-lookup-snippet yas-insert-snippet yas-new-snippet
             yas-visit-snippet-file snippet-mode)
  :preface
  (defvar yas-minor-mode-map (make-sparse-keymap))

  :init
  ;; Ensure `yas-reload-all' is called as late as possible. Other modules could
  ;; have additional configuration for yasnippet. For example, file-templates.
  (add-transient-hook! yas-minor-mode-hook (yas-reload-all))

  (add-hook! (text-mode prog-mode snippet-mode markdown-mode org-mode)
    'yas-minor-mode-on)

  :config
  (setq yas-verbosity 0
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t
        yas-prompt-functions '(yas-completing-prompt yas-ido-prompt yas-no-prompt)
        yas-snippet-dirs '(yas-installed-snippets-dir)
        yas-use-menu nil
        ;; Allow nested snippets
        yas-triggers-in-field t)

  ;; Allows project-specific snippets
  (defun +snippets|enable-project-modes (mode &rest _)
    "Enable snippets for project modes."
    (if (symbol-value mode)
        (yas-activate-extra-mode mode)
      (yas-deactivate-extra-mode mode)))
  (add-hook 'doom-project-hook '+snippets|enable-project-modes)

  ;; fix an error caused by smartparens interfering with yasnippet bindings
  (advice-add 'yas-expand :before 'sp-remove-active-pair-overlay)

  (after! evil
    (map! (:map yas-keymap
            "C-e"           '+snippets/goto-end-of-field
            "C-a"           '+snippets/goto-start-of-field
            "<M-right>"     '+snippets/goto-end-of-field
            "<M-left>"      '+snippets/goto-start-of-field
            "<M-backspace>" '+snippets/delete-to-start-of-field
            [escape]        'evil-normal-state
            [backspace]     '+snippets/delete-backward-char
            [delete]        '+snippets/delete-forward-char-or-field)

          (:map yas-minor-mode-map
            :i "<tab>" yas-maybe-expand
            :v "<tab>" '+snippets/expand-on-region))

    ;; Exit snippets on ESC in normal mode
    (advice-add 'evil-force-normal-state :before 'yas-exit-all-snippets)
    ;; Once you're in normal mode, you're out
    (add-hook 'evil-normal-state-entry-hook 'yas-abort-snippet)
    ;; Strip out whitespace before a line selection
    (defun +snippets|yas-before-expand ()
      "Strip out the shitespace before a line selection."
      (when (and (evil-visual-state-p)
                 (eq (evil-visual-type) 'line))
        (setq yas-selected-text
              (replace-regexp-in-string
               "\\(^\\s-*\\|\n? $\\)" ""
               (buffer-substring-no-properties evil-visual-beginning
                                               evil-visual-end)))))
    (add-hook 'yas-before-expand-snippet-hook '+snippets|yas-before-expand)

    (defun +snippets|yas-after-expand ()
      "Fix previous hook persisting yas-selected-text between expansions."
      (setq yas-selected-text nil))
    (add-hook 'yas-after-exit-snippet-hook '+snippets|yas-after-expand)))


(def-package! auto-yasnippet
  :commands (aya-create aya-expand aya-open-line aya-persist-snippet)
  :init
  (map! :i  [C-tab] 'aya-expand
        :nv [C-tab] 'aya-create)
  :config
  (setq aya-persist-snippets-dir (concat doom-local-dir "auto-snippets/")))

