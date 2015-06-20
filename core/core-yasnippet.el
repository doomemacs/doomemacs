;;; core-yasnippet.el --- For the lazy typist
;; see lib/yasnippet-defuns.el
;; see lib/yasnippet-macros.el

(use-package yasnippet
  :mode (("emacs\\.d/snippets/.+$" . snippet-mode))
  :diminish (yas-minor-mode . "Y")
  :commands (yas-minor-mode
             yas-minor-mode-on
             yas-expand
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file)
  :init
  (add-hook! (prog-mode snippet-mode markdown-mode org-mode) 'yas-minor-mode-on)
  (add-hook! snippet-mode 'narf|disable-final-newline)

  (setq yas-verbosity 0
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t
        yas-wrap-around-region nil
        ;; Only load personal snippets
        yas-snippet-dirs `(,@narf-snippet-dirs)
        yas-prompt-functions '(yas-ido-prompt yas-no-prompt))

  (bind! :i [(tab)] 'yas-expand
         :v "<backtab>" 'narf/yas-insert-snippet)

  (defvar yas-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (evil-define-key 'insert map [(tab)] 'yas-expand)
      (evil-define-key 'visual map (kbd "<backtab>") 'narf/yas-insert-snippet)
      map))
  :config
  (after! helm (add-to-list 'yas-dont-activate 'helm-alive-p))

  (yas-reload-all)

  ;; Simpler `yas-selected-text' alias for templates
  (defvaralias '% 'yas-selected-text)
  ;; Undo global maps
  (bind! :i [(tab)]     nil
         :v "<backtab>" nil)

  ;; keybinds
  (bind! :map yas-keymap
         "C-e"           'narf/yas-goto-end-of-field
         "C-a"           'narf/yas-goto-start-of-field
         "<M-right>"     'narf/yas-goto-end-of-field
         "<M-left>"      'narf/yas-goto-start-of-field
         "<S-tab>"       'yas-prev-field
         "<M-backspace>" 'narf/yas-clear-to-sof

         [backspace]     'narf/yas-backspace
         "<delete>"      'narf/yas-delete)

  ;; Once you're in normal mode, you're out
  (add-hook! evil-normal-state-entry 'yas-abort-snippet)
  ;; Strip out the shitespace before a line selection
  (add-hook! yas-before-expand-snippet 'narf|yas-before-expand)
  ;; Previous hook causes yas-selected-text to persist between expansions.
  ;; This little hack gets around it.
  (add-hook! yas-after-exit-snippet 'narf|yas-after-expand)

  ;; Exit snippets on ESC in normal mode
  (advice-add 'evil-force-normal-state :before 'yas-exit-all-snippets)
  ;; Prevents evil's visual-line from mode gobbling up the newline on the
  ;; right due to an off-by-one issue.
  (defadvice yas-expand-snippet (around yas-expand-snippet-visual-line activate)
    (when (narf/evil-visual-line-state-p)
      (ad-set-arg 2 (1- (ad-get-arg 2)))) ad-do-it))

(provide 'core-yasnippet)
;;; core-yasnippet.el ends here
