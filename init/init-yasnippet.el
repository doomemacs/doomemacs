(provide 'init-yasnippet)

(use-package yasnippet
  :mode (("emacs\\.d/snippets/.+$" . snippet-mode))
  :pre-load
  (defvar yas-minor-mode-map
    ;; Fix yasnippet keymaps so they only work in insert mode
    (let ((map (make-sparse-keymap)))
      (bind 'insert map [(tab)] 'yas-expand)
      (bind 'insert map (kbd "TAB") 'yas-expand)
      (bind 'visual map (kbd "<backtab>") 'yas-insert-snippet)
      map))
  :init
  (progn
    (add-hook 'snippet-mode-hook 'disable-final-newline)
    (add-hook 'snippet-mode-hook 'yas-minor-mode)
    (add-hook 'text-mode-hook 'yas-minor-mode)
    (add-hook 'prog-mode-hook 'yas-minor-mode)
    ;; (add-hook 'markdown-mode-hook 'yas-minor-mode)
    (add-hook 'org-mode-hook 'yas-minor-mode))
  :config
  (progn
    (setq yas-verbosity 0)
    (setq yas-indent-line 'auto)
    (setq yas-also-auto-indent-first-line t)
    (setq yas-wrap-around-region nil)
    ;; Only load personal snippets
    (setq yas-snippet-dirs `(,my-snippets-dir))
    (setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt))

    (push '(snippet-mode :position bottom :stick t) popwin:special-display-config)

    (yas-reload-all)

    (after "auto-complete"
      (defadvice ac-expand (before advice-for-ac-expand activate)
        (when (yas-expand) (ac-stop))))

    (after "evil"
      ;; Exit snippets on ESC in normal mode
      (defadvice evil-force-normal-state (before evil-esc-quit-yasnippet activate)
        (yas-exit-all-snippets))
      ;; Once you're in normal mode, you're out
      (add-hook 'evil-normal-state-entry-hook 'yas-abort-snippet)

      ;; Fixes: evil's visual-line mode gobbles up the newline on the right.
      ;; This prevents that by essentially doing (1- (region-end)).
      (defadvice yas-expand-snippet (around yas-expand-snippet-visual-line activate)
        (if (evil-visual-line-state-p)
            (ad-set-arg 2 (1- (ad-get-arg 2)))) ad-do-it)

      ;; Fixes: visual-line includes indentation before the selection. This
      ;; strips it out.
      (add-hook! 'yas-before-expand-snippet-hook
                 (if (evil-visual-line-state-p)
                     (setq-local yas-selected-text
                                 (replace-regexp-in-string
                                  "\\(^ *\\|\n? $\\)" ""
                                  (buffer-substring-no-properties (region-beginning)
                                                                  (1- (region-end)))))))
      ;; Previous hook causes yas-selected-text to persist between expansions.
      ;; This little hack gets around it.
      (add-hook! 'yas-after-exit-snippet-hook
                 (setq-local yas-selected-text nil))

      (evil-ex-define-cmd "snip[pets]" 'ex:snippets)
      (evil-define-operator ex:snippets (beg end &optional name)
        :motion nil
        :move-point nil
        :type exclusive
        :repeat nil
        (interactive "<r><a>")
        (if (and beg end)
            (yas-insert-snippet)
          (if name
              (popwin:find-file (concat my-snippets-dir
                                        (symbol-name major-mode) "/" name))
            (yas-visit-snippet-file)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Snippet helpers
    (defvaralias '% 'yas-selected-text)
    ;; Shorthand defun to surround text with newlines if more than one line.
    (defun !%! ()
      (when %
        (if (> (s-count-lines %) 1)
            (concat "\n" % "\n")
          (s-trim %))))
    ;; Shorthand defun for snippets: prepends a newline to `yas-selected-text'
    ;; IF it contains more than one line.
    (defun !% ()
      (when %
        (if (> (s-count-lines %) 1)
            (concat "\n" %)
          (s-trim %))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Inter-field navigation
    (defun my/yas-goto-start-of-field ()
      (interactive)
      (let* ((snippet (car (yas--snippets-at-point)))
             (position (yas--field-start (yas--snippet-active-field snippet))))
        (if (= (point) position)
            (move-beginning-of-line 1)
          (goto-char position))))
    (defun my/yas-goto-end-of-field ()
      (interactive)
      (let* ((snippet (car (yas--snippets-at-point)))
             (position (yas--field-end (yas--snippet-active-field snippet))))
        (if (= (point) position)
            (move-end-of-line 1)
          (goto-char position))))

    ;; Prevents Yas from stepping on my toes when I use backspace
    (defun my/yas-clear-field (&optional field)
      (interactive)
      (let ((field (or field (and yas--active-field-overlay
                                  (overlay-buffer yas--active-field-overlay)
                                  (overlay-get yas--active-field-overlay 'yas--field)))))
        (cond ((and field
                    (not (yas--field-modified-p field))
                    (eq (point) (marker-position (yas--field-start field))))
               (yas--skip-and-clear field)
               (yas-next-field 1))
              (t (delete-char -1)))))

    ;; keybinds
    (bind yas-keymap
          "C-e"        'my/yas-goto-end-of-field
          "C-a"        'my/yas-goto-start-of-field
          "<s-right>"  'my/yas-goto-end-of-field
          "<s-left>"   'my/yas-goto-start-of-field
          [backspace]  'my/yas-clear-field)))
