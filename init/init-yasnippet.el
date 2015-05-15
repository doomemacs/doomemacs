(use-package yasnippet
  :mode (("emacs\\.d/snippets/.+$" . snippet-mode))
  :demand t
  :init
  (progn
    (defvar yas-minor-mode-map
      ;; Fix yasnippet keymaps so they only work in insert mode
      (let ((map (make-sparse-keymap)))
        (bind 'insert map [(tab)] 'yas-expand)
        (bind 'insert map (kbd "TAB") 'yas-expand)
        (bind 'visual map (kbd "<backtab>") 'yas-insert-snippet)
        map))

    ;; (add-hook 'snippet-mode-hook 'yas-minor-mode-on)
    ;; (add-hook 'text-mode-hook 'yas-minor-mode-on)
    ;; (add-hook 'prog-mode-hook 'yas-minor-mode-on)
    ;; (add-hook 'org-mode-hook 'yas-minor-mode-on))
    (add-hook 'snippet-mode-hook 'disable-final-newline))
  :config
  (progn
    (setq yas-verbosity 0)
    (setq yas-indent-line 'auto)
    (setq yas-also-auto-indent-first-line nil)
    (setq yas-wrap-around-region nil)
    ;; Only load personal snippets
    (setq yas-snippet-dirs `(,my-snippets-dir))
    (setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt))

    (yas-global-mode 1)
    (yas-reload-all)

    (after "helm"
      (add-to-list 'yas-dont-activate 'helm-alive-p))

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

    (evil-define-operator my:snippets (beg end &optional name)
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
          (yas-visit-snippet-file))))

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

    ;; Trim selection; do no further processing
    (defun %1 () (s-trim %))

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
    (defun my/yas-backspace (&optional field)
      (interactive)
      (let ((field (or field (and yas--active-field-overlay
                                  (overlay-buffer yas--active-field-overlay)
                                  (overlay-get yas--active-field-overlay 'yas--field)))))
        (cond ((eq (point) (marker-position (yas--field-start field))) nil)
              (t (delete-char -1)))))

    (defun my/yas-delete (&optional field)
      (interactive)
      (let ((field (or field (and yas--active-field-overlay
                                  (overlay-buffer yas--active-field-overlay)
                                  (overlay-get yas--active-field-overlay 'yas--field)))))
        (cond ((and field
                    (not (yas--field-modified-p field))
                    (eq (point) (marker-position (yas--field-start field))))
               (yas--skip-and-clear field)
               (yas-next-field 1))
              ((eq (point) (marker-position (yas--field-end field))) nil)
              (t (delete-char 1)))))

    (defun my/yas-clear-to-sof (&optional field)
      (interactive)
      (let* ((field (or field (and yas--active-field-overlay
                                   (overlay-buffer yas--active-field-overlay)
                                   (overlay-get yas--active-field-overlay 'yas--field))))
             (sof (marker-position (yas--field-start field))))
        (when (and field (> (point) sof))
          (delete-region sof (point)))))

    (defun my--init-yas-mode (&rest modes)
      ;; Yasnippet 0.8.1+
      (after "yasnippet"
             (when (boundp 'yas-extra-modes)
               (dolist (mode modes)
                 (if (symbol-value mode)
                   (yas-activate-extra-mode mode)
                   (setq yas-extra-modes (delq mode yas-extra-modes)))))))

    ;; keybinds
    (bind yas-keymap
          "C-e"           'my/yas-goto-end-of-field
          "C-a"           'my/yas-goto-start-of-field
          "<M-right>"     'my/yas-goto-end-of-field
          "<M-left>"      'my/yas-goto-start-of-field
          "<S-tab>"       'yas-prev-field
          "<M-backspace>" 'my/yas-clear-to-sof

          [backspace]  'my/yas-backspace
          "<delete>"   'my/yas-delete)))


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
