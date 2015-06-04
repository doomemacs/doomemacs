(use-package yasnippet
  :diminish (yas-minor-mode . "Y")
  :commands (yas-minor-mode yas-minor-mode-on narf/init-yas-mode)
  :mode (("emacs\\.d/snippets/.+$" . snippet-mode))
  :init
  (progn
    (add-to-hooks 'yas-minor-mode-on '(prog-mode-hook
                                      snippet-mode-hook
                                      markdown-mode-hook
                                      org-mode-hook))
    (add-hook 'snippet-mode-hook 'disable-final-newline)

    ;; Switch to insert mode when expanding a template via backtab, or go back
    ;; to normal mode if there are no fields.
    (defun narf/insert-yas-snippet ()
      (interactive)
      (yas-insert-snippet)
      (evil-insert-state +1))

    (defvar yas-minor-mode-map
      (let ((map (make-sparse-keymap)))
        (bind insert :map map [(tab)] 'yas-expand)
        (bind visual :map map "<backtab>" 'narf/insert-yas-snippet)
        map)))
  :config
  (progn
    ;; Undo global maps
    (bind insert [(tab)]     nil)
    (bind visual "<backtab>" nil)

    (after "helm" (add-to-list 'yas-dont-activate 'helm-alive-p))

    (setq yas-verbosity 0
          yas-indent-line 'auto
          yas-also-auto-indent-first-line t
          yas-wrap-around-region nil
          ;; Only load personal snippets
          yas-snippet-dirs `(,SNIPPETS-DIR)
          yas-prompt-functions '(yas-ido-prompt yas-no-prompt))
    (yas-reload-all)

    ;; Exit snippets on ESC in normal mode
    (advice-add 'evil-force-normal-state :before 'yas-exit-all-snippets)
    ;; Once you're in normal mode, you're out
    (add-hook 'evil-normal-state-entry-hook 'yas-abort-snippet)

    ;; Fixes: evil's visual-line mode gobbles up the newline on the right.
    ;; This prevents that by essentially doing (1- (region-end)).
    (defadvice yas-expand-snippet (around yas-expand-snippet-visual-line activate)
      (when (evil-visual-line-state-p)
        (ad-set-arg 2 (1- (ad-get-arg 2)))) ad-do-it)

    ;; Fixes: visual-line includes indentation before the selection. This
    ;; strips it out.
    (add-hook! 'yas-before-expand-snippet-hook
      (when (evil-visual-line-state-p)
        (setq-local yas-selected-text
                    (replace-regexp-in-string
                     "\\(^ *\\|\n? $\\)" ""
                     (buffer-substring-no-properties (region-beginning)
                                                     (1- (region-end)))))))
    ;; Previous hook causes yas-selected-text to persist between expansions.
    ;; This little hack gets around it.
    (add-hook! 'yas-after-exit-snippet-hook (setq-local yas-selected-text nil))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Snippet helpers
    (defvaralias '% 'yas-selected-text)
    ;; Shorthand defun to surround text with newlines if more than one line.
    (defun !%! ()
      (when %
        (if (> (length (s-lines %)) 1)
            (concat "\n" % "\n")
          (s-trim %))))
    ;; Shorthand defun for snippets: prepends a newline to `yas-selected-text'
    ;; IF it contains more than one line.
    (defun !% ()
      (when %
        (if (> (length (s-lines %)) 1)
            (concat "\n" %)
          (s-trim %))))
    ;; Trim selection; do no further processing
    (defun %1 () (s-trim %))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Inter-field navigation
    (defun narf/yas-goto-start-of-field ()
      (interactive)
      (let* ((snippet (car (yas--snippets-at-point)))
             (position (yas--field-start (yas--snippet-active-field snippet))))
        (if (= (point) position)
            (move-beginning-of-line 1)
          (goto-char position))))
    (defun narf/yas-goto-end-of-field ()
      (interactive)
      (let* ((snippet (car (yas--snippets-at-point)))
             (position (yas--field-end (yas--snippet-active-field snippet))))
        (if (= (point) position)
            (move-end-of-line 1)
          (goto-char position))))

    ;; Prevents Yas from stepping on my toes when I use backspace
    (defun narf/yas-backspace (&optional field)
      (interactive)
      (let ((field (or field (and yas--active-field-overlay
                                  (overlay-buffer yas--active-field-overlay)
                                  (overlay-get yas--active-field-overlay 'yas--field)))))
        (cond ((eq (point) (marker-position (yas--field-start field))) nil)
              (t (delete-char -1)))))

    (defun narf/yas-delete (&optional field)
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

    (defun narf/yas-clear-to-sof (&optional field)
      (interactive)
      (let* ((field (or field (and yas--active-field-overlay
                                   (overlay-buffer yas--active-field-overlay)
                                   (overlay-get yas--active-field-overlay 'yas--field))))
             (sof (marker-position (yas--field-start field))))
        (when (and field (> (point) sof))
          (delete-region sof (point)))))

    (defun narf/init-yas-mode (&rest modes)
      ;; Yasnippet 0.8.1+
      (after "yasnippet"
        (when (boundp 'yas-extra-modes)
          (dolist (mode modes)
            (if (symbol-value mode)
              (yas-activate-extra-mode mode)
              (setq yas-extra-modes (delq mode yas-extra-modes)))))))

    ;; keybinds
    (bind :map yas-keymap
          "C-e"           'narf/yas-goto-end-of-field
          "C-a"           'narf/yas-goto-start-of-field
          "<M-right>"     'narf/yas-goto-end-of-field
          "<M-left>"      'narf/yas-goto-start-of-field
          "<S-tab>"       'yas-prev-field
          "<M-backspace>" 'narf/yas-clear-to-sof

          [backspace]  'narf/yas-backspace
          "<delete>"   'narf/yas-delete)))


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
