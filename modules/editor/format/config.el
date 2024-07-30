;;; editor/format/config.el -*- lexical-binding: t; -*-

(defcustom +format-on-save-disabled-modes
  '(sql-mode           ; sqlformat is currently broken
    tex-mode           ; latexindent is broken
    latex-mode
    org-msg-edit-mode) ; doesn't need a formatter
  "A list of major modes in which to not reformat the buffer upon saving.

If it is t, it is disabled in all modes, the same as if the +onsave flag wasn't
  used at all.
If nil, formatting is enabled in all modes."
  :type '(list symbol))

(defvaralias '+format-with 'apheleia-formatter)
(defvaralias '+format-inhibit 'apheleia-inhibit)


;;
;;; Bootstrap

(use-package! apheleia
  :defer t
  :init
  (when (modulep! +onsave)
    (add-hook 'doom-first-file-hook #'apheleia-global-mode)
    ;; apheleia autoloads `apheleia-inhibit-functions' so it will be immediately
    ;; available to mutate early.
    (add-hook! 'apheleia-inhibit-functions
      (defun +format-maybe-inhibit-h ()
        "Check if formatting should be disabled for current buffer.
This is controlled by `+format-on-save-disabled-modes'."
        (or (eq major-mode 'fundamental-mode)
            (string-blank-p (buffer-name))
            (eq +format-on-save-disabled-modes t)
            (not (null (memq major-mode +format-on-save-disabled-modes)))))))

  ;; Use the formatter provided by lsp-mode and eglot, if available.
  (when (modulep! +lsp)
    (add-hook 'eglot-managed-mode-hook #'+format-with-lsp-toggle-h)
    (add-hook 'lsp-managed-mode-hook #'+format-with-lsp-toggle-h))

  :config
  (add-to-list 'doom-debug-variables '(apheleia-log-only-errors . nil))
  (add-to-list 'doom-debug-variables '(apheleia-log-debug-info . t))

  (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt))

  ;; A psuedo-formatter that dispatches to the appropriate LSP client (via
  ;; `lsp-mode' or `eglot') that is capable of formatting. Without +lsp, users
  ;; must manually set `+format-with' to `lsp' to use it, or activate
  ;; `+format-with-lsp-mode' in the appropriate modes.
  (add-to-list 'apheleia-formatters '(lsp . +format-lsp-buffer))

  (defadvice! +format--inhibit-reformat-on-prefix-arg-a (orig-fn &optional arg)
    "Make it so \\[save-buffer] with prefix arg inhibits reformatting."
    :around #'save-buffer
    (let ((apheleia-mode (and apheleia-mode (memq arg '(nil 1)))))
      (funcall orig-fn)))

  ;; HACK: Apheleia suppresses notifications that the current buffer has
  ;;   changed, so plugins that listen for them need to be manually informed:
  (add-hook!
   'apheleia-post-format-hook
   (defun +format--update-web-mode-h ()
     (when (eq major-mode 'web-mode)
       (setq web-mode-fontification-off nil)
       (when (and web-mode-scan-beg web-mode-scan-end global-font-lock-mode)
         (save-excursion
           (font-lock-fontify-region web-mode-scan-beg web-mode-scan-end)))))
   (defun +format--update-vc-gutter-h ()
     (when (fboundp '+vc-gutter-update-h)
       (+vc-gutter-update-h)))))
