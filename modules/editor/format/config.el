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

(defcustom +format-with-lsp t
  "If non-nil, format with LSP formatter if it's available.

LSP formatter is provided by either `lsp-mode' or `eglot'.

This can be set buffer-locally with `setq-hook!' to disable LSP formatting in
select buffers, from a project's .dir-locals.el file, or as a file-local
variable."
  :type 'boolean
  :safe 'booleanp)

(defvaralias '+format-with 'apheleia-formatter)


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

  :config
  (add-to-list 'doom-debug-variables '(apheleia-log-only-errors . nil))

  ;; Use the formatter provided by lsp-mode and eglot, if they are available and
  ;; `+format-with-lsp' is non-nil.
  (cond ((modulep! :tools lsp +eglot)
         (add-to-list 'apheleia-formatters '(eglot . +format-eglot-buffer))
         (add-hook 'eglot-managed-hook #'+format-toggle-eglot-formatter-h))
        ((modulep! :tools lsp)
         (add-to-list 'apheleia-formatters '(lsp . +format-lsp-buffer))
         (add-hook 'lsp-configure-hook #'+format-enable-lsp-formatter-h)
         (add-hook 'lsp-unconfigure-hook #'+format-disable-lsp-formatter-h)))

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
