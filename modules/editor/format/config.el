;;; editor/format/config.el -*- lexical-binding: t; -*-

(defcustom +format-on-save-disabled-modes
  '(sql-mode           ; sqlformat is currently broken
    tex-mode           ; latexindent is broken
    latex-mode
    LaTeX-mode
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
       (+vc-gutter-update-h))))


  ;;
  ;; Custom formatters

  ;; Apheleia already has a definition for shfmt, but doesn't assign it to any
  ;; major modes, so...
  (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt))

  ;; A psuedo-formatter that dispatches to the appropriate LSP client (via
  ;; `lsp-mode' or `eglot') that is capable of formatting. Without +lsp, users
  ;; must manually set `+format-with' to `lsp' to use it, or activate
  ;; `+format-with-lsp-mode' in the appropriate modes.
  (add-to-list 'apheleia-formatters '(lsp . +format-lsp-buffer))

  ;; Use clang-format for cuda and protobuf files.
  (add-to-list 'apheleia-mode-alist '(cuda-mode . clang-format))
  (add-to-list 'apheleia-mode-alist '(protobuf-mode . clang-format))
  (add-to-list 'apheleia-formatters-mode-extension-assoc '(cuda-mode . ".cu"))
  (add-to-list 'apheleia-formatters-mode-extension-assoc '(protobuf-mode . ".proto"))

  ;; Apheleia's default clang-format config doesn't respect `c-basic-offset', so
  ;; force it to in the absence of a .clang-format file.
  (setf (alist-get 'clang-format apheleia-formatters)
        `("clang-format"
          "-assume-filename"
          (or (apheleia-formatters-local-buffer-file-name)
              (apheleia-formatters-mode-extension)
              ".c")
          (when apheleia-formatters-respect-indent-level
            (unless (locate-dominating-file default-directory ".clang-format")
              (format "--style={IndentWidth: %d}" c-basic-offset)))))

  ;; Apheleia's default config for prettier passes an explicit --tab-width N to
  ;; all prettier formatters, respecting your indent settings in Emacs, but
  ;; overriding any indent settings in your prettier config files. This changes
  ;; it to omit indent switches if any configuration for prettier is present in
  ;; the current project.
  (dolist (formatter '(prettier prettier-css prettier-html prettier-javascript
                       prettier-json prettier-scss prettier-svelte
                       prettier-typescript prettier-yaml))
    (setf (alist-get formatter apheleia-formatters)
          (append (delete '(apheleia-formatters-js-indent "--use-tabs" "--tab-width")
                          (alist-get formatter apheleia-formatters))
                  '((when apheleia-formatters-respect-indent-level
                      (unless (or (cl-loop for file
                                           in '(".prettierrc"
                                                ".prettierrc.json"
                                                ".prettierrc.yml"
                                                ".prettierrc.yaml"
                                                ".prettierrc.json5"
                                                ".prettierrc.js" "prettier.config.js"
                                                ".prettierrc.mjs" "prettier.config.mjs"
                                                ".prettierrc.cjs" "prettier.config.cjs"
                                                ".prettierrc.toml")
                                           if (locate-dominating-file default-directory file)
                                           return t)
                                  (when-let ((pkg (locate-dominating-file default-directory "package.json")))
                                    (require 'json)
                                    (let ((json-key-type 'alist))
                                      (assq 'prettier
                                            (json-read-file (expand-file-name "package.json" pkg))))))
                        (apheleia-formatters-indent "--use-tabs" "--tab-width"))))))))
