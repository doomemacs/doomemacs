;;; lang/zig/config.el -*- lexical-binding: t; -*-

(after! projectile
  (pushnew! projectile-project-root-files "build.zig"))


;;
;; zig-mode

(use-package! zig-mode
  :hook (zig-mode . rainbow-delimiters-mode)
  :config
  ;; Disable zig-mode's default format on save behaviour.
  (setq zig-format-on-save nil)
  (when (featurep! +lsp)
    (add-hook 'zig-mode-local-vars-hook #'lsp!))
  (map! :localleader
        :map zig-mode-map
        "b" #'zig-compile
        "f" #'zig-format-buffer
        "r" #'zig-run
        "t" #'zig-test-buffer))

(when (featurep! :checkers syntax)
  (flycheck-define-checker zig
    "A zig syntax checker using the zig-fmt interpreter."
    :command ("zig" "fmt" (eval (buffer-file-name)))
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": error: " (message) line-end))
    :modes zig-mode)
  (add-to-list 'flycheck-checkers 'zig))
