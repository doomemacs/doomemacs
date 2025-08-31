;;; lang/zig/config.el -*- lexical-binding: t; -*-

;; DEPRECATED: Remove when projectile is replaced with project.el
(after! projectile
  (add-to-list 'projectile-project-root-files "build.zig"))


;;
;;; Packages

(defun +zig-common-config (mode)
  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append))
  (map! :localleader
        :map ,(intern (format "%s-map" mode))
        "b" #'zig-compile
        "f" #'zig-format-buffer
        "r" #'zig-run
        "t" #'zig-test-buffer))


(when (modulep! :checkers syntax -flymake)
  (after! flycheck
    (eval '(flycheck-define-checker zig
             "A zig syntax checker using zig's `ast-check` command."
             :command ("zig" "ast-check" (eval (buffer-file-name)))
             :error-patterns
             ((error line-start (file-name) ":" line ":" column ": error: " (message) line-end))
             :modes (zig-mode zig-ts-mode))
          t)
    (add-to-list 'flycheck-checkers 'zig)))


(use-package! zig-mode
  :hook (zig-mode . rainbow-delimiters-mode)
  :config
  (setq zig-format-on-save nil) ; rely on :editor format instead
  (+zig-common-config 'zig-mode))


(use-package! zig-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'zig-ts-mode)
  :defer t
  :init
  (set-tree-sitter! 'zig-mode 'zig-ts-mode
    '((zig :url "https://github.com/tree-sitter/zig-tree-sitter"
           :rev "v0.25.0")))
  :config
  (+zig-common-config 'zig-ts-mode))
