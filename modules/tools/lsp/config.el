;;; tools/lsp/config.el -*- lexical-binding: t; -*-

(defvar +lsp-defer-shutdown 3
  "If non-nil, defer shutdown of LSP servers for this many seconds after last
workspace buffer is closed.

This delay prevents premature server shutdown when a user still intends on
working on that project after closing the last buffer.")



;;
;;; Common

(defun +lsp-init-optimizations-h ()
  (when (or (bound-and-true-p eglot--managed-mode)
            (bound-and-true-p lsp-mode))
    ;; `read-process-output-max' is only available on recent development
    ;; builds of Emacs 27 and above.
    (setq-local read-process-output-max (* 1024 1024))
    ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
    ;;        native JSON library, so we up the GC threshold to stave off
    ;;        GC-induced slowdowns/freezes. Doom uses `gcmh' to enforce its GC
    ;;        strategy, so we modify its variables rather than
    ;;        `gc-cons-threshold' directly.
    (setq-local gcmh-high-cons-threshold (* 2 (default-value 'gcmh-high-cons-threshold)))))


;;
;;; Implementations

(if (featurep! +eglot)
    (load! "+eglot")
  (load! "+lsp"))
