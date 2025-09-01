;;; tools/lsp/doctor.el -*- lexical-binding: t; -*-

(assert! (not (modulep! +eglot +peek))
         "+eglot and +peek flags are not compatible. Peek uses lsp-mode, while Eglot is another package altogether for LSP.")

(unless (modulep! +eglot)
  (unless (executable-find "npm")
    (warn! "Couldn't find npm. `lsp-mode' needs npm to auto-install some LSP servers. For more information, see https://emacs-lsp.github.io/lsp-mode/page/languages/.")))

(when (modulep! +booster)
  (if (modulep! +eglot)
      (unless (executable-find "emacs-lsp-booster")
        (warn! "Couldn't find emacs-lsp-booster executable."))
    (error! "+booster does nothing without +eglot")))
