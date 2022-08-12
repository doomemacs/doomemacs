;;; tools/lsp/doctor.el -*- lexical-binding: t; -*-

(assert! (not (and (modulep! +eglot)
                   (modulep! +peek)))
         "+eglot and +peek flags are not compatible. Peek uses lsp-mode, while Eglot is another package altogether for LSP.")

(unless (executable-find "npm")
  (warn! "Couldn't find npm, most server installers won't work and will have to be installed manually.
For more information, see https://emacs-lsp.github.io/lsp-mode/page/languages/."))
