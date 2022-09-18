;;; tools/debugger/doctor.el -*- lexical-binding: t; -*-

(when (and (modulep! +lsp) (modulep! :tools lsp +eglot))
  (warn! "+lsp flag is not compatible with :tools (lsp +eglot). Choose only one of (eglot or dap-mode) please"))
