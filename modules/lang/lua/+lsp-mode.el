;;; lang/lua/+lsp-mode.el -*- lexical-binding: t; -*-

(defun lsp-lua-langserver--lsp-command ()
  "Generate LSP startup command."
  (list (doom-path lua-lsp-dir
                   (cond (IS-MAC     "bin/macOS")
                         (IS-LINUX   "bin/Linux")
                         (IS-WINDOWS "bin/Windows"))
                   "lua-language-server")
        "-E" "-e" "LANG=en"
        (doom-path lua-lsp-dir "main.lua")))

(after! lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-lua-langserver--lsp-command)
                    :major-modes '(lua-mode)
                    :priority -1
                    :server-id 'lua-langserver)))
