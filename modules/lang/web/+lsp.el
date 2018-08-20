;;; lang/web/+lsp.el -*- lexical-binding: t; -*-

(lsp-define-stdio-client
 lsp-css
 "css"
 #'projectile-project-root
 '("css-languageserver" "--stdio"))

(lsp-define-stdio-client
 lsp-scss
 "scss"
 #'projectile-project-root
 '("css-languageserver" "--stdio"))

(lsp-define-stdio-client
 lsp-less
 "less"
 #'projectile-project-root
 '("css-languageserver" "--stdio"))
