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

(lsp-define-stdio-client
 lsp-html
 "html"
 (lambda! () default-directory)
 '("html-languageserver" "--stdio"))

(add-hook! '(css-mode-hook sass-mode-hook) #'+web|init-lsp-css)
(add-hook! 'html-mode-hook #'+web|init-lsp-html)

(after! (:or css-mode sass-mode)
   (set-company-backend! '(css-mode sass-mode scss-mode less-css-mode) 'company-lsp))
(after! 'html-mode
  (set-company-backend! 'html-mode 'company-lsp))
