;;; core-file-templates.el --- file templates

(use-package autoinsert
  :defer t
  :init
  (setq auto-insert-query nil    ; Don't prompt before insertion
        auto-insert-alist '())
  :config
  (auto-insert-mode 1)

  (defun auto-insert-template (rule)
    (define-auto-insert
      (nth 0 rule)
      (vector `(lambda () (narf/auto-insert-snippet ,(nth 1 rule) ',(nth 2 rule) ,(nth 3 rule))))))

  (mapc 'auto-insert-template
        `(;; General
          ("/\\.gitignore$"                  "__"               gitignore-mode)
          ("/Dockerfile$"                    "__"               dockerfile-mode)
          ("/docker-compose.yml$"            "__"               yaml-mode)

          ;; C/C++
          ("/Makefile$"                      "__"               makefile-gmake-mode)
          ("/main\\.\\(cc\\|cpp\\)$"         "__main.cpp"       c++-mode)
          ("/win32_\\.\\(cc\\|cpp\\)$"       "__winmain.cpp"    c++-mode)
          ("\\.\\([Hh]\\|hpp\\)$"            "__.h"             c++-mode)
          ("\\.\\(cc\\|cpp\\)$"              "__.cpp"           c++-mode)
          ("\\.c$"                           "__.c"             c-mode)

          ;; Elisp
          ("\\.emacs\\.d/.+\\.el$"           "__initfile"       emacs-lisp-mode)
          ("\\.emacs\\.d/private/\\(snippets\\|templates\\)/.+$" "__" snippet-mode)

          ;; Go
          ("/main\\.go$"                     "__main.go"        go-mode t)
          ("\\.go$"                          "__.go"            go-mode)

          ;; HTML
          ("\\.html$"                        "__.html"          web-mode)

          ;; java
          ("/src/.+/.+\\.java$"              "__"               java-mode)
          ("/main\\.java$"                   "__main"           java-mode)
          ("/build\\.gradle$"                "__build.gradle"   android-mode)

          ;; Javascript
          ("\\.lbaction/.+/Info.plist$"                       "__Info.plst"  lb6-mode)
          ("\\.lbaction/.+/\\(default\\|suggestions\\)\\.js$" "__default.js" lb6-mode)
          ("/package\\.json$"                "__package.json"   json-mode)
          ("\\.\\(json\\|jshintrc\\)$"       "__"               json-mode)

          ;; Lua
          ("/main\\.lua$"                    "__main.lua"       love-mode)
          ("/conf\\.lua$"                    "__conf.lua"       love-mode)

          ;; Markdown
          ("\\.md$"                          "__"               markdown-mode)

          ;; PHP
          ("\\.class\\.php$"                 "__.class.php"     php-mode)
          ("\\.php$"                         "__"               php-mode)

          ;; Python
          ;;"tests?/test_.+\\.py$"         "__"               nose-mode)
          ;;"/setup\\.py$"                 "__setup.py"       python-mode)
          ("\\.py$"                          "__"               python-mode)

          ;; Ruby
          ("/\\.rspec$"                      "__.rspec"         rspec-mode)
          ("/spec_helper\\.rb$"              "__helper"         rspec-mode t)
          ("_spec\\.rb$"                     "__"               rspec-mode t)
          ("/Rakefile$"                      "__Rakefile"       ruby-mode t)
          ("/Gemfile$"                       "__Gemfile"        ruby-mode t)
          ("\\.gemspec$"                     "__.gemspec"       ruby-mode t)
          ("/lib/.+\\.rb$"                   "__module"         ruby-mode t)
          ("\\.rb$"                          "__"               ruby-mode)

          ;; Rust
          ("/Cargo.toml$"                    "__Cargo.toml"     rust-mode)
          ("/main\\.rs$"                     "__main.rs"        rust-mode)

          ;; SCSS
          ("/master\\.scss$"                 "__master.scss"    scss-mode)
          ("/normalize\\.scss$"              "__normalize.scss" scss-mode)
          ("\\.scss$"                        "__"               scss-mode)

          ;; Slim
          ("/\\(index\\|main\\)\\.slim$"     "__"               slim-mode)

          ;; Shell scripts
          ("\\.z?sh$"                        "__"               sh-mode)
          )))

(provide 'core-file-templates)
;;; core-file-templates.el ends here
