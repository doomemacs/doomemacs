;;; feature/file-templates/config.el -*- lexical-binding: t; -*-

(require! :feature snippets)

(defvar +file-templates-dir
  (expand-file-name "templates/" (file-name-directory load-file-name))
  "")

(def-package! autoinsert ; built-in
  :defer 1
  :init
  (setq auto-insert-query nil  ; Don't prompt before insertion
        auto-insert-alist nil) ; Tabula rasa

  (after! yasnippet
    (push '+file-templates-dir yas-snippet-dirs))

  :config
  (auto-insert-mode 1)

  (defun +file-templates--expand (key &optional mode project-only)
    "Auto insert a snippet of yasnippet into new file."
    (when (if project-only (doom-project-p) t)
      (require 'yasnippet)
      (unless yas-minor-mode (yas-minor-mode-on))
      (when (and yas-minor-mode
                 (yas-expand-snippet (yas-lookup-snippet key mode t))
                 (and (featurep 'evil) evil-mode)
                 (and yas--active-field-overlay
                      (overlay-buffer yas--active-field-overlay)
                      (overlay-get yas--active-field-overlay 'yas--field)))
        (evil-initialize-state 'insert))))

  (defun +file-templates-add (args)
    (cl-destructuring-bind (regexp trigger mode &optional project-only-p) args
      (define-auto-insert
        regexp
        (vector `(lambda () (+file-templates--expand ,trigger ',mode ,project-only-p))))))

  (mapc #'+file-templates-add
        ;; General
        '(("/\\.gitignore$"                  "__"               gitignore-mode)
          ("/Dockerfile$"                    "__"               dockerfile-mode)
          ("/docker-compose.yml$"            "__"               yaml-mode)
          ;; C/C++
          ("\\.h$"                           "__h"              c-mode)
          ("\\.c$"                           "__c"              c-mode)
          ("\\.h\\(h\\|pp|xx\\)$"            "__hpp"            c++-mode)
          ("\\.\\(cc\\|cpp\\)$"              "__cpp"            c++-mode)
          ("/main\\.\\(cc\\|cpp\\)$"         "__main.cpp"       c++-mode)
          ("/win32_\\.\\(cc\\|cpp\\)$"       "__winmain.cpp"    c++-mode)
          ("/Makefile$"                      "__"               makefile-gmake-mode)
          ;; Elisp
          ("\\.el$"                          "__initfile"       emacs-lisp-mode)
          ("-test\\.el$"                     "__"               emacs-ert-mode)
          ("/.emacs.d/.+\\.el$"              "__doom-module"    emacs-lisp-mode)
          ("/.emacs.d/.+/packages\\.el$"     "__doom-packages"  emacs-lisp-mode)
          (snippet-mode "__" snippet-mode)
          ;; Go
          ("\\.go$"                          "__.go"            go-mode)
          ("/main\\.go$"                     "__main.go"        go-mode t)
          ;; HTML
          ("\\.html$"                        "__.html"          web-mode)
          ;; java
          ("/src/.+/.+\\.java$"              "__"               java-mode)
          ("/main\\.java$"                   "__main"           java-mode)
          ("/build\\.gradle$"                "__build.gradle"   android-mode)
          ;; Javascript
          ("\\.\\(json\\|jshintrc\\)$"       "__"                  json-mode)
          ("/package\\.json$"                "__package.json"      json-mode)
          ("/bower\\.json$"                  "__bower.json"        json-mode)
          ("/gulpfile\\.js$"                 "__gulpfile.js"       js-mode)
          ("/webpack\\.config\\.js$"         "__webpack.config.js" js-mode)
          ("\\.lbaction/.+/Info.plist$"                       "__Info.plst"  lb6-mode)
          ("\\.lbaction/.+/\\(default\\|suggestions\\)\\.js$" "__default.js" lb6-mode)
          ;; Lua
          ("/main\\.lua$"                    "__main.lua"       love-mode)
          ("/conf\\.lua$"                    "__conf.lua"       love-mode)
          ;; Markdown
          ("\\.md$"                          "__"               markdown-mode)
          ;; PHP
          ("\\.php$"                         "__"               php-mode)
          ("\\.class\\.php$"                 "__.class.php"     php-mode)
          ;; Python
          ;;("tests?/test_.+\\.py$"         "__"                 nose-mode)
          ;;("/setup\\.py$"                 "__setup.py"         python-mode)
          ("\\.py$"                          "__"               python-mode)
          ;; Ruby
          ("\\.rb$"                          "__"               ruby-mode)
          ("/Rakefile$"                      "__Rakefile"       ruby-mode t)
          ("/Gemfile$"                       "__Gemfile"        ruby-mode t)
          ("/\\.rspec$"                      "__.rspec"         rspec-mode)
          ("\\.gemspec$"                     "__.gemspec"       ruby-mode t)
          ("/spec_helper\\.rb$"              "__helper"         rspec-mode t)
          ("/lib/.+\\.rb$"                   "__module"         ruby-mode t)
          ("_spec\\.rb$"                     "__"               rspec-mode t)
          ;; Rust
          ("/main\\.rs$"                     "__main.rs"        rust-mode)
          ("/Cargo.toml$"                    "__Cargo.toml"     rust-mode)
          ;; SCSS
          ("\\.scss$"                        "__"               scss-mode)
          ("/master\\.scss$"                 "__master.scss"    scss-mode)
          ("/normalize\\.scss$"              "__normalize.scss" scss-mode)
          ;; Slim
          ("/\\(index\\|main\\)\\.slim$"     "__"               slim-mode)
          ;; Shell scripts
          ("\\.z?sh$"                        "__"               sh-mode))))

