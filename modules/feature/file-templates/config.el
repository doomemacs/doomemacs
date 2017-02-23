;;; feature/file-templates/config.el

(require! :feature snippets)

(defvar +file-templates-dir
  (expand-file-name "templates/" (file-name-directory load-file-name))
  "")

(def-package! autoinsert ; built-in
  :after yasnippet
  :init
  (setq auto-insert-query nil  ; Don't prompt before insertion
        auto-insert-alist nil) ; Tabula rasa

  :config
  (auto-insert-mode 1)

  (push '+file-templates-dir yas-snippet-dirs)

  (defun +file-templates--expand (key &optional mode project-only)
    "Auto insert a snippet of yasnippet into new file."
    (interactive)
    (when (if project-only (doom-project-p) t)
      (require 'yasnippet)
      (unless yas-minor-mode
        (yas-minor-mode-on))
      (let ((snippet (let ((template (cdar (cl-mapcan #'(lambda (table) (yas--fetch table key))
                                                      (yas--get-snippet-tables mode)))))
                       (if template (yas--template-content template) nil))))
        (when (and yas-minor-mode snippet)
          (yas-expand-snippet snippet)
          (when (and (featurep 'evil) evil-mode)
            (evil-initialize-state 'insert))))))

  (defun +file-templates|add (regexp trigger mode &optional project-only-p)
    (define-auto-insert
      regexp
      (vector `(lambda () (+file-templates--expand ,trigger ',mode ,project-only-p)))))

  (mapc (lambda (args) (apply '+file-templates|add args))
        ;; General
        '(("/\\.gitignore$"                  "__"               gitignore-mode)
          ("/Dockerfile$"                    "__"               dockerfile-mode)
          ("/docker-compose.yml$"            "__"               yaml-mode)
          ;; Org-mode
          ("\\.org$"                         "__"               org-mode)
          ("/Work/.+\\.org$"                 "__project.org"    org-mode)
          ("/Invoices/.+\\.org$"             "__invoice.org"    org-mode)
          ("/Contacts/.+\\.org$"             "__contact.org"    org-mode)
          ;; C/C++
          ("/Makefile$"                      "__"               makefile-gmake-mode)
          ("/main\\.\\(cc\\|cpp\\)$"         "__main.cpp"       c++-mode)
          ("/win32_\\.\\(cc\\|cpp\\)$"       "__winmain.cpp"    c++-mode)
          ("\\.h\\(h\\|pp|xx\\)$"            "__hpp"            c++-mode)
          ("\\.\\(cc\\|cpp\\)$"              "__cpp"            c++-mode)
          ("\\.h$"                           "__h"              c-mode)
          ("\\.c$"                           "__c"              c-mode)
          ;; Elisp
          ("-test\\.el$"                     "__"               emacs-ert-mode)
          ("/.+\\.el$"                       "__initfile"       emacs-lisp-mode)
          (snippet-mode "__" snippet-mode)
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
          ("/bower\\.json$"                  "__bower.json"     json-mode)
          ("\\.\\(json\\|jshintrc\\)$"       "__"               json-mode)
          ("/gulpfile\\.js$"                 "__gulpfile.js"    js-mode)
          ;; Lua
          ("/main\\.lua$"                    "__main.lua"       love-mode)
          ("/conf\\.lua$"                    "__conf.lua"       love-mode)
          ;; Markdown
          ("\\.md$"                          "__"               markdown-mode)
          ;; PHP
          ("\\.class\\.php$"                 "__.class.php"     php-mode)
          ("\\.php$"                         "__"               php-mode)
          ;; Python
          ;;("tests?/test_.+\\.py$"         "__"                 nose-mode)
          ;;("/setup\\.py$"                 "__setup.py"         python-mode)
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
          ("\\.z?sh$"                        "__"               sh-mode))))

