;;; feature/file-templates/config.el -*- lexical-binding: t; -*-

(require! :feature snippets)

(defvar +file-templates-dir
  (expand-file-name "templates/" (file-name-directory load-file-name))
  "The path to a directory of yasnippet folders to use for file templates.")

(def-setting! :file-template (regexp trigger mode &optional project-only-p)
  "Register a file template (associated with TRIGGER, the uuid of the target
snippet) for empty files that match REGEXP in MODE (a major mode symbol).

If PROJECT-ONLY-P is non-nil, the template won't be expanded if the buffer isn't
in a project."
  `(+file-templates-add (list ,regexp ,trigger ,mode ,project-only-p)))


;;
;; Plugins
;;

(def-package! autoinsert ; built-in
  :commands (auto-insert-mode auto-insert)
  :init
  (setq auto-insert-query nil  ; Don't prompt before insertion
        auto-insert-alist nil) ; Tabula rasa

  (after! yasnippet
    (push '+file-templates-dir yas-snippet-dirs))

  ;; load autoinsert as late as possible
  (defun +file-templates|init ()
    (and (not buffer-read-only)
         (bobp) (eobp)
         (remove-hook 'find-file-hook #'+file-templates|init)
         (auto-insert)))
  (add-hook 'find-file-hook #'+file-templates|init)

  :config
  (auto-insert-mode 1)

  (defun +file-templates--expand (key &optional mode project-only)
    "Auto insert a yasnippet snippet into the blank file."
    (when (if project-only (doom-project-p) t)
      (require 'yasnippet)
      (unless yas-minor-mode
        (yas-minor-mode-on))
      (when (and yas-minor-mode
                 (yas-expand-snippet
                  (yas--template-content
                   (cl-find key (yas--all-templates (yas--get-snippet-tables mode))
                            :key #'yas--template-key :test #'equal)))
                 (and (featurep 'evil) evil-mode)
                 (and yas--active-field-overlay
                      (overlay-buffer yas--active-field-overlay)
                      (overlay-get yas--active-field-overlay 'yas--field)))
        (evil-initialize-state 'insert))))

  (defun +file-templates-add (args)
    (cl-destructuring-bind (regexp trigger &optional mode project-only-p) args
      (push `(,regexp . (lambda () (+file-templates--expand ,trigger ',mode ,project-only-p)))
            auto-insert-alist)))

  (mapc #'+file-templates-add
        (let ((doom (concat "/" (regexp-opt '(".emacs.d" ".doom.d" "doom-emacs" ".config/doom")) "/")))
          `(;; General
            ("/\\.gitignore$"                 "__"               gitignore-mode)
            ("/Dockerfile$"                   "__"               dockerfile-mode)
            ("/docker-compose.yml$"           "__"               yaml-mode)
            ("/Makefile$"                     "__"               makefile-gmake-mode)
            ;; elisp
            ("\\.el$"                         "__initfile"       emacs-lisp-mode)
            ("/.dir-locals.el$"               nil)
            ("-test\\.el$"                    "__"               emacs-ert-mode)
            (,(concat doom ".+\\.el$")          "__doom-module"    emacs-lisp-mode)
            (,(concat doom ".*/packages\\.el$") "__doom-packages"  emacs-lisp-mode)
            (,(concat doom ".*/test/.+\\.el$")  "__doom-test"      emacs-lisp-mode)
            (snippet-mode "__" snippet-mode)
            ;; C/C++
            ("\\.h$"                           "__h"              c-mode)
            ("\\.c$"                           "__c"              c-mode)
            ("\\.h\\(h\\|pp|xx\\)$"            "__hpp"            c++-mode)
            ("\\.\\(cc\\|cpp\\)$"              "__cpp"            c++-mode)
            ("/main\\.\\(cc\\|cpp\\)$"         "__main.cpp"       c++-mode)
            ("/win32_\\.\\(cc\\|cpp\\)$"       "__winmain.cpp"    c++-mode)
            ;; go
            ("\\.go$"                          "__.go"            go-mode)
            ("/main\\.go$"                     "__main.go"        go-mode t)
            ;; web-mode
            ("\\.html$"                        "__.html"          web-mode)
            ("\\.scss$"                        "__"               scss-mode)
            ("/master\\.scss$"                 "__master.scss"    scss-mode)
            ("/normalize\\.scss$"              "__normalize.scss" scss-mode)
            ;; java
            ("/src/.+\\.java$"                 "__"               java-mode)
            ("/main\\.java$"                   "__main"           java-mode)
            ("/build\\.gradle$"                "__build.gradle"   android-mode)
            ;; javascript
            ("\\.\\(json\\|jshintrc\\)$"       "__"                  json-mode)
            ("/package\\.json$"                "__package.json"      json-mode)
            ("/bower\\.json$"                  "__bower.json"        json-mode)
            ("/gulpfile\\.js$"                 "__gulpfile.js"       js-mode)
            ("/webpack\\.config\\.js$"         "__webpack.config.js" js-mode)
            ;; Lua
            ("/main\\.lua$"                    "__main.lua"       love-mode)
            ("/conf\\.lua$"                    "__conf.lua"       love-mode)
            ;; Markdown
            ("\\.md$"                          "__"               markdown-mode)
            ;; Org
            ("\\.org$"                                          "__"            org-mode)
            (,(concat doom "/README\\.org$")   "__doom-readme"    org-mode)
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
            ;; Slim
            ("/\\(index\\|main\\)\\.slim$"     "__"               slim-mode)
            ;; Shell scripts
            ("\\.z?sh$"                        "__"               sh-mode)
            ("\\.fish$"                        "__"               fish-mode)
            ("\\.zunit$"                       "__zunit"          sh-mode)))))
