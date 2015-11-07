;;; core-auto-insert.el --- file templates

(use-package autoinsert
  :defer t
  :init
  (setq auto-insert-query nil)    ; Don't prompt before insertion
  (setq auto-insert-alist '())
  :config
  (auto-insert-mode 1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (add-template! "/\\.gitignore$"                  "__"               'gitignore-mode)

  ;; C/C++
  (add-template! "/Makefile$"                      "__"               'makefile-gmake-mode)
  (add-template! "/main\\.\\(cc\\|cpp\\)$"         "__main.cpp"       'c++-mode)
  (add-template! "/win32_\\.\\(cc\\|cpp\\)$"       "__winmain.cpp"    'c++-mode)
  (add-template! "\\.\\([Hh]\\|hpp\\)$"            "__.h"             'c++-mode)
  (add-template! "\\.\\(cc\\|cpp\\)$"              "__.cpp"           'c++-mode)
  (add-template! "\\.c$"                           "__.c"             'c-mode)

  ;; Elisp
  (add-template! "\\.emacs\\.d/.+\\.el$"           "__initfile"       'emacs-lisp-mode)
  (add-template! "\\.emacs\\.d/private/\\(snippets\\|templates\\)/.+$" "__" 'snippet-mode)

  ;; Go
  (add-template! "/main\\.go$"                     "__main.go"        'go-mode t)
  (add-template! "\\.go$"                          "__.go"            'go-mode)

  ;; HTML
  (add-template! "\\.html$"                        "__.html"           'web-mode)

  ;; java
  (add-template! "/src/.+/.+\\.java$"              "__"               'java-mode)
  (add-template! "/main\\.java$"                   "__main"           'java-mode)
  (add-template! "/build\\.gradle$"                "__build.gradle"   'android-mode)

  ;; Javascript
  (add-template! "\\.lbaction/.+/Info.plist$"                       "__Info.plst"  'lb6-mode)
  (add-template! "\\.lbaction/.+/\\(default\\|suggestions\\)\\.js$" "__default.js" 'lb6-mode)
  (add-template! "/package\\.json$"                "__package.json"   'json-mode)
  (add-template! "\\.\\(json\\|jshintrc\\)$"       "__"               'json-mode)

  ;; Lua
  (add-template! "/main\\.lua$"                    "__main.lua"       'love-mode)
  (add-template! "/conf\\.lua$"                    "__conf.lua"       'love-mode)

  ;; Markdown
  (add-template! "\\.md$"                          "__"               'markdown-mode)
  (add-template! "/_posts/.+\\.md$"                "__jekyll-post"    'markdown-mode)
  (add-template! "/_layouts/.+\\.html$"            "__jekyll-layout.html" 'web-mode)

  ;; PHP
  (add-template! "\\.class\\.php$"                 "__.class.php"     'php-mode)
  (add-template! "\\.php$"                         "__"               'php-mode)

  ;; Python
  ;; (add-template! "tests?/test_.+\\.py$"         "__"               'nose-mode)
  ;; (add-template! "/setup\\.py$"                 "__setup.py"       'python-mode)
  (add-template! "\\.py$"                          "__"               'python-mode)

  ;; Ruby
  (add-template! "/\\.rspec$"                      "__.rspec"         'rspec-mode)
  (add-template! "/spec_helper\\.rb$"              "__helper"         'rspec-mode t)
  (add-template! "_spec\\.rb$"                     "__"               'rspec-mode t)
  (add-template! "/Rakefile$"                      "__Rakefile"       'enh-ruby-mode t)
  (add-template! "/Gemfile$"                       "__Gemfile"        'enh-ruby-mode t)
  (add-template! "\\.gemspec$"                     "__.gemspec"       'enh-ruby-mode t)
  (add-template! "/lib/.+\\.rb$"                   "__module"         'enh-ruby-mode t)
  (add-template! "\\.rb$"                          "__"               'enh-ruby-mode)

  ;; Rust
  (add-template! "/Cargo.toml$"                    "__Cargo.toml"     'rust-mode)
  (add-template! "/main\\.rs$"                     "__main.rs"        'rust-mode)

  ;; SCSS
  (add-template! "/master\\.scss$"                 "__master.scss"    'scss-mode)
  (add-template! "/normalize\\.scss$"              "__normalize.scss" 'scss-mode)
  (add-template! "\\.scss$"                        "__"               'scss-mode)

  ;; Shell scripts
  (add-template! "\\.z?sh$"                        "__"               'sh-mode)
  )

(provide 'core-auto-insert)
;;; core-auto-insert.el ends here
