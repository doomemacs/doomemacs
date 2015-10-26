;;; core-auto-insert.el --- file templates

(use-package autoinsert
  :defer t
  :init
  (setq auto-insert-query nil)    ; Don't prompt before insertion
  (setq auto-insert-alist '())
  :config
  (auto-insert-mode 1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (add-template! "/\\.gitignore$"                  "@@"               'gitignore-mode)

  ;; C/C++
  (add-template! "/Makefile$"                      "@@"               'makefile-gmake-mode)
  (add-template! "/main\\.\\(cc\\|cpp\\)$"         "@@main.cpp"       'c++-mode)
  (add-template! "/win32_\\.\\(cc\\|cpp\\)$"       "@@winmain.cpp"    'c++-mode)
  (add-template! "\\.\\([Hh]\\|hpp\\)$"            "@@.h"             'c++-mode)
  (add-template! "\\.\\(cc\\|cpp\\)$"              "@@.cpp"           'c++-mode)
  (add-template! "\\.c$"                           "@@.c"             'c-mode)

  ;; Elisp
  (add-template! "\\.emacs\\.d/.+\\.el$"           "@@initfile"       'emacs-lisp-mode)
  (add-template! "\\.emacs\\.d/private/\\(snippets\\|templates\\)/.+$" "@@" 'snippet-mode)

  ;; Go
  (add-template! "/main\\.go$"                     "@@main.go"        'go-mode t)
  (add-template! "\\.go$"                          "@@.go"            'go-mode)

  ;; HTML
  (add-template! "\\.html$"                        "@@.html"          'web-mode)

  ;; Java
  (add-template! "/src/.+/.+\\.java$"              "@@"               'java-mode)
  (add-template! "/main\\.java$"                   "@@main"           'java-mode)
  (add-template! "/build\\.gradle$"                "@@build.gradle"   'android-mode)

  ;; Javascript
  (add-template! "\\.lbaction/.+/Info.plist$"                       "@@Info.plst"  'lb6-mode)
  (add-template! "\\.lbaction/.+/\\(default\\|suggestions\\)\\.js$" "@@default.js" 'lb6-mode)
  (add-template! "/package\\.json$"                "@@package.json"   'json-mode)
  (add-template! "\\.\\(json\\|jshintrc\\)$"       "@@"               'json-mode)

  ;; Lua
  (add-template! "/main\\.lua$"                    "@@main.lua"       'love-mode)
  (add-template! "/conf\\.lua$"                    "@@conf.lua"       'love-mode)

  ;; Markdown
  (add-template! "\\.md$"                          "@@"               'markdown-mode)
  (add-template! "/_posts/.+\\.md$"                "@@jekyll-post"    'markdown-mode)
  (add-template! "/_layouts/.+\\.html$"            "@@jekyll-layout.html" 'web-mode)

  ;; PHP
  (add-template! "\\.class\\.php$"                 "@@.class.php"     'php-mode)
  (add-template! "\\.php$"                         "@@"               'php-mode)

  ;; Python
  ;; (add-template! "tests?/test_.+\\.py$"         "@@"               'nose-mode)
  ;; (add-template! "/setup\\.py$"                 "@@setup.py"       'python-mode)
  (add-template! "\\.py$"                          "@@"               'python-mode)

  ;; Ruby
  (add-template! "/\\.rspec$"                      "@@.rspec"         'rspec-mode)
  (add-template! "/spec_helper\\.rb$"              "@@helper"         'rspec-mode t)
  (add-template! "_spec\\.rb$"                     "@@"               'rspec-mode t)
  (add-template! "/Rakefile$"                      "@@Rakefile"       'enh-ruby-mode t)
  (add-template! "/Gemfile$"                       "@@Gemfile"        'enh-ruby-mode t)
  (add-template! "\\.gemspec$"                     "@@.gemspec"       'enh-ruby-mode t)
  (add-template! "/lib/.+\\.rb$"                   "@@module"         'enh-ruby-mode t)
  (add-template! "\\.rb$"                          "@@"               'enh-ruby-mode)

  ;; Rust
  (add-template! "/Cargo.toml$"                    "@@Cargo.toml"     'rust-mode)
  (add-template! "/main\\.rs$"                     "@@main.rs"        'rust-mode)

  ;; SCSS
  (add-template! "/master\\.scss$"                 "@@master.scss"    'scss-mode)
  (add-template! "/normalize\\.scss$"              "@@normalize.scss" 'scss-mode)
  (add-template! "\\.scss$"                        "@@"               'scss-mode)

  ;; Shell scripts
  (add-template! "\\.z?sh$"                        "@@"               'sh-mode)
  )

(provide 'core-auto-insert)
;;; core-auto-insert.el ends here
