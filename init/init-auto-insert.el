(use-package autoinsert
  :init (auto-insert-mode 1)
  :config
  (progn
    ;; (setq auto-insert-directory "%/.emacs.d/templates/")
    (setq auto-insert-query nil)    ; Don't prompt before insertion
    (setq auto-insert-alist '())))

(after "yasnippet"
    (defmacro template (file-regexp uuid mode &optional project-only)
      `(define-auto-insert ,file-regexp '(lambda () (my--template-insert ,uuid ,mode ,project-only))))

    (defun my--template-insert (uuid mode &optional project-only)
      "Expand snippet template in MODE by its UUID"
      (unless (and project-only
                   (not (projectile-project-p)))
        (yas-expand-snippet
         (yas--template-content
          (yas--get-template-by-uuid mode uuid)) (point-min) (point-max))
        (evil-insert-state)))

    (template "/\\.gitignore$"                                        "%%"               'gitignore-mode)

    ;; C/C++
    (template "/Makefile$"                                            "%%"               'makefile-gmake-mode)
    (template "/main\\.\\(cc\\|cpp\\)$"                               "%main.cpp%"       'c++-mode)
    (template "/win32_\\.\\(cc\\|cpp\\)$"                             "%winmain.cpp%"    'c++-mode)
    (template "\\.\\([Hh]\\|hpp\\)$"                                  "%.h%"             'c++-mode)
    (template "\\.\\([Cc]\\|cc\\|cpp\\)$"                             "%.cpp%"           'c++-mode)

    ;; Shell scripts
    (template "\\.z?sh$"                                              "%%"               'sh-mode)

    ;; Ruby
    (template "/spec_helper\\.rb$"                                    "%helper%"         'rspec-mode t)
    (template "_spec\\.rb$"                                           "%%"               'rspec-mode t)
    (template "/\\.rspec$"                                            "%.rspec%"         'rspec-mode)
    (template "/Rakefile$"                                            "%Rakefile%"       'ruby-mode t)
    (template "/Gemfile$"                                             "%Gemfile%"        'ruby-mode t)
    ;; (template "\\.gemspec$"                                        "%.gemspec%"       'ruby-mode t)
    (template "/lib/.+\\.rb$"                                         "%module%"         'ruby-mode t)
    (template "\\.rb$"                                                "%%"               'ruby-mode)

    ;; ;; Python
    ;; (template "tests?/test_.+\\.py$"                              "%%"               'nose-mode)
    ;; (template "/setup\\.py$"                                      "%setup%"          'python-mode)
    (template "\\.py$"                                                  "%%"               'python-mode)

    ;; ;; PHP
    ;; (template "\\.class\\.php$"                                   "%class%"          'php-mode)
    ;; (template "\\.php$"                                           "%%"               'php-mode)

    ;; ;; Markdown
    (template "/README\\.md$"                                        "%README.md%"      'markdown-mode)
    ;; (template "/_posts/.+\\.md$"                                  "%jekyll-post"     'markdown-mode)
    ;; (template "/_layouts/.+\\.html$"                              "%jekyll-layout%"  'web-mode)

    ;; ;; Javascript
    ;; (template "\\.lbaction/Contents/Info.plist$"                  "%lb6%"            'nxml-mode)
    ;; (template "\\.lbaction/.+/\\(default\\|suggestions\\)\\.js$"  "%lb6%"            'js-mode)
    ;; (template "/package\\.json$"                                  "%package.json%"   'json-mode)
    ;; (template "\\.\\(json\\|jshintrc\\)$"                         "%%"               'json-mode)

    ;; ;; SCSS
    ;; (template "/master\\.scss$"                                   "%master%"         'scss-mode)
    ;; (template "/normalize\\.scss$"                                "%normalize%"      'scss-mode)
    ;; (template "\\.scss$"                                          "%%"               'scss-mode)

    ;; ;; HTML
    ;; (template "\\.html$"                                          "%%"               'web-mode)

     ;; Lua
    ;; (template "\\.love/main\\.lua$"                               "%love.main%"      'lua-mode)
    ;; (template "\\.love/conf\\.lua$"                               "%love.conf%"      'lua-mode)
    ;; (template "\\.lua$"                                           "%%"               'lua-mode)

    ;; ;; Java
    (template "/src/.+/.+\\.java$"                                   "%%"               'java-mode)
    ;; (template "\\.gradle$"                                        "%%"               'groovy-mode)

    ;; ;; Elisp
    (template "\\.emacs\\.d/.+\\.el$"                                "%initfile%"       'emacs-lisp-mode)
    (template "\\.emacs\\.d/snippets/.+$"                            "%%"               'snippet-mode))


(provide 'init-auto-insert)
;;; init-auto-insert.el ends here
