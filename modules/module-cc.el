;;; module-cc.el

(use-package cmake-mode
  :mode "CMakeLists\\.txt$"
  :config
  (after! company
    (require 'company-cmake)
    (define-company-backend! cmake-mode (cmake yasnippet))))

(use-package glsl-mode
  :mode ("\\.glsl\\'" "\\.vert\\'" "\\.frag\\'" "\\.geom\\'"))

(use-package cc-mode
  :defines (c-syntactic-context)
  :functions (c-toggle-electric-state c-toggle-auto-newline
              c-skip-comments-and-strings c-forward-sws c-end-of-macro
              c-font-lock-invalid-string csharp-log c-font-lock-declarators
              c-get-lang-constant c-forward-keyword-clause
              c-fontify-recorded-types-and-refs c-forward-type imenu--split
              c-backward-sws c-determine-limit c-beginning-of-decl-1)
  :commands (c-mode c++-mode objc-mode java-mode)
  :init
  (associate! objc-mode :match "\\.mm$")
  (add-hook! (c-mode c++-mode) '(flycheck-mode narf|init-c/c++-settings))
  :config
  (setq c-basic-offset 4
        c-tab-always-indent nil
        c-electric-flag nil)

  ;; C/C++ Settings
  (add-hook! (c-mode c++-mode)
    (electric-indent-local-mode +1)
    (setq electric-indent-chars '(?\n ?})))
  (add-hook! c++-mode 'narf|init-c++-C11-highlights)

  (when IS-MAC
    (after! company
      (setq-default company-c-headers-path-system
                    (append '("/usr/include/" "/usr/local/include")
                            (f-directories "/usr/include/c++/")
                            (f-directories "/usr/local/include/c++/"))))
    (after! flycheck
      (setq-default flycheck-clang-include-path '("/usr/local/include")
                    flycheck-gcc-include-path   '("/usr/local/include"))))

  (after! flycheck
    (add-hook! c++-mode (setq flycheck-clang-language-standard "c++11"
                              flycheck-clang-standard-library  "libc++")))

  (after! company
    ;; TODO Clang is *really* slow in larger projects, maybe replace it with
    ;; irony-mode or ycmd?
    (define-company-backend! c-mode-common (c-headers clang xcode)))

  ;; Fix enum and C++11 lambda indentation
  (defadvice c-lineup-arglist (around c-lineup-arglist-indent-fix activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq ad-return-value
          (if (and (eq major-mode 'c++-mode)
                   (ignore-errors
                     (save-excursion
                       (goto-char (c-langelem-pos langelem))
                       ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                       ;;   and with unclosed brace.
                       (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
              0                           ; no additional indent
            ad-do-it)))

  ;; Obj-C
  (add-to-list 'magic-mode-alist
               `(,(lambda ()
                    (and (string= (file-name-extension buffer-file-name) "h")
                         (re-search-forward "@\\<interface\\>"
                                            magic-mode-regexp-match-limit t)))
                 . objc-mode))
  (after! flycheck (add-hook! objc-mode (require 'flycheck-objc))))

(provide 'module-cc)
;;; module-cc.el ends here
