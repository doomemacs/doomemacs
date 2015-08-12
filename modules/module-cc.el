;;; module-cc.el

(use-package cmake-mode
  :mode "CMakeLists\\.txt$"
  :config
  (after! company
    (require 'company-cmake)
    (add-company-backend! cmake-mode (cmake yasnippet))))

(use-package glsl-mode
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)))

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
  (associate! c++-mode  :match "\\.h$")
  (associate! objc-mode :match "\\.mm$")
  :config
  (setq c-basic-offset 4
        c-tab-always-indent nil
        c-electric-flag nil)

  (progn ; C/C++ Settings
    (when IS-MAC
      (after! flycheck
        (setq-default flycheck-c/c++-clang-executable  "clang++"
                      flycheck-clang-include-path      '("/usr/local/include"))))

    (after! company
      ;; TODO Clang is *really* slow in larger projects, maybe replace it with
      ;; irony-mode or ycmd?
      (add-company-backend! c-mode    (c-headers clang))
      (add-company-backend! c++-mode  (c-headers clang))
      (add-company-backend! objc-mode (c-headers xcode)))

    (add-hook! (c-mode c++-mode) 'narf|init-c/c++-settings)
    (add-hook! c++-mode 'narf|init-c++-C11-highlights)
    (add-hook! c++-mode (setq flycheck-clang-language-standard "c++11"
                              flycheck-clang-standard-library  "libc++"))

    ;; Fix enum and C++11 lambda indentation
    (defadvice c-lineup-arglist (around c-lineup-arglist-indent-fix activate)
      "Improve indentation of continued C++11 lambda function opened as argument."
      (setq ad-return-value
            (if (and (equal major-mode 'c++-mode)
                     (ignore-errors
                       (save-excursion
                         (goto-char (c-langelem-pos langelem))
                         ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                         ;;   and with unclosed brace.
                         (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
                0                           ; no additional indent
              ad-do-it))))

  (progn ; Obj-C
    (add-to-list 'magic-mode-alist
                 `(,(lambda ()
                      (and (string= (file-name-extension buffer-file-name) "h")
                           (re-search-forward "@\\<interface\\>"
                                              magic-mode-regexp-match-limit t)))
                   . objc-mode))
    (after! flycheck (add-hook! objc-mode (require 'flycheck-objc)))))

(provide 'module-cc)
;;; module-cc.el ends here
