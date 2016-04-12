;;; module-cc.el

(use-package cmake-mode
  :mode "CMakeLists\\.txt$"
  :config
  (require 'company-cmake)
  (define-company-backend! cmake-mode (cmake yasnippet)))

(use-package glsl-mode :mode ("\\.glsl\\'" "\\.vert\\'" "\\.frag\\'" "\\.geom\\'"))

(use-package cuda-mode :mode "\\.cuh?$")

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
  (add-hook! (c-mode c++-mode) '(narf|init-c/c++-settings))
  :config
  (setq c-tab-always-indent nil
        c-electric-flag nil)

  (map! (:map c-mode-base-map
          (:localleader
            :nv ";" 'narf/append-semicolon)))

  (sp-local-pair 'c++-mode "<" ">" :when '(narf/sp-point-is-template-p))
  (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
    (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    ;; Doxygen blocks
    (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

  (progn ;; C/C++
    (add-hook 'c++-mode-hook 'narf|init-c++-C11-highlights)
    (add-hook! (c-mode c++-mode)
      (electric-indent-local-mode +1)
      (setq electric-indent-chars '(?\n ?})))

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
              ad-do-it))))

  (progn ;; Obj-C
    (add-to-list 'magic-mode-alist
                 `(,(lambda ()
                      (and (string= (file-name-extension buffer-file-name) "h")
                           (re-search-forward "@\\<interface\\>"
                                              magic-mode-regexp-match-limit t)))
                   . objc-mode)))

  (use-package irony
    :config
    (setq irony-server-install-prefix (concat narf-temp-dir "/irony/"))
    (push "-std=c++11" irony-additional-clang-options)

    (require 'irony-eldoc)

    (require 'flycheck-irony)
    (flycheck-irony-setup)

    (require 'company-irony)
    (define-company-backend! c-mode (irony))
    (define-company-backend! c++-mode (irony))
    (define-company-backend! objc-mode (irony))

    (add-hook! c-mode-common-hook
      (when (memq major-mode '(c-mode c++-mode objc-mode))
        (flycheck-mode +1)
        (irony-mode +1)
        (eldoc-mode +1)
        (irony-eldoc +1)))))

(provide 'module-cc)
;;; module-cc.el ends here
