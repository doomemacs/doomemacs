;;; module-cc.el --- C, C++, and Objective-C

(use-package cc-mode
  :commands (c-mode c++-mode objc-mode java-mode)
  :init
  (associate! objc-mode :match "\\.mm$")
  (add-hook! 'c++-mode-hook '(highlight-numbers-mode narf|init-c++-C11-highlights))
  (add-hook 'c-initialization-hook 'narf|init-c/c++-settings)

  ;; C++ header files
  (push `(,(lambda () (and (f-ext? buffer-file-name "h")
                      (or (f-exists? (f-swap-ext buffer-file-name "cpp"))
                          (awhen (car-safe (projectile-get-other-files (buffer-file-name) (projectile-current-project-files)))
                            (f-ext? it "cpp")))))
          . c++-mode)
        magic-mode-alist)

  ;; Obj-C
  (push `(,(lambda () (and (f-ext? buffer-file-name "h")
                      (re-search-forward "@\\<interface\\>" magic-mode-regexp-match-limit t)))
          . objc-mode)
        magic-mode-alist)

  :config
  (def-electric! (c-mode c++-mode objc-mode) :chars (?\n ?\}))
  (def-company-backend! (c-mode c++-mode objc-mode) (irony-c-headers irony))

  (setq c-tab-always-indent nil
        c-electric-flag nil)

  (map! :map c-mode-base-map (:localleader :nv ";" 'narf/append-semicolon))

  (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
    (sp-local-pair "<" ">" :when '(narf/sp-point-is-template-p narf/sp-point-after-include-p))
    (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    ;; Doxygen blocks
    (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

  ;; Improve indentation of inline lambdas in C++11
  (advice-add 'c-lineup-arglist :around 'narf/c-lineup-arglist))

(use-package irony
  :after cc-mode
  :config
  (setq irony-server-install-prefix (concat narf-temp-dir "/irony/"))
  (push "-std=c++11" irony-additional-clang-options)

  (require 'irony-eldoc)
  (require 'company-irony)
  (require 'company-irony-c-headers)
  (require 'flycheck-irony)
  (flycheck-irony-setup)

  ;; some c-mode dervied modes wrongfully trigger these hooks (like php-mode)
  (add-hook! (c-mode c++-mode ojbc-mode)
    (when (memq major-mode '(c-mode c++-mode objc-mode))
      (flycheck-mode +1)
      (irony-mode +1)
      (eldoc-mode +1)
      (irony-eldoc +1))))

(use-package disaster :commands (disaster))

;;
(use-package cmake-mode
  :mode "CMakeLists\\.txt$"
  :config (def-company-backend! cmake-mode (cmake yasnippet)))
(use-package company-cmake :after cmake-mode)

(use-package glsl-mode :mode ("\\.glsl\\'" "\\.vert\\'" "\\.frag\\'" "\\.geom\\'"))

(use-package cuda-mode :mode "\\.cuh?$")

(provide 'module-cc)
;;; module-cc.el ends here
