;;; module-cc.el --- C, C++, and Objective-C

(use-package cc-mode
  :commands (c-mode c++-mode objc-mode java-mode)
  :mode ("\\.mm" . objc-mode)
  :init
  (add-hook! c++-mode '(highlight-numbers-mode doom|init-c++-C11-highlights))
  (add-hook 'c-initialization-hook 'doom|init-c/c++-settings)

  ;; C++ header files
  (push (cons (lambda () (and (f-ext? buffer-file-name "h")
                         (or (f-exists? (f-swap-ext buffer-file-name "cpp"))
                             (awhen (car-safe (projectile-get-other-files (buffer-file-name) (projectile-current-project-files)))
                               (f-ext? it "cpp")))))
              'c++-mode)
        magic-mode-alist)

  ;; Obj-C
  (push (cons (lambda () (and (f-ext? buffer-file-name "h")
                         (re-search-forward "@\\<interface\\>" magic-mode-regexp-match-limit t)))
              'objc-mode)
        magic-mode-alist)

  :config
  (def-electric! (c-mode c++-mode objc-mode) :chars (?\n ?\}))
  (def-company-backend! (c-mode c++-mode objc-mode) (irony-c-headers irony))
  (def-docset! c-mode "c,sdl,glib,gl2,gl3,gl4,manpages")
  (def-docset! c++-mode "cpp,sdl,net,boost,qt,cvcpp,cocos2dx,c,manpages")

  (setq c-tab-always-indent nil
        c-electric-flag nil)

  (map! :map c-mode-base-map (:localleader :nv ";" 'doom/append-semicolon))

  (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
    (sp-local-pair "<" ">" :when '(doom/sp-point-is-template-p doom/sp-point-after-include-p))
    (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    ;; Doxygen blocks
    (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

  ;; Improve indentation of inline lambdas in C++11
  (advice-add 'c-lineup-arglist :around 'doom/c-lineup-arglist))

(use-package irony
  :after cc-mode
  :config
  (setq irony-server-install-prefix (concat doom-temp-dir "/irony/"))
  (add-hook! c++-mode
    (make-variable-buffer-local 'irony-additional-clang-options)
    (push "-std=c++11" irony-additional-clang-options))

  (require 'irony-eldoc)
  (require 'company-irony)
  (require 'company-irony-c-headers)
  (require 'flycheck-irony)
  (flycheck-irony-setup)

  ;; some c-mode dervied modes wrongfully trigger these hooks (like php-mode)
  (add-hook! (c-mode c++-mode objc-mode)
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

(use-package opencl-mode :mode "\\.cl$")

(use-package demangle-mode
  :commands demangle-mode
  :init (add-hook 'llvm-mode-hook 'demangle-mode))

(provide 'module-cc)
;;; module-cc.el ends here
