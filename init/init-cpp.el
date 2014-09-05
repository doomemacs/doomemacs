(provide 'init-cpp)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook
    (lambda()
      (use-package auto-complete-clang)
      (use-package auto-complete-c-headers)

      (setq ac-sources
            '(ac-source-clang
              ac-source-c-headers
              ac-source-yasnippet
              ac-source-words-in-same-mode-buffers
              ))))

(use-package glsl-mode
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)))

(define-minor-mode cocos2d-mode
  "Buffer local minor mode for Cocos2D-x"
  :init-value nil
  :lighter " C2D"
  :keymap (make-sparse-keymap))

(associate-mode "[.-]c2d/" cocoa2d-mode t)
