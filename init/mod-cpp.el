(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook
    (lambda()
      (use-package auto-complete-clang :ensure t)
      (use-package auto-complete-c-headers :ensure t)

      (setq ac-sources
            '(ac-source-clang
              ac-source-c-headers
              ac-source-yasnippet
              ac-source-words-in-same-mode-buffers
              ))))

(use-package glsl-mode :ensure t
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)))

(define-minor-mode cocos2d-mode
  "Buffer local minor mode for Cocos2D-x"
  :init-value nil
  :lighter " C2D"
  :keymap (make-sparse-keymap))

(defun cocoa2d-mode-maybe()
  (let ((root (projectile-project-root)))
    (if (or (string-match "[.-]cocos2d/" root)
            (file-exists-p (concat root ".cocos2d-mode")))
        (cocos-mode t))))

;;
(provide 'mod-cpp)
