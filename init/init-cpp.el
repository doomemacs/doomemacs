(provide 'init-cpp)

(setq-default c-basic-offset 4
              c-default-style "linux"
              c-tab-always-indent nil)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook! 'c-mode-common-hook
           (use-package auto-complete-clang)
           (use-package auto-complete-c-headers)

           (c-toggle-electric-state -1)
           (c-toggle-auto-newline -1)

           (defmap c-mode-map (kbd "DEL") nil)

           (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
           (c-set-offset 'inline-open '+)
           (c-set-offset 'block-open '+)
           (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
           (c-set-offset 'case-label '+)        ; indent case labels by c-indent-level, too

           (setq ac-sources
                 '(ac-source-clang
                   ac-source-c-headers
                   ac-source-yasnippet
                   ac-source-words-in-same-mode-buffers
                   )))

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

(associate-minor-mode "[.-]c2d/" cocoa2d-mode)
