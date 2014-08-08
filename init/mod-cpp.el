
(mapc 'my/install-package '(auto-complete-clang auto-complete-c-headers))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c-mode-common-hook
          (lambda()
            (use-package auto-complete-clang)
            (use-package auto-complete-c-headers)

            (setq ac-sources '(ac-source-clang ac-source-c-headers ac-source-yasnippet))))

;; TODO Better SFML & build settings

;;
(provide 'mod-cpp)
