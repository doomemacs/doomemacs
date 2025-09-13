;;; lang/ada/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(defun +ada-common-config (mode)
  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append))

  (map! :map ,(intern (format "%s-map" mode))
        :localleader
        :desc "Build Alire Project" "b" #'+ada/alr-build
        :desc "Run Alire Project"   "r" #'+ada/alr-run
        :desc "Clean Alire Project" "c" #'+ada/alr-clean))


(use-package! ada-mode
  :mode "\\.gpr\\'"
  :config
  (+ada-common-config 'ada-mode))


(use-package! ada-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'ada-mode 'ada-ts-mode
    '((ada :url "https://github.com/briot/tree-sitter-ada")))
  :config
  (+ada-common-config 'ada-ts-mode)

  (setq ada-ts-mode-grammar-install nil)  ; redundant w/ `treesit-auto-install-grammar'

  ;; HACK: `ada-ts-mode' sets buffer-local values for
  ;;   `treesit-language-source-alist' and `eglot-server-program' during major
  ;;   mode activation. This is poor ettiquite, overshadowing any user changes
  ;;   to the global values of these variables. Not to mention, it's redundant
  ;;   with ~set-tree-sitter!~ and ~eglot~ already handling support for
  ;;   ada-ts-mode.
  ;; REVIEW: Undo buffer-local changes to these variables upstream.
  (defadvice! +ada--suppress-side-effects-a (&rest _)
    :after #'ada-ts-mode
    (kill-local-variable 'treesit-language-source-alist)
    (kill-local-variable 'eglot-server-programs)))
