;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! treesit :built-in t)
(when (> emacs-major-version 28)
  ;; (package! combobulate
  ;;   :recipe '(;; If pulled from emacsmirror, this would otherwise pull in test
  ;;             ;; repos that users don't need.
  ;;             :nonrecursive t
  ;;             ;; HACK: This package has terrible autoload ettiquette, eagerly
  ;;             ;;   loading a number of expensive packages at startup, so
  ;;             ;;   autoloads are handled manually in config.el
  ;;             :build (:not autoloads))
  ;;   :pin "59b64d66d66eb84da6a2cedd152b1692378af674")
  ;; (when (modulep! :editor evil +everywhere)
  ;;   (package! evil-textobj-tree-sitter
  ;;     :pin "bce236e5d2cc2fa4eae7d284ffd19ad18d46349a"))
  )
