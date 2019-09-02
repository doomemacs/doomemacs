;;; lang/scala/config.el -*- lexical-binding: t; -*-

(after! scala-mode
  (setq scala-indent:align-parameters t
        ;; indent block comments to first asterix, not second
        scala-indent:use-javadoc-style t)

  (setq-hook! 'scala-mode-hook comment-line-break-function #'+scala-comment-indent-new-line)

  (after! dtrt-indent
    (add-to-list 'dtrt-indent-hook-mapping-list '(scala-mode c/c++/java scala-indent:step)))

  (when (featurep! +lsp)
    (add-hook 'scala-mode-local-vars-hook #'lsp!)))


(use-package! ensime
  :unless (featurep! +lsp)
  :defer t
  :config
  (setq ensime-startup-snapshot-notification nil
        ensime-startup-notification nil
        ensime-eldoc-hints 'all
        ;; let DOOM handle company setup
        ensime-completion-style nil)

  (set-company-backend! 'scala-mode '(ensime-company company-yasnippet))

  ;; Fix void-variable imenu-auto-rescan error caused by `ensime--setup-imenu'
  ;; trying to make imenu variables buffer local before imenu is loaded.
  (require 'imenu))


(use-package! sbt-mode
  :after scala-mode
  :config (set-repl-handler! 'scala-mode #'+scala/open-repl))
