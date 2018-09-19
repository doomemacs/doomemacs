;;; lang/scala/config.el -*- lexical-binding: t; -*-

(after! scala-mode
  (setq scala-indent:align-parameters t)
  (after! dtrt-indent
    (add-to-list 'dtrt-indent-hook-mapping-list '(scala-mode c/c++/java scala-indent:step))))


(after! ensime
  (setq ensime-startup-snapshot-notification nil
        ensime-startup-notification nil
        ensime-eldoc-hints 'all
        ;; let DOOM handle company setup
        ensime-completion-style nil)

  (set-company-backend! 'scala-mode '(ensime-company company-yasnippet))

  ;; Fix void-variable imenu-auto-rescan error caused by `ensime--setup-imenu'
  ;; trying to make imenu variables buffer local before imenu is loaded.
  (require 'imenu))


(def-package! sbt-mode
  :after scala-mode
  :config (set-repl-handler! 'scala-mode #'run-scala))
