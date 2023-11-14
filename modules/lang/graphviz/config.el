;;; lang/graphviz/config.el -*- lexical-binding: t; -*-

(use-package! graphviz-dot-mode)

(after! (:and graphviz-dot-mode org-babel)
  ;; We can't use the doom emacs default autoloading behaviour here, since
  ;; graphviz-dot-mode is not called "dot-mode" (and there is no "ob-dot-mode").
  ;; This is how you register dot-mode according to the docs:
  ;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-dot.html. We do
  ;; that lazily.
  (add-to-list 'org-babel-load-languages '(dot . t)))

(after! (:and graphviz-dot-mode flycheck)
  ;; TODO: should probably be upstreamed into flycheck
  (flycheck-define-checker graphviz-dot
    "A checker using graphviz dot."
    :command ("dot")
    :standard-input t
    :error-patterns ((error line-start "Error: <stdin>: " (message "syntax error in line " line (* nonl)))
                     ;; I have no idea if this can actually be printed
                     (error line-start "Error: <stdin>: " (message)))
    :modes graphviz-dot-mode)
  (add-to-list 'flycheck-checkers 'graphviz-dot))

(after! org-src
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot-mode)))
