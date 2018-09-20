;;; lang/ocaml/config.el -*- lexical-binding: t; -*-

;; def-project-mode!/associate! doesn't work when a
;; package is lazy loaded, and everything is compiled

(def-package! tuareg
  :defer t ;; modes set by autoload
  :config
  ;; tuareg-mode has the prettify symbols itself
  (set-pretty-symbols! 'tuareg-mode :alist
    (append tuareg-prettify-symbols-basic-alist
            tuareg-prettify-symbols-extra-alist))
  (setq tuareg-prettify-symbols-full t)
  ;; Use opam to set environment
  (setq tuareg-opam-insinuate t)
  (tuareg-opam-update-env (tuareg-opam-current-compiler))
  ;; Spell-check comments
  (when (featurep! :feature spellcheck)
    (add-hook 'tuareg-mode-hook #'flyspell-prog-mode)))

(def-package! merlin
  :after tuareg
  :init
  (set-lookup-handlers! 'tuareg-mode
    :definition #'merlin-locate
    :references #'merlin-occurrences
    :documentation #'merlin-document)
  (defun +ocaml|init-merlin ()
    (when (and (projectile-locate-dominating-file default-directory ".merlin")
               (executable-find "ocamlmerlin"))
      (merlin-mode)))
  (add-hook 'tuareg-mode-hook #'+ocaml|init-merlin)

  :config
  (map! :map tuareg-mode-map
        :localleader
        :n "t" #'merlin-type-enclosing
        :n "a" #'tuareg-find-alternate-file)
  (set-company-backend! 'tuareg-mode 'merlin-company-backend)
  (setq merlin-completion-with-doc t))

(def-package! flycheck-ocaml
  :when (featurep! :feature syntax-checker)
  :after merlin
  :config
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)
  ;; Enable Flycheck checker
  (flycheck-ocaml-setup))

(def-package! ocp-indent
  ;; must be careful to always defer this, it has autoloads that adds hooks
  ;; which we do not want if the executable can't be found
  :defer t
  :init
  (defun +ocaml|init-ocp-indent ()
    (when (executable-find "ocp-indent")
      (ocp-setup-indent)))
  (add-hook 'tuareg-mode-hook #'+ocaml|init-ocp-indent))

(def-package! utop
  :defer t ;; loaded by hook below
  :when (featurep! :feature eval)
  :init
  (set-repl-handler! 'tuareg-mode #'utop)
  (set-eval-handler! 'tuareg-mode #'utop-eval-region)
  (defun +ocaml|init-utop ()
    (when (executable-find "utop")
      (utop-minor-mode)))
  (add-hook 'tuareg-mode-hook #'+ocaml|init-utop))

(def-package! ocamlformat
  :after tuareg
  :commands (ocamlformat)
  :init
  (set-formatter! 'ocamlformat #'ocamlformat
    :modes '(caml-mode tuareg-mode))
  (defun +ocaml|init-ocamlformat ()
    (setq +format-with 'ocp-indent)
    (when (and (executable-find "ocamlformat")
               (locate-dominating-file default-directory ".ocamlformat"))
      (setq +format-with 'ocamlformat)))
  (add-hook 'tuareg-mode-hook #'+ocaml|init-ocamlformat))
