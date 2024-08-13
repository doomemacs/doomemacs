;;; lang/ocaml/config.el -*- lexical-binding: t; -*-

(after! projectile
  (pushnew! projectile-project-root-files "dune-project"))

;;
;;; Packages

(when (modulep! +lsp)
  (add-hook! '(tuareg-mode-local-vars-hook
               reason-mode-local-vars-hook)
             :append #'lsp!))


(after! tuareg
  (set-formatter! 'ocamlformat
    '("ocamlformat" "-" "--name" filepath "--enable-outside-detected-project"
      (if (locate-dominating-file default-directory ".ocamlformat")
          (pcase (apheleia-formatters-extension-p "eliom" "eliomi")
            ("eliom"  '("--impl"))
            ("eliomi" '("--intf")))
        '("--profile=ocamlformat"))))
  ;; tuareg-mode has the prettify symbols itself
  (set-ligatures! 'tuareg-mode :alist
    (append tuareg-prettify-symbols-basic-alist
            tuareg-prettify-symbols-extra-alist))
  ;; harmless if `prettify-symbols-mode' isn't active
  (setq tuareg-prettify-symbols-full t)

  (setq-hook! 'tuareg-mode-hook
    comment-line-break-function #'+ocaml/comment-indent-new-line)

  (map! :localleader
        :map tuareg-mode-map
        "a" #'tuareg-find-alternate-file)

  (use-package! utop
    :when (modulep! :tools eval)
    :hook (tuareg-mode-local-vars . +ocaml-init-utop-h)
    :init
    (set-repl-handler! 'tuareg-mode #'utop)
    (set-eval-handler! 'tuareg-mode #'utop-eval-region)
    (defun +ocaml-init-utop-h ()
      (when (executable-find "utop")
        (utop-minor-mode)))
    :config
    (set-popup-rule! "^\\*utop\\*" :quit nil)))


(use-package! merlin
  :unless (modulep! +lsp)
  :hook (tuareg-mode-local-vars . +ocaml-init-merlin-h)
  :init
  (defun +ocaml-init-merlin-h ()
    "Activate `merlin-mode' if the ocamlmerlin executable exists."
    (when (executable-find "ocamlmerlin")
      (merlin-mode)))

  (after! tuareg
    (set-company-backend! 'tuareg-mode 'merlin-company-backend)
    (set-lookup-handlers! 'tuareg-mode :async t
      :definition #'merlin-locate
      :references #'merlin-occurrences
      :documentation #'merlin-document))
  :config
  (setq merlin-completion-with-doc t)

  (map! :localleader
        :map tuareg-mode-map
        "t" #'merlin-type-enclosing)

  (use-package! flycheck-ocaml
    :when (and (modulep! :checkers syntax)
               (not (modulep! :checkers syntax +flymake)))

    :hook (merlin-mode . +ocaml-init-flycheck-h)
    :config
    (defun +ocaml-init-flycheck-h ()
      "Activate `flycheck-ocaml`"
      ;; Disable Merlin's own error checking
      (setq merlin-error-after-save nil)
      ;; Enable Flycheck checker
      (flycheck-ocaml-setup)))

  (use-package! merlin-eldoc
    :hook (merlin-mode . merlin-eldoc-setup))

  (use-package! merlin-iedit
    :when (modulep! :editor multiple-cursors)
    :defer t
    :init
    (map! :map tuareg-mode-map
          :v "R" #'merlin-iedit-occurrences))

  (use-package! merlin-imenu
    :when (modulep! :emacs imenu)
    :hook (merlin-mode . merlin-use-merlin-imenu)))


(use-package! ocp-indent
  ;; must be careful to always defer this, it has autoloads that adds hooks
  ;; which we do not want if the executable can't be found
  :hook (tuareg-mode-local-vars . +ocaml-init-ocp-indent-h)
  :config
  (defun +ocaml-init-ocp-indent-h ()
    "Run `ocp-setup-indent', so long as the ocp-indent binary exists."
    (when (executable-find "ocp-indent")
      (ocp-setup-indent))))



(use-package! opam-switch-mode
  :hook (tuareg-mode-local-vars . +ocaml-init-opam-switch-h)
  :init
  (map! :localleader
        :map tuareg-mode-map
        "w" #'opam-switch-set-switch)

  (defun +ocaml-init-opam-switch-h ()
    "Activate `opam-switch-mode' if the opam executable exists."
    (when (executable-find "opam")
      (opam-switch-mode)))
  :config
  ;; Use opam to set environment
  (setq tuareg-opam-insinuate t)
  (opam-switch-set-switch (tuareg-opam-current-compiler)))

;; Tree sitter
(eval-when! (modulep! +tree-sitter)
  (add-hook! 'tuareg-mode-local-vars-hook #'tree-sitter!))
