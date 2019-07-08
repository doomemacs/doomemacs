;;; lang/ocaml/config.el -*- lexical-binding: t; -*-

(when (featurep! +lsp)
  (add-hook! (tuareg-mode-local-vars reason-mode-local-vars)
    #'lsp!))


(after! tuareg
  ;; tuareg-mode has the prettify symbols itself
  (set-pretty-symbols! 'tuareg-mode :alist
    (append tuareg-prettify-symbols-basic-alist
            tuareg-prettify-symbols-extra-alist))
  ;; harmless if `prettify-symbols-mode' isn't active
  (setq tuareg-prettify-symbols-full t)

  ;; Use opam to set environment
  (setq tuareg-opam-insinuate t)
  (tuareg-opam-update-env (tuareg-opam-current-compiler))

  ;; Spell-check comments
  (when (featurep! :tools flyspell)
    (add-hook 'tuareg-mode-local-vars-hook #'flyspell-prog-mode))

  ;; Ensure asterixes in block comments have at least one space of indentation
  (setq-hook! 'tuareg-mode-hook
    comment-line-break-function #'+ocaml/comment-indent-new-line)


  (def-package! utop
    :when (featurep! :tools eval)
    :hook (tuareg-mode . +ocaml|init-utop)
    :init
    (set-repl-handler! 'tuareg-mode #'utop)
    (set-eval-handler! 'tuareg-mode #'utop-eval-region)
    (defun +ocaml|init-utop ()
      (when (executable-find "utop")
        (utop-minor-mode)))))


(def-package! merlin
  :unless (featurep! +lsp)
  :hook (tuareg-mode . +ocaml|init-merlin)
  :init
  (defun +ocaml|init-merlin ()
    "Activate `merlin-mode' if the ocamlmerlin executable exists."
    (when (executable-find "ocamlmerlin")
      (merlin-mode)))

  (after! tuareg
    (set-company-backend! 'tuareg-mode 'merlin-company-backend)
    (set-lookup-handlers! 'tuareg-mode
      :definition #'merlin-locate
      :references #'merlin-occurrences
      :documentation #'merlin-document))
  :config
  (setq merlin-completion-with-doc t)

  (map! :localleader
        :map tuareg-mode-map
        "t" #'merlin-type-enclosing
        "a" #'tuareg-find-alternate-file)

  (def-package! flycheck-ocaml
    :when (featurep! :tools flycheck)
    :hook (merlin-mode . +ocaml|init-flycheck)
    :config
    (defun +ocaml|init-flycheck ()
      "Activate `flycheck-ocaml` if the current project possesses a .merlin file."
      (when (projectile-locate-dominating-file default-directory ".merlin")
        ;; Disable Merlin's own error checking
        (setq merlin-error-after-save nil)
        ;; Enable Flycheck checker
        (flycheck-ocaml-setup))))

  (def-package! merlin-eldoc
    :hook (merlin-mode . merlin-eldoc-setup))

  (def-package! merlin-iedit
    :when (featurep! :editor multiple-cursors)
    :defer t
    :init
    (map! :map tuareg-mode-map
          :v "R" #'merlin-iedit-occurrences))

  (def-package! merlin-imenu
    :when (featurep! :emacs imenu)
    :hook (merlin-mode . merlin-use-merlin-imenu)))


(def-package! ocp-indent
  ;; must be careful to always defer this, it has autoloads that adds hooks
  ;; which we do not want if the executable can't be found
  :hook (tuareg-mode . +ocaml|init-ocp-indent)
  :config
  (defun +ocaml|init-ocp-indent ()
    "Run `ocp-setup-indent', so long as the ocp-indent binary exists."
    (when (executable-find "ocp-indent")
      (ocp-setup-indent))))


(def-package! ocamlformat
  :when (featurep! :editor format)
  :commands ocamlformat
  :hook (tuareg-mode . +ocaml|init-ocamlformat)
  :config
  (set-formatter! 'ocamlformat #'ocamlformat
    :modes '(caml-mode tuareg-mode))
  ;; TODO Fix region-based formatting support
  (defun +ocaml|init-ocamlformat ()
    (setq +format-with 'ocp-indent)
    (when (and (executable-find "ocamlformat")
               (locate-dominating-file default-directory ".ocamlformat"))
      (setq +format-with 'ocamlformat))))
