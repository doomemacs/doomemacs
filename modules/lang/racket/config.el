;;; lang/racket/config.el -*- lexical-binding: t; -*-

;; DEPRECATED: Remove when projectile is replaced with project.el
(after! projectile
  (add-to-list 'projectile-project-root-files "info.rkt"))


;;
;;; Packages

(let (mode
      mode-map
      mode-exts
      mode-hook
      mode-local-vars-hook)
  (if (modulep! +hash-lang)
      (setq mode 'racket-hash-lang-mode
            mode-exts '("\\.rkt\\'"    ; Racket
                        "\\.scrbl\\'"  ; Scribble
                        "\\.rhm\\'"))  ; Rhombus
    (setq mode 'racket-mode
          mode-exts `("\\.rkt\\'")))
  (let ((mode-name (symbol-name mode)))
    (setq mode-map (intern (format "%s-map" mode-name))
          mode-hook (intern (format "%s-hook" mode-name))
          mode-local-vars-hook (intern (format "%s-local-vars-hook" mode-name))))

  (use-package! racket-mode
    :defer t
    :init
    (dolist (entry mode-exts)
      (add-to-list 'auto-mode-alist (cons entry mode)))
    :config
    (set-repl-handler! mode #'+racket/open-repl)
    (set-lookup-handlers! `(,mode racket-repl-mode)
      :definition    #'+racket-lookup-definition
      :documentation #'+racket-lookup-documentation)
    (set-docsets! mode "Racket")
    (set-ligatures! mode
      :lambda  "lambda"
      :map     "map"
      :dot     ".")
    (set-rotate-patterns! mode :symbols '(("#true" "#false")))
    (set-formatter! 'raco-fmt '("raco" "fmt") :modes (list mode))

    (add-hook mode-hook #'rainbow-delimiters-mode)
    (add-hook mode-hook #'highlight-quoted-mode)

    (when (modulep! +lsp)
      (add-hook mode-local-vars-hook #'lsp! 'append))

    (when (modulep! +xp)
      (add-hook mode-local-vars-hook #'racket-xp-mode)
      ;; Both flycheck and racket-xp produce error popups, but racket-xp's are
      ;; higher quality so disable flycheck's:
      (when (modulep! :checkers syntax)
        (add-hook! 'racket-xp-mode-hook
          (defun +racket-disable-flycheck-h ()
            (cl-pushnew 'racket flycheck-disabled-checkers)))))

    (unless (or (modulep! :editor parinfer)
                (modulep! :editor lispy))
      (add-hook mode-hook #'racket-smart-open-bracket-mode))

    (map! (:map racket-xp-mode-map
           [remap racket-doc]              #'racket-xp-documentation
           [remap racket-visit-definition] #'racket-xp-visit-definition
           [remap next-error]              #'racket-xp-next-error
           [remap previous-error]          #'racket-xp-previous-error)
          (:localleader
           :map ,mode-map
           "a" #'racket-align
           "A" #'racket-unalign
           "f" #'racket-fold-all-tests
           "F" #'racket-unfold-all-tests
           "h" #'racket-doc
           "i" #'racket-unicode-input-method-enable
           "l" #'racket-logger
           "o" #'racket-profile
           "p" #'racket-cycle-paren-shapes
           "r" #'racket-run
           "R" #'racket-run-and-switch-to-repl
           "t" #'racket-test
           "u" #'racket-backward-up-list
           "y" #'racket-insert-lambda
           (:prefix ("m" . "macros")
            "d" #'racket-expand-definition
            "e" #'racket-expand-last-sexp
            "r" #'racket-expand-region
            "a" #'racket-expand-again)
           (:prefix ("g" . "goto")
            "b" #'racket-unvisit
            "d" #'racket-visit-definition
            "m" #'racket-visit-module
            "r" #'racket-open-require-path)
           (:prefix ("s" . "send")
            "d" #'racket-send-definition
            "e" #'racket-send-last-sexp
            "r" #'racket-send-region)
           :map racket-repl-mode-map
           "l" #'racket-logger
           "h" #'racket-repl-documentation
           "y" #'racket-insert-lambda
           "u" #'racket-backward-up-list
           (:prefix ("m" . "macros")
            "d" #'racket-expand-definition
            "e" #'racket-expand-last-sexp
            "f" #'racket-expand-file
            "r" #'racket-expand-region)
           (:prefix ("g" . "goto")
            "b" #'racket-unvisit
            "m" #'racket-visit-module
            "d" #'racket-repl-visit-definition)))))
