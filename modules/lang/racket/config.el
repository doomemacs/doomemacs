;;; lang/racket/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "info.rkt"))


;;
;;; Packages

(use-package! racket-mode
  :mode "\\.rkt\\'"  ; give it precedence over :lang scheme
  :hook (racket-repl-mode . racket-unicode-input-method-enable)
  :config
  (set-repl-handler! 'racket-mode #'+racket/open-repl)
  (set-lookup-handlers! 'racket-mode
    :definition    #'+racket-lookup-definition
    :documentation #'+racket-lookup-documentation)
  (set-docsets! 'racket-mode "Racket")
  (set-pretty-symbols! 'racket-mode
    :lambda  "lambda"
    :map     "map"
    :dot     ".")
  (set-rotate-patterns! 'racket-mode
    :symbols '(("#true" "#false")))

  (add-hook! 'racket-mode-hook
             #'rainbow-delimiters-mode
             #'highlight-quoted-mode)

  (when (featurep! +xp)
    (add-hook 'racket-mode-local-vars-hook #'racket-xp-mode)
    ;; Both flycheck and racket-xp produce error popups, but racket-xp's are
    ;; higher quality so disable flycheck's:
    (when (featurep! :checkers syntax)
      (add-hook! 'racket-xp-mode-hook
        (defun +racket-disable-flycheck-h ()
          (cl-pushnew 'racket flycheck-disabled-checkers)))))

  (unless (or (featurep! :editor parinfer)
              (featurep! :editor lispy))
    (add-hook 'racket-mode-hook #'racket-smart-open-bracket-mode))

  (map! (:map racket-xp-mode-map
         [remap next-error]     #'racket-xp-next-error
         [remap previous-error] #'racket-xp-previous-error)
        (:localleader
         :map racket-mode-map
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
          "r" #'racket-send-region))))
