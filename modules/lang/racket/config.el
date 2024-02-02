;;; lang/racket/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "info.rkt"))

(defmacro racket-use-package (major-mode-var mode-map mode mode-hook local-vars-hook &rest body)
  `(use-package! racket-mode
     :mode ,mode
     :config
     (set-repl-handler! ',major-mode-var #'+racket/open-repl)
     (set-lookup-handlers! '(,major-mode-var racket-repl-mode)
       :definition    #'+racket-lookup-definition
       :documentation #'+racket-lookup-documentation)
     (set-docsets! ',major-mode-var "Racket")
     (set-ligatures! ',major-mode-var
                     :lambda  "lambda"
                     :map     "map"
                     :dot     ".")
     (set-rotate-patterns! ',major-mode-var
                           :symbols '(("#true" "#false")))
     (set-formatter! 'raco-fmt '("raco" "fmt") :modes '(,major-mode-var))

     (add-hook! ',mode-hook
                #'rainbow-delimiters-mode
                #'highlight-quoted-mode)

     (when (modulep! +lsp)
       (add-hook ',local-vars-hook #'lsp! 'append))

     (when (modulep! +xp)
       (add-hook ',local-vars-hook #'racket-xp-mode)
       ;; Both flycheck and racket-xp produce error popups, but racket-xp's have
       ;; higher quality so disable flycheck's:
       (when (modulep! :checkers syntax)
         (add-hook! 'racket-xp-mode-hook
           (defun +racket-disable-flycheck-h ()
             (setq-local flycheck-standard-error-navigation nil)
             (cl-pushnew 'racket flycheck-disabled-checkers)))))

     (map! (:map racket-xp-mode-map
                 [remap racket-doc] #'racket-xp-documentation)
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
                     "b" #'xref-pop-marker-stack
                     "d" #'xref-find-definitions
                     "m" #'xref-find-definitions
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
                     "b" #'xref-pop-marker-stack
                     "m" #'xref-find-definitions
                     "d" #'xref-find-definitions)))

     ,@body))

;;
;;; Packages

(if (modulep! +hash-lang)
    (racket-use-package
     ;; mode
     racket-hash-lang-mode
     ;; mode-map
     racket-hash-lang-mode-map
     ;; mode
     (("\\.rkt\\'" . racket-hash-lang-mode) ; Racket
      ("\\.scrbl\\'" . racket-hash-lang-mode) ; Scribble
      ("\\.rhm\\'" . racket-hash-lang-mode)) ; Rhombus
     ;; mode-hook
     racket-hash-lang-mode-hook
     ;; local-vars-hook
     racket-hash-lang-mode-local-vars-hook)
  (racket-use-package
   ;; mode
   racket-mode
   ;; mode-map
   racket-mode-map
   ;; mode
   "\\.rkt\\'"
   ;; mode-hook
   racket-mode-hook
   ;; local-vars-hook
   racket-mode-local-vars-hook
   ;; body
   (unless (or (modulep! :editor parinfer)
               (modulep! :editor lispy))
     (add-hook 'racket-mode-hook #'racket-smart-open-bracket-mode))))
