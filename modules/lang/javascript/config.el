;;; lang/javascript/config.el -*- lexical-binding: t; -*-

(def-package! js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :config
  (setq js2-skip-preprocessor-directives t
        js2-highlight-external-variables nil
        js2-mode-show-parse-errors nil)

  (add-hook! 'js2-mode-hook
    #'(flycheck-mode highlight-indentation-mode rainbow-delimiters-mode))

  (set! :repl 'js2-mode #'+javascript/repl)
  (set! :electric 'js2-mode :chars '(?\} ?\) ?.))
  (set! :jump 'js2-mode :xref-backend #'xref-js2-xref-backend)

  ;; Conform switch-case indentation to editorconfig's config
  (set! :editorconfig :add '(js2-mode js2-basic-offset js-switch-indent-offset))

  ;; Favor local eslint over global, if available
  (defun +javascript|init-flycheck-elint ()
    (when (derived-mode-p 'js-mode)
      (when-let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js"
                                           (doom-project-root)))
                 (exists-p (file-exists-p eslint))
                 (executable-p (file-executable-p eslint)))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'+javascript|init-flycheck-elint)

  (sp-with-modes '(js2-mode rjsx-mode)
    (sp-local-pair "/* " " */" :post-handlers '(("| " "SPC"))))

  (map! :map js2-mode-map
        :localleader
        :n  "S"  #'+javascript/skewer-this-buffer

        :prefix "r"
        :n  "g"  #'js2r-add-to-globals-annotation
        :n  "ca" #'js2r-arguments-to-object
        :n  "Xa" #'js2r-contract-array
        :n  "Xf" #'js2r-contract-function
        :n  "Xo" #'js2r-contract-object
        :nv "d"  #'js2r-debug-this
        :n  "xa" #'js2r-expand-array
        :n  "xf" #'js2r-expand-function
        :n  "xo" #'js2r-expand-object
        :v  "ef" #'js2r-extract-function
        :v  "em" #'js2r-extract-method
        :v  "ev" #'js2r-extract-var
        :n  "F"  #'js2r-forward-barf
        :n  "f"  #'js2r-forward-slurp
        :v  "ii" #'js2r-inject-global-in-iife
        :v  "iv" #'js2r-inline-var
        :v  "p"  #'js2r-introduce-parameter
        :n  "p"  #'js2r-localize-parameter
        :nv "l"  #'js2r-log-this
        :n  "r"  #'js2r-rename-var
        :n  "ss" #'js2r-split-string
        :n  "sv" #'js2r-split-var-declaration
        :n  "ct" #'js2r-ternary-to-if
        :v  "u"  #'js2r-unwrap
        :n  "t"  #'js2r-var-to-this
        :n  "ii" #'js2r-wrap-buffer-in-iife))


;; A find-{definition,references} backend for js2-mode. NOTE The xref API is
;; unstable and may break with an Emacs update.
(def-package! xref-js2 :commands xref-js2-xref-backend)


(def-package! nodejs-repl :commands nodejs-repl)


(def-package! js2-refactor
  :commands
  (js2r-extract-function js2r-extract-method js2r-introduce-parameter
   js2r-localize-parameter js2r-expand-object js2r-contract-object
   js2r-expand-function js2r-contract-function js2r-expand-array
   js2r-contract-array js2r-wrap-buffer-in-iife js2r-inject-global-in-iife
   js2r-add-to-globals-annotation js2r-extract-var js2r-inline-var
   js2r-rename-var js2r-var-to-this js2r-arguments-to-object js2r-ternary-to-if
   js2r-split-var-declaration js2r-split-string js2r-unwrap js2r-log-this
   js2r-debug-this js2r-forward-slurp js2r-forward-barf))


(def-package! tern
  :commands tern-mode
  :init (add-hook 'js2-mode-hook #'tern-mode)
  :config
  (advice-add #'tern-project-dir :override #'doom*project-root))


(def-package! company-tern
  :when (featurep! :completion company)
  :after tern
  :config
  (set! :company-backend 'js2-mode '(company-tern)))


(def-package! rjsx-mode
  :commands rjsx-mode
  :mode "\\.jsx$"
  :mode "components/.+\\.js$"
  :init
  ;; auto-detect JSX file
  (push (cons (lambda ()
                (and buffer-file-name
                     (equal (file-name-extension buffer-file-name) "js")
                     (re-search-forward "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
                                        magic-mode-regexp-match-limit t)
                     (progn
                       (goto-char (match-beginning 1))
                       (not (sp-point-in-string-or-comment)))))
              'rjsx-mode)
        magic-mode-alist)

  :config
  (set! :electric 'rjsx-mode :chars '(?\} ?\) ?. ?>))

  ;; disable electric keys (I use snippets and `emmet-mode' instead)
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (add-hook! rjsx-mode
    ;; jshint doesn't really know how to deal with jsx
    (push 'javascript-jshint flycheck-disabled-checkers)))


(def-package! coffee-mode
  :mode "\\.coffee$"
  :init (setq coffee-indent-like-python-mode t))


(def-package! web-beautify
  :commands web-beautify-js
  :init
  (map! :map* (json-mode js2-mode-map) :n "gQ" #'web-beautify-js))


;;
;; Skewer-mode
;;

(def-package! skewer-mode
  :commands (skewer-mode run-skewer)
  :config
  (map! :map skewer-mode-map
        :localleader
        :n "sE" #'skewer-eval-last-expression
        :n "se" #'skewer-eval-defun
        :n "sf" #'skewer-load-buffer))

(def-package! skewer-css ; in skewer-mode
  :commands skewer-css-mode
  :config
  (map! :map skewer-css-mode-map
        :localleader
        :n "se" #'skewer-css-eval-current-declaration
        :n "sr" #'skewer-css-eval-current-rule
        :n "sb" #'skewer-css-eval-buffer
        :n "sc" #'skewer-css-clear-all))

(def-package! skewer-html ; in skewer-mode
  :commands skewer-html-mode
  :config
  (map! :map skewer-html-mode-map
        :localleader
        :n "se" #'skewer-html-eval-tag))


;;
;; Projects
;;

(def-project-mode! +javascript-screeps-mode
  :match "/screeps/.+$"
  :modes (+javascript-npm-mode)
  :init (load! +screeps))

(def-project-mode! +javascript-gulp-mode
  :files "gulpfile.js")

(def-project-mode! +javascript-npm-mode
  :modes (html-mode css-mode web-mode js2-mode markdown-mode)
  :files "package.json")

(def-project-mode! +javascript-lb6-mode
  :modes (web-mode js2-mode nxml-mode markdown-mode)
  :match "\\.lb\\(action\\|ext\\)/"
  :init
  ;; TODO
  ;; (when IS-MAC
  ;;   (set! :build 'launchbar-action '+javascript-lb6-mode
  ;;     (lambda ()
  ;;       (when-let (dir (f-traverse-upwards (lambda (f) (f-ext? f "lbaction"))))
  ;;         (shell-command (format "open '%s'" dir))))
  ;;     :when
  ;;     (lambda () (f-traverse-upwards (lambda (f) (f-ext? f "lbaction"))))))
  )

