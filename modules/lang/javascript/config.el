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

  ;; Conform switch-case indentation to js2 normal indent
  (defvaralias 'js-switch-indent-offset 'js2-basic-offset)

  (sp-with-modes '(js2-mode rjsx-mode)
    (sp-local-pair "/* " " */" :post-handlers '(("| " "SPC"))))

  ;; If it's available globally, use eslint_d
  (setq flycheck-javascript-eslint-executable (executable-find "eslint_d"))

  (defun +javascript|init-flycheck-eslint ()
    "Favor local eslint over global installs and configure flycheck for eslint."
    (when (derived-mode-p 'js-mode)
      (when-let* ((exec-path (list (doom-project-expand "node_modules/.bin")))
                  (eslint (executable-find "eslint")))
        (setq-local flycheck-javascript-eslint-executable eslint))
      (when (flycheck-find-checker-executable 'javascript-eslint)
        ;; Flycheck has it's own trailing command and semicolon warning that was
        ;; conflicting with the eslint settings.
        (setq-local js2-strict-trailing-comma-warning nil)
        (setq-local js2-strict-missing-semi-warning nil))))
  (add-hook 'flycheck-mode-hook #'+javascript|init-flycheck-eslint)

  (map! :map js2-mode-map
        :localleader
        "r" #'+javascript/refactor-menu
        "S" #'+javascript/skewer-this-buffer))


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
   js2r-debug-this js2r-forward-slurp js2r-forward-barf)
  :init
  (def-menu! +javascript/refactor-menu
    "Refactoring commands for `js2-mode' buffers."
    '(("Extract into function"           :exec js2r-extract-function          :region t)
      ("Extract into method"             :exec js2r-extract-method            :region t)
      ("Introduce parameter to function" :exec js2r-introduce-parameter       :region t)
      ("Localize parameter"              :exec js2r-localize-parameter        :region nil)
      ("Expand object"                   :exec js2r-expand-object             :region nil)
      ("Expand function"                 :exec js2r-expand-function           :region nil)
      ("Expand array"                    :exec js2r-expand-array              :region nil)
      ("Contract object"                 :exec js2r-contract-object           :region nil)
      ("Contract function"               :exec js2r-contract-function         :region nil)
      ("Contract array"                  :exec js2r-contract-array            :region nil)
      ("Wrap buffer in IIFE"             :exec js2r-wrap-buffer-in-iife       :region nil)
      ("Inject global into IIFE"         :exec js2r-inject-global-in-iife     :region t)
      ("Add to globals annotation"       :exec js2r-add-to-globals-annotation :region nil)
      ("Extract variable"                :exec js2r-extract-var               :region t)
      ("Inline variable"                 :exec js2r-inline-var                :region t)
      ("Rename variable"                 :exec js2r-rename-var                :region nil)
      ("Replace var with this"           :exec js2r-var-to-this               :region nil)
      ("Arguments to object"             :exec js2r-arguments-to-object       :region nil)
      ("Ternary to if"                   :exec js2r-ternary-to-if             :region nil)
      ("Split var declaration"           :exec js2r-split-var-declaration     :region nil)
      ("Split string"                    :exec js2r-split-string              :region nil)
      ("Unwrap"                          :exec js2r-unwrap                    :region t)
      ("Log this"                        :exec js2r-log-this)
      ("Debug this"                      :exec js2r-debug-this)
      ("Reformat buffer (eslint_d)"      :exec eslintd-fix :region nil :when (fboundp 'eslintd-fix)))
    :prompt "Refactor: "))


(def-package! tern
  :hook (js2-mode . tern-mode)
  :config
  (advice-add #'tern-project-dir :override #'doom-project-root))


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
  (defun +javascript-jsx-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "js")
         (re-search-forward "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
                            magic-mode-regexp-match-limit t)
         (progn (goto-char (match-beginning 1))
                (not (sp-point-in-string-or-comment)))))

  (push (cons #'+javascript-jsx-file-p 'rjsx-mode) magic-mode-alist)

  :config
  (set! :electric 'rjsx-mode :chars '(?\} ?\) ?. ?>))

  ;; disable electric keys (I use snippets and `emmet-mode' instead)
  (map! :map rjsx-mode-map
        "<" nil
        "C-d" nil)
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


(def-package! eslintd-fix
  :commands (eslintd-fix-mode eslintd-fix))


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
  :match "/screeps\\(-ai\\)?/.+$"
  :modes (+javascript-npm-mode)
  :add-hooks (+javascript|init-screeps-mode)
  :on-load (load! +screeps))

(def-project-mode! +javascript-gulp-mode
  :files "gulpfile.js")

(def-project-mode! +javascript-npm-mode
  :modes (html-mode css-mode web-mode js2-mode markdown-mode)
  :files "package.json"
  :on-enter
  (when (make-local-variable 'exec-path)
    (push (doom-project-expand "node_modules/.bin")
          exec-path)))


;;
;; Tools
;;

(def-project-mode! +javascript-eslintd-fix-mode
  :add-hooks (eslintd-fix-mode))

