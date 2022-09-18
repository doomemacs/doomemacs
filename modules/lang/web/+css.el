;;; lang/web/+css.el -*- lexical-binding: t; -*-

(defvar +web-continue-block-comments t
  "If non-nil, newlines in block comments are continued with a leading *.

This also indirectly means the asterisks in the opening /* and closing */ will
be aligned.

If set to `nil', disable all the above behaviors.")

(after! projectile
  (pushnew! projectile-other-file-alist
            '("css"  "scss" "sass" "less" "styl")
            '("scss" "css")
            '("sass" "css")
            '("less" "css")
            '("styl" "css")))


;;
;;; Major modes

(setq-hook! 'css-mode-hook
  ;; Correctly continue /* and // comments on newline-and-indent
  comment-line-break-function #'+css/comment-indent-new-line
  ;; Fix `fill-paragraph' not conjoining line comments in CSS modes correctly.
  adaptive-fill-function #'+css-adaptive-fill-fn
  ;; Fix filled lines not being auto-prefixed with a * when needed.
  adaptive-fill-first-line-regexp "\\'[ \t]*\\(?:\\* *\\)?\\'")

(after! (:any css-mode sass-mode)
  (set-docsets! '(css-mode scss-mode sass-mode)
    "CSS" "HTML" "Bourbon" "Compass"
    ["Sass" (memq major-mode '(scss-mode sass-mode))]))

(add-hook! '(css-mode-hook sass-mode-hook stylus-mode-hook)
           #'rainbow-mode)

;; built-in. Contains both css-mode & scss-mode
(after! css-mode
  ;; css-mode hooks apply to scss and less-css modes
  (map! :localleader
        :map scss-mode-map
        "b" #'+css/scss-build
        :map (css-mode-map scss-mode-map less-css-mode-map)
        "rb" #'+css/toggle-inline-or-block)

  (use-package! counsel-css
    :when (modulep! :completion ivy)
    :hook (css-mode . counsel-css-imenu-setup)
    :init
    (map! :map (css-mode-map scss-mode-map less-css-mode-map)
          :localleader ";" #'counsel-css))

  (use-package! helm-css-scss
    :when (modulep! :completion helm)
    :defer t
    :init
    (map! :map (css-mode-map scss-mode-map less-css-mode-map)
          :localleader ";" #'helm-css-scss)
    :config
    (setq helm-css-scss-split-direction #'split-window-vertically
          helm-css-scss-split-with-multiple-windows t)))


(after! sass-mode
  (set-company-backend! 'sass-mode 'company-css)
  (map! :map sass-mode-map :localleader "b" #'+css/sass-build))


;;
;;; Tools

(when (modulep! +lsp)
  (add-hook! '(css-mode-local-vars-hook
               scss-mode-local-vars-hook
               sass-mode-local-vars-hook
               less-css-mode-local-vars-hook)
             :append #'lsp!))

(when (modulep! +tree-sitter)
  (add-hook 'css-mode-local-vars-hook #'tree-sitter! 'append))
