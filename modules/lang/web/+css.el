;;; lang/web/+css.el -*- lexical-binding: t; -*-

;; An improved newline+continue comment function
(setq-hook! css-mode
  comment-indent-function #'+css/comment-indent-new-line)

(after! (:any css-mode sass-mode)
  (set-docsets! '(css-mode scss-mode sass-mode)
    "CSS" "HTML" "Bourbon" "Compass"
    ["Sass" (memq major-mode '(scss-mode sass-mode))]))

(after! projectile
  (pushnew! projectile-other-file-alist
            '("css"  "scss" "sass" "less" "styl")
            '("scss" "css")
            '("sass" "css")
            '("less" "css")
            '("styl" "css")))


;;
;;; Major modes

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
    :when (featurep! :completion ivy)
    :hook (css-mode . counsel-css-imenu-setup)
    :init
    (map! :map (css-mode-map scss-mode-map less-css-mode-map)
          :localleader ";" #'counsel-css))

  (use-package! helm-css-scss
    :when (featurep! :completion helm)
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

(when (featurep! +lsp)
  (add-hook! '(css-mode-local-vars-hook
               sass-mode-local-vars-hook
               less-css-mode-local-vars-hook)
             #'lsp!))
