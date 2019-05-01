;;; lang/web/+css.el -*- lexical-binding: t; -*-

;; An improved newline+continue comment function
(setq-hook! css-mode comment-indent-function #'+css/comment-indent-new-line)

(map! :map (css-mode-map scss-mode-map less-css-mode-map)
      :localleader
      "rb" #'+css/toggle-inline-or-block)

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

(add-hook! (css-mode sass-mode stylus-mode) #'rainbow-mode)

;; built-in, and contains both css-mode & scss-mode
(after! css-mode
  ;; css-mode hooks apply to scss and less-css modes
  (add-hook 'css-mode-hook #'rainbow-delimiters-mode)
  (set-company-backend! '(css-mode scss-mode)
    (if EMACS26+
        ;; css-mode's built in completion is superior in 26+
        'company-capf
      'company-css))
  (map! :map scss-mode-map :localleader "b" #'+css/scss-build))


(after! sass-mode
  (set-company-backend! 'sass-mode 'company-css)
  (map! :map sass-mode-map :localleader "b" #'+css/sass-build))


;;
;;; Tools

(when (featurep! +lsp)
  (add-hook! (css-mode sass-mode less-css-mode) #'lsp!))


(def-package! counsel-css
  :when (featurep! :completion ivy)
  :commands counsel-css
  :hook (css-mode . counsel-css-imenu-setup)
  :init
  (map! :map (css-mode-map scss-mode-map less-css-mode-map)
        :localleader ";" #'counsel-css))


(def-package! helm-css-scss
  :when (featurep! :completion helm)
  :defer t
  :init
  (map! :map (css-mode-map scss-mode-map less-css-mode-map)
        :localleader ";" #'helm-css-scss)
  :config
  (setq helm-css-scss-split-direction #'split-window-vertically
        helm-css-scss-split-with-multiple-windows t))
