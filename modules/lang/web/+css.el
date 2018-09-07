;;; lang/web/+css.el -*- lexical-binding: t; -*-

(defvar +web-css-docsets '("CSS" "HTML" "Bourbon" "Compass")
  "TODO")

;; An improved newline+continue comment function
(setq-hook! css-mode comment-indent-function #'+css/comment-indent-new-line)

(map! :map* (css-mode-map scss-mode-map less-css-mode-map)
      :localleader
      :n "rb" #'+css/toggle-inline-or-block)


;;
;; Packages
;;

;; css-mode hooks apply to scss and less-css modes
(add-hook 'css-mode-hook #'rainbow-delimiters-mode)
(add-hook! (css-mode sass-mode stylus-mode)
  #'(yas-minor-mode-on
     rainbow-mode))


(def-package! counsel-css
  :when (featurep! :completion ivy)
  :commands counsel-css
  :hook (css-mode . counsel-css-imenu-setup)
  :init
  (map! :map* (css-mode-map scss-mode-map less-css-mode-map)
        :localleader :n ";" #'counsel-css))


(def-package! css-mode ; built-in
  :defer t
  :config
  ;; contains both css-mode & scss-mode
  (set-docsets! 'css-mode  +web-css-docsets)
  (set-docsets! 'scss-mode "Sass" +web-css-docsets)
  (unless EMACS26+
    ;; css-mode's built in completion is superior in 26+
    (set-company-backend! '(css-mode scss-mode) 'company-css))
  (map! :map scss-mode-map :localleader :n "b" #'+css/scss-build))


(def-package! sass-mode
  :defer t
  :config
  (set-docsets! 'sass-mode "Sass" +web-css-docsets)
  (set-company-backend! 'sass-mode 'company-css)
  (map! :map scss-mode-map :localleader :n "b" #'+css/sass-build))

