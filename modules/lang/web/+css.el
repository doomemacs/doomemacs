;;; lang/web/+css.el -*- lexical-binding: t; -*-

;; An improved newline+continue comment function
(setq-hook! css-mode comment-indent-function #'+css/comment-indent-new-line)

(map! :map* (css-mode-map scss-mode-map less-css-mode-map)
      :localleader
      :n "rb" #'+css/toggle-inline-or-block)

(after! (:any css-mode sass-mode)
  (set-docsets! '(css-mode scss-mode sass-mode)
    "CSS" "HTML" "Bourbon" "Compass"
    ["Sass" (memq major-mode '(scss-mode sass-mode))]))


;;
;; Major modes

(add-hook! (css-mode sass-mode stylus-mode) #'rainbow-mode)

(after! css-mode  ; built-in -- contains both css-mode & scss-mode
  ;; css-mode hooks apply to scss and less-css modes
  (add-hook 'css-mode-hook #'rainbow-delimiters-mode)
  (unless EMACS26+
    ;; css-mode's built in completion is superior in 26+
    (set-company-backend! '(css-mode scss-mode) 'company-css))
  (map! :map scss-mode-map :localleader :n "b" #'+css/scss-build))


(after! sass-mode
  (set-company-backend! 'sass-mode 'company-css)
  (map! :map sass-mode-map :localleader :n "b" #'+css/sass-build))


;;
;; Tools

(def-package! counsel-css
  :when (featurep! :completion ivy)
  :commands counsel-css
  :hook (css-mode . counsel-css-imenu-setup)
  :init
  (map! :map* (css-mode-map scss-mode-map less-css-mode-map)
        :localleader :n ";" #'counsel-css))


<<<<<<< HEAD
(def-package! helm-css-scss
  :when (featurep! :completion helm)
  :defer t
  :config
  :defer t
  :init 
  (map! :map* (css-mode-map scss-mode-map less-css-mode-map)
	:localleader :n ";" #'helm-css-scss)
  :config
  (setq helm-css-scss-split-direction #'split-window-vertically
	helm-css-scss-split-with-multiple-windows t))
  
;; LSP-mode
(def-package! lsp-css
  :when (featurep! +lsp)
  :after-call (css-mode sass-mode scss-mode-hook less-css-mode)
  :hook ((css-mode . +lsp-css|css-mode)
         (sass-mode . +lsp-css|scss-mode)
         (scss-mode . +lsp-css|scss-mode)
         (less-css-mode . +lsp-css|less-mode))
  :config
  (set-company-backend! '(css-mode sass-mode scss-mode less-css-mode) '(company-css company-lsp)))
