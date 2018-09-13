;;; lang/data/config.el -*- lexical-binding: t; -*-

;; Built in plugins
(add-to-list 'auto-mode-alist '("/sxhkdrc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode))

(after! nxml-mode
  (set-company-backend! 'nxml-mode '(company-nxml company-yasnippet)))


;;
;; Third-party plugins

;; `csv-mode'
(map! :after csv-mode
      :map csv-mode-map
      :localleader
      :nvm "a" #'csv-align-fields
      :nvm "u" #'csv-unalign-fields
      :nvm "s" #'csv-sort-fields
      :nvm "S" #'csv-sort-numeric-fields
      :nvm "k" #'csv-kill-fields
      :nvm "t" #'csv-transpose)

(def-package! graphql-mode
  :mode "\\.gql\\'")

(def-package! json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(rc\\)?\\)\\'"
  :config
  (set-electric! 'json-mode :chars '(?\n ?: ?{ ?})))

(def-package! vimrc-mode
  :mode "\\.?vimperatorrc\\'")


;;
;; Frameworks

(def-project-mode! +data-vagrant-mode
  :files ("Vagrantfile"))

