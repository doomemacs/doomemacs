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
      :localleader
      :map csv-mode-map
      "a" #'csv-align-fields
      "u" #'csv-unalign-fields
      "s" #'csv-sort-fields
      "S" #'csv-sort-numeric-fields
      "k" #'csv-kill-fields
      "t" #'csv-transpose)

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

