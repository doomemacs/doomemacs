;;; lang/data/config.el -*- lexical-binding: t; -*-

;; Built in plugins
(add-to-list 'auto-mode-alist '("/sxhkdrc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode))

(use-package! nxml-mode
  :mode "\\.p\\(?:list\\|om\\)\\'" ; plist, pom
  :mode "\\.xs\\(?:d\\|lt\\)\\'"   ; xslt, xsd
  :mode "\\.rss\\'"
  :magic "<\\?xml"
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t)
  (set-company-backend! 'nxml-mode '(company-nxml company-yasnippet)))


;;
;;; Third-party plugins

;;;###package csv-mode
(map! :after csv-mode
      :localleader
      :map csv-mode-map
      "a" #'csv-align-fields
      "u" #'csv-unalign-fields
      "s" #'csv-sort-fields
      "S" #'csv-sort-numeric-fields
      "k" #'csv-kill-fields
      "t" #'csv-transpose)

(use-package! graphql-mode
  :mode "\\.gql\\'")

(use-package! json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :config
  (set-electric! 'json-mode :chars '(?\n ?: ?{ ?})))

(after! jsonnet-mode
  (set-electric! 'jsonnet-mode :chars '(?\n ?: ?{ ?})))


;;
;;; Frameworks

(def-project-mode! +data-vagrant-mode
  :files ("Vagrantfile"))
