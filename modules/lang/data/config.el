;;; lang/data/config.el -*- lexical-binding: t; -*-

;; Built in plugins
(dolist (spec '(("/sxhkdrc\\'" . conf-mode)
                ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)
                ("\\.plist\\'" . nxml-mode)))
  (map-put auto-mode-alist (car spec) (cdr spec)))

(set! :company-backend 'nxml-mode '(company-nxml company-yasnippet))


;;
;; Third-party plugins
;;

;; `csv-mode'
(map! :after csv-mode
      :map csv-mode-map
      (:localleader
        :desc "Align fields" :nvm "a" #'csv-align-fields
        :desc "Unalign fields" :nvm "u" #'csv-unalign-fields
        :desc "Sort fields" :nvm "s" #'csv-sort-fields
        :desc "Sort fields (n)" :nvm "S" #'csv-sort-numeric-fields
        :desc "Kill fields" :nvm "k" #'csv-kill-fields
        :desc "Transpose fields" :nvm "t" #'csv-transpose))

(def-package! graphql-mode
  :mode "\\.gql\\'")

(def-package! json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(rc\\)?\\)\\'"
  :config
  (when (featurep! :feature syntax-checker)
    (add-hook 'json-mode-hook #'flycheck-mode))
  (set! :electric 'json-mode :chars '(?\n ?: ?{ ?})))

(def-package! vimrc-mode
  :mode "\\.?vimperatorrc\\'")


;;
;; Frameworks
;;

(def-project-mode! +data-ansible-mode
  :modes (yaml-mode)
  :files "roles/")

(def-project-mode! +data-vagrant-mode
  :files "Vagrantfile")

