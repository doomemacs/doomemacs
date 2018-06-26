;;; lang/data/config.el -*- lexical-binding: t; -*-

;; Built in plugins
(unless after-init-time
  (push '("/sxhkdrc\\'" . conf-mode) auto-mode-alist)
  (push '("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode) auto-mode-alist)
  (push '("\\.plist\\'" . nxml-mode) auto-mode-alist))

(after! nxml-mode
  (set-company-backend! 'nxml-mode '(company-nxml company-yasnippet)))


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
  (set-electric! 'json-mode :chars '(?\n ?: ?{ ?})))

(def-package! vimrc-mode
  :mode "\\.?vimperatorrc\\'")


;;
;; Frameworks
;;

(def-project-mode! +data-ansible-mode
  :modes (yaml-mode)
  :files ("roles/"))

(def-project-mode! +data-vagrant-mode
  :files ("Vagrantfile"))

