;;; lang/data/config.el -*- lexical-binding: t; -*-

(push '("/sxhkdrc" . conf-mode) auto-mode-alist)


(def-package! nxml-mode
  :mode "\\.plist$"
  :config
  (set! :company-backend 'nxml-mode '(company-nxml company-yasnippet)))


(def-package! toml-mode :mode "\\.toml$")


(def-package! yaml-mode :mode "\\.ya?ml$")


(def-package! json-mode
  :mode "\\.js\\(on\\|[hl]int\\(rc\\)?\\)$"
  :config
  (set! :electric 'json-mode :chars '(?\n ?: ?{ ?})))


(def-package! vimrc-mode
  :mode "/\\.?g?vimrc$"
  :mode "\\.vim$"
  :mode "\\.?vimperatorrc$"
  :mode "\\.vimp$")


(def-package! dockerfile-mode
  :mode "/Dockerfile$"
  :config
  ;; TODO
  ;; (set! :build 'build-image 'dockerfile-mode '+data/dockerfile-build
  ;;   :when '+data-dockerfile-p)
  )


;; For ROM hacking or debugging
(def-package! hexl
  :mode ("\\.hex$" . hexl-mode)
  :mode ("\\.nes$" . hexl-mode))


;;
;; Frameworks
;;

(def-project-mode! +data-ansible-mode
  :modes (yaml-mode)
  :files "roles/")

(def-project-mode! +data-vagrant-mode
  :files "Vagrantfile")

