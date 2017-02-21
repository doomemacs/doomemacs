;;; module-data.el

(@associate conf-mode :match "/sxhkdrc$")


(@def-package nxml-mode
  :mode "\\.plist$"
  :config
  (@set :company-backend 'nxml-mode '(company-nxml company-yasnippet)))


(@def-package toml-mode :mode "\\.toml$")


(@def-package yaml-mode
  :mode "\\.ya?ml$"
  :config
  (@set :electric 'yaml-mode :chars '(?\n ?\: ?\-)))


(@def-package json-mode :mode "\\.js\\(on\\|[hl]int\\(rc\\)?\\)$"
  :config
  (@set :electric 'json-mode :chars '(?\n ?: ?{ ?})))


(@def-package vimrc-mode
  :mode ("/\\.?g?vimrc$" "\\.vim$" "/\\.?vimperatorrc$" "\\.vimp$"))


(@def-package dockerfile-mode
  :mode "/Dockerfile$"
  :config
  ;; TODO
  (@set :build 'docker 'dockerfile-mode '+data-dockerfile-p '+data/dockerfile-build))


;; For ROM hacking or debugging
(@def-package hexl-mode
  :mode ("\\.hex$" "\\.nes$"))


;;
;; Frameworks
;;

;; (@def-project ansible-mode "ans"
;;   :modes (yaml-mode)
;;   :files ("roles/"))

;; (@def-project vagrant "vagrant"
;;   :files ("Vagrantfile"))

