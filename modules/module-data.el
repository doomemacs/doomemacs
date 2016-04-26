;;; module-data.el

(associate! conf-mode :match "/\\.?editorconfig$")
(associate! nxml-mode :match "\\.plist$")

(use-package toml-mode :mode "\\.toml$")

(use-package yaml-mode :mode "\\.ya?ml$"
  :config (def-electric! yaml-mode :chars (?\n ?: ?-)))

(use-package json-mode :mode "\\.js\\(on\\|hintrc\\)$"
  :config (def-electric! json-mode :chars (?\n ?: ?{ ?})))

(use-package vimrc-mode :mode ("/\\.?g?vimrc$" "\\.vim$" "/\\.vim/rc/.+$"))

(use-package dockerfile-mode :mode "/Dockerfile$"
  :config
  (def-docset! dockerfile-mode "docker")
  (def-builder! dockerfile-mode dockerfile-build-buffer "Dockerfile"))

;;
(def-project-type! ansible-mode "ans"
  :modes (yaml-mode)
  :files ("roles/"))

(use-package company-ansible
  :commands (company-ansible)
  :init (def-company-backend! ansible-mode (ansible)))

(def-project-type! vagrant "vagrant"
  :files ("Vagrantfile"))

(provide 'module-data)
;;; module-data.el ends here
