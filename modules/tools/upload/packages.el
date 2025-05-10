;; -*- no-byte-compile: t; -*-
;;; tools/upload/packages.el

(package! ssh-deploy
  :recipe (:host github :repo "emacsmirror/ssh-deploy")
  :pin "dc8882d1806c0fdd635bc625b109179dfa3c929c")
