;;; tools/docker/config.el -*- lexical-binding: t; -*-

(def-package! docker
  :config
  (set-evil-initial-state! 'docker-container-mode 'emacs)
  (set-evil-initial-state! 'docker-image-mode 'emacs)
  (set-evil-initial-state! 'docker-network-mode 'emacs)
  (set-evil-initial-state! 'docker-volume-mode 'emacs)
  (set-evil-initial-state! 'docker-machine-mode 'emacs))

(def-package! docker-tramp)
(def-package! dockerfile-mode)
