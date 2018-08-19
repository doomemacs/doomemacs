;;; emacs/imenu/config.el -*- lexical-binding: t; -*-

;; `imenu-anywhere'
(setq imenu-anywhere-delimiter ": ")


(after! imenu-list
  (setq imenu-list-idle-update-delay 0.5)

  (set-popup-rule! "^\\*Ilist"
    :side 'right :size 35 :quit nil :select nil :ttl 0))
