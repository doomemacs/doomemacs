;;; init.el

(require 'core (concat user-emacs-directory "core/core"))
(setq debug-on-error nil)

(doom! :feature evil
       :private hlissner)
