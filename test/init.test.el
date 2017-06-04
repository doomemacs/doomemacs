;;; init.el

(require 'core (concat user-emacs-directory "core/core"))

(setq-default debug-on-error nil)

(doom! :feature evil
       :private hlissner)
