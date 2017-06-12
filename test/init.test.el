;;; init.el

(require 'core (concat user-emacs-directory "core/core"))

(doom! :feature evil
       :tools password-store
       :private hlissner)
