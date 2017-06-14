;;; init.test.el -- for automated unit tests -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(doom! :feature evil
       :tools password-store
       :private hlissner)
