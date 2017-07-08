;;; init.test.el -- for automated unit tests -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(doom! :feature
       evil
       workspaces

       :completion
       company

       :tools
       password-store

       :lang
       web

       :private
       hlissner)
