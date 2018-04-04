;;; init.test.el -- for automated unit tests -*- lexical-binding: t; -*-

(setq doom-private-dir nil)
(require 'core (concat user-emacs-directory "core/core"))

(doom! :feature
       evil
       workspaces

       :completion
       company

       :ui
       doom-dashboard

       :tools
       password-store

       :lang
       org
       web)
