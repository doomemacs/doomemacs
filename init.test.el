;;; init.test.el  -*- lexical-binding: t; -*-

;; An init.el for our unit test suites. Do not use this!

(doom! :completion
       company
       :ui
       doom-dashboard
       popup
       workspaces
       :editor
       evil
       :tools
       pass
       :lang
       org
       web)
