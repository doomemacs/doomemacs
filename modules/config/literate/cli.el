;;; config/literate/cli.el -*- lexical-binding: t; -*-

(load! "autoload")

;; Tangle the user's config.org before 'doom sync' runs
(add-hook 'doom-before-sync-hook #'+literate-tangle-h)
