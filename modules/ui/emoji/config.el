;;; ui/emoji/config.el -*- lexical-binding: t; -*-

(use-package! emojify
  :hook (doom-first-buffer . global-emojify-mode)
  :config
  (setq emojify-styles
        (delq
         nil (list (if (featurep! +ascii) 'ascii)
                   (if (featurep! +github) 'github)
                   (if (featurep! +unicode) 'unicode))))

  ;; No rendering emoji in verbatim tags.
  (setq-hook! 'org-mode-hook
    emojify-inhibit-functions (cons #'org-in-verbatim-emphasis emojify-inhibit-functions))

  (emojify-set-emoji-styles emojify-styles))
