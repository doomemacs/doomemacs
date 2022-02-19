;;; ui/emoji/config.el -*- lexical-binding: t; -*-

(use-package! emojify
  :hook (doom-first-buffer . global-emojify-mode)
  :config
  (setq emojify-styles
        (delq
         nil (list (if (featurep! +ascii) 'ascii)
                   (if (featurep! +github) 'github)
                   (if (featurep! +unicode) 'unicode))))

  (emojify-set-emoji-styles emojify-styles))
