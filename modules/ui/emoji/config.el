;;; ui/emoji/config.el -*- lexical-binding: t; -*-

(use-package! emojify
  :hook (doom-first-buffer . global-emojify-mode)
  :config
  (setq emojify-styles
        (delq
         nil (list (if (modulep! +ascii) 'ascii)
                   (if (modulep! +github) 'github)
                   (if (modulep! +unicode) 'unicode))))

  (emojify-set-emoji-styles emojify-styles))
