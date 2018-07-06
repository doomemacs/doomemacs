;;; ui/pretty-code/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +fira)
       (load! "+fira"))
      ((featurep! +iosevka)
       (load! "+iosevka")))

;; When you get to the right edge, it goes back to how it normally prints
(setq prettify-symbols-unprettify-at-point 'right-edge)

(add-hook! 'after-change-major-mode-hook #'+pretty-code|init-pretty-symbols)
