;;; tools/pdf/config.el -*- lexical-binding: t; -*-

(def-package! pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :init (load "pdf-tools-autoloads" nil t)
  :config
  (unless noninteractive
    (pdf-tools-install))

  (map! :map pdf-view-mode-map
        "q" #'kill-this-buffer
        doom-leader-key nil)

  (setq-default pdf-view-display-size 'fit-page)
  ;; Turn off cua so copy works
  (add-hook! 'pdf-view-mode-hook (cua-mode 0))
  ;; Custom modeline that removes useless info and adds page numbers
  (when (featurep! :ui doom-modeline)
    (load! +modeline)
    (add-hook! pdf-tools-enabled (doom-set-modeline 'pdf-tools-modeline)))
  ;; Handle PDF-tools related popups better
  (set! :popup "^\\*Outline*" '((side . right) (size . 40)) '((select)))
  ;; TODO: Add additional important windows that should be handled differently
  ;; TODO: These two next rules don't work (they should), investigate
  ;; (set! :popup "\\*Contents\\*" '((side . right) (size . 40)) nil)
  ;; (set! :popup "* annots\\*$" '((side . left) (size . 40)) '((select)))
  )
