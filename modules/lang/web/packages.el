;; -*- no-byte-compile: t; -*-
;;; lang/web/packages.el

;; +html.el
(package! emmet-mode)
(package! haml-mode)
(package! pug-mode)
(package! slim-mode)
(when (package! web-mode)
  (when (featurep! :completion company)
    (package! company-web)))

;; +css.el
(package! less-css-mode)
(package! sass-mode)
(package! stylus-mode)
<<<<<<< HEAD
<<<<<<< HEAD
(package! rainbow-mode)
(when (featurep! :completion ivy)
  (package! counsel-css))
(when (featurep! :completion helm)
  (package! helm-css-scss))
=======
(when (featurep! +lsp)
  (package! lsp-css))
=======
>>>>>>> Cleared up README for lang/web

>>>>>>> Added lsp support for Javascript, HTML and CSS/SCSS/SASS/LESS
