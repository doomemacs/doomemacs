;; -*- no-byte-compile: t; -*-
;;; lang/web/packages.el

(package! rainbow-mode)
(package! web-beautify)
(when (featurep! :completion ivy)
  (package! counsel-css :recipe (:fetcher github :repo "hlissner/emacs-counsel-css")))

;; +html.el
(package! company-web)
(package! emmet-mode)
(package! haml-mode)
(package! pug-mode)
(package! web-mode)

;; +css.el
(package! less-css-mode)
(package! sass-mode)
(package! stylus-mode)

;;
(def-bootstrap! web
  :requires nodejs
  (unless (executable-find "js-beautify")
    (sh "npm -g install js-beautify"))
  (unless (executable-find "stylelint")
    (sh "npm -g install stylelint stylelint-scss")))
