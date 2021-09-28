;;; lang/web/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(unless (executable-find "js-beautify")
  (warn! "Couldn't find js-beautify. Code formatting in JS/CSS/HTML modes will not work."))

(unless (executable-find "stylelint")
  (warn! "Couldn't find stylelint. Linting for CSS modes will not work."))

(unless (executable-find "tidy")
  (warn! "Couldn't find tidy. Code formatting in HTML modes will not work."))
