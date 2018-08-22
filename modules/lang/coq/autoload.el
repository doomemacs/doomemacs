;;; lang/coq/autoload.el -*- lexical-binding: t; -*-
;;;###if (featurep! :completion company)

;;;###autoload
(add-hook 'coq-mode-hook #'company-coq-mode)
