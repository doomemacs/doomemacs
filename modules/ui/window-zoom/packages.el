;; -*- no-byte-compile: t; -*-
;;; ui/window-zoom/packages.el

(cond ((featurep! +zoom)
       ;; Install zoom if the user indicated the '+zoom' module flag
       (package! zoom))
      ((or (featurep! +golden-ratio) t)
       ;; Install golden-ratio if the user selects the flag '+golden-ratio' or by default
       ;; ... if the user did not specify a module flag
       (package! golden-ratio)))
