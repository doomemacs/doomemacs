;; -*- no-byte-compile: t; -*-
;;; ui/window-select/packages.el

(cond ((featurep! +switch-window)
       ;; Install switch-window if the user indicated the '+switch-window'
       ;; module flag
       (package! switch-window))
      ((or (featurep! +ace-window) t)
       ;; Install ace-window if the user selects the flag '+ace-window' or by
       ;; default ... if the user did not specify a module flag
       (package! ace-window)))

