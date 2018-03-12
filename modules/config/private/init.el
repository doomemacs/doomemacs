;;; config/private/init.el -*- lexical-binding: t; -*-

(defvar +private-config-path
  (if (featurep! +xdg)
      (expand-file-name "doom/" (or (getenv "XDG_CONFIG_HOME") "~/.config"))
    "~/.doom.d")
  "The directory that serves as the root of your external private config for
Doom Emacs.")

;; Ensure `doom//reload-autoloads', `doom//byte-compile' and
;; `doom-initialize-packages' will treat `+private-config-path' as the root of
;; this module.
(cl-pushnew +private-config-path doom-psuedo-module-dirs)
(cl-pushnew (expand-file-name "modules/" +private-config-path)
            doom-modules-dirs :test #'string=)

;;
(load (expand-file-name "init.el" +private-config-path)
      'noerror 'nomessage)
