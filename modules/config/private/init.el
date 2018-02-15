;;; config/private/init.el -*- lexical-binding: t; -*-

(defvar +private-config-path
  (if (featurep! +xdg)
      (expand-file-name "doom/" (or (getenv "XDG_CONFIG_HOME") "~/.config"))
    "~/.doom.d")
  "The directory that serves as the root of your external private config for
Doom Emacs.")

(defvar +private-modules-path
  (expand-file-name "modules/" +private-config-path)
  "The path to your private, external modules. This will be symlinked to
external/ in `doom-modules-dir'.")

(defvar +private-module-prefix "private"
  "TODO")

;; Ensure `doom//reload-autoloads', `doom//byte-compile' and
;; `doom-initialize-packages' all include this module in their operations.
(add-to-list 'doom-extra-module-paths +private-config-path)

;;
(load (expand-file-name "init.el" +private-config-path)
      'noerror 'nomessage)

