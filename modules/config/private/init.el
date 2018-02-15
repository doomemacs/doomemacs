;;; config/private/init.el -*- lexical-binding: t; -*-

(defvar +private-config-path
  (if (featurep! +xdg)
      (expand-file-name "doom/" (or (getenv "XDG_CONFIG_HOME") "~/.config"))
    "~/.doom.d")
  "The directory that serves as the root of your external private config for
Doom Emacs.")

(defvar +private-symlink-path
  (expand-file-name "private" doom-modules-dir)
  "Where the place the symbolic link to the private modules directory.")

;; Ensure `doom//reload-autoloads', `doom//byte-compile' and
;; `doom-initialize-packages' all include this module in their operations.
(add-to-list 'doom-extra-module-paths +private-config-path)

;;
(load (expand-file-name "init.el" +private-config-path)
      'noerror 'nomessage)

