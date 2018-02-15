;; -*- no-byte-compile: t; -*-
;;; config/private/packages.el

(when (file-directory-p +private-modules-path)
  ;; automatically symlinks your private modules to `doom-modules-dir'.
  (let ((symlink (expand-file-name +private-module-prefix doom-modules-dir)))
    (if (and (file-directory-p symlink)
             (not (file-symlink-p symlink)))
        (lwarn "config/private" :warning
               "modules/%s already exists; can't create symlink" +private-module-prefix)
      (make-symbolic-link +private-modules-path symlink t))))

;;
(load (expand-file-name "packages.el" +private-config-path)
      'noerror 'nomessage)
