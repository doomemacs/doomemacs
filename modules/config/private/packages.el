;; -*- no-byte-compile: t; -*-
;;; config/private/packages.el

(let ((modules-path (expand-file-name "modules/" +private-config-path)))
  (when (file-directory-p modules-path)
    ;; automatically symlinks your private modules to `doom-modules-dir'.
    (if (and (file-directory-p +private-symlink-path)
             (not (file-symlink-p +private-symlink-path)))
        (lwarn "config/private" :warning
               "modules/%s already exists; can't create symlink" +private-symlink-path)
      (make-symbolic-link modules-path +private-symlink-path t))))

;;
(load (expand-file-name "packages.el" +private-config-path)
      'noerror 'nomessage)
