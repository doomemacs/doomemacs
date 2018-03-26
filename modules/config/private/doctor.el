;;; config/private/doctor.el -*- lexical-binding: t; -*-

(unless (file-directory-p +private-config-path)
  (warn! "Couldn't find '%s'" (file-relative-name +private-config-path "~")))

