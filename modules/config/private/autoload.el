;;; config/private/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+private/find-in-config "config/private/autoload" nil t)
(+default--def-find-in!   config +private-config-path)
;;;###autoload (autoload '+private/browse-config "config/private/autoload" nil t)
(+default--def-browse-in! config +private-config-path)
