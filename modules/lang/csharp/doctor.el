;;; lang/csharp/doctor.el -*- lexical-binding: t; -*-

(require 'omnisharp)
(let ((omnisharp-bin (or omnisharp-server-executable-path (omnisharp--server-installation-path t))))
  (unless (file-exists-p omnisharp-bin)
    (warn! "Omnisharp server isn't installed, completion won't work")))
