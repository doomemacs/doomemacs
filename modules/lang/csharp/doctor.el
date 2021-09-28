;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/csharp/doctor.el

(when (and (require 'omnisharp nil t) (not (featurep! +lsp)))
  (let ((omnisharp-bin (or omnisharp-server-executable-path (omnisharp--server-installation-path t))))
    (unless (file-exists-p omnisharp-bin)
      (warn! "Omnisharp server isn't installed, completion won't work"))))
