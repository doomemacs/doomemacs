;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/csharp/doctor.el

(when (and (require 'omnisharp nil t) (not (modulep! +lsp)))
  (let ((omnisharp-bin (or omnisharp-server-executable-path (omnisharp--server-installation-path t))))
    (unless (file-exists-p omnisharp-bin)
      (warn! "Omnisharp server isn't installed, completion won't work"))))

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")
