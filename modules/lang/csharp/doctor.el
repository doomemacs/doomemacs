;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/csharp/doctor.el

(when (and (require 'omnisharp nil t) (not (modulep! +lsp)))
  (let ((omnisharp-bin (or omnisharp-server-executable-path (omnisharp--server-installation-path t))))
    (unless (file-exists-p omnisharp-bin)
      (warn! "Omnisharp server isn't installed, completion won't work"))))

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(when (modulep! :editor format)
  (unless (and (file-exists-p (expand-file-name "~/.dotnet/tools/dotnet-csharpier"))
               (file-exists-p ".config/dotnet-tools.json")
               (eq 0 (call-process-shell-command
                      (format "grep -q 'dotnet-csharpier' %s" (expand-file-name ".config/dotnet-tools.json")) nil nil)))
    (warn! "csharpier is not installed or setup as a local tool. Please see the module README. \nOtherwise, formatting will be disabled.")))
