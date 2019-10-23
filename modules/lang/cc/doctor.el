;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/cc/doctor.el

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(when (require 'rtags nil t)
  ;; rtags
  (when-let (bins (cl-remove-if #'rtags-executable-find
                                (list rtags-rdm-binary-name
                                      rtags-rc-binary-name)))
    (warn! "Couldn't find the rtag client and/or server programs %s. Disabling rtags support"
           bins)))

;; irony server
(when (require 'irony nil t)
  (unless (file-directory-p irony-server-install-prefix)
    (warn! "Irony server isn't installed. Run M-x irony-install-server")))

(when (featurep! :completion company)
  ;; glslangValidator
  (unless (executable-find "glslangValidator")
    (warn! "Couldn't find glslangValidator. GLSL code completion is disabled")))
