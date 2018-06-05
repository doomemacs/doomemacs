;;; config/literate/config.el -*- lexical-binding: t; -*-

(defvar +literate-config-file "config.org"
  "The literate config file, searched for in `doom-private-dir' (unless this is
an absolute path).")

(defvar +literate-config-dest-file "config.el"
  "The file that `+literate-config-file' will be tangled to, then byte-compiled
from.")


;;
(let ((org (expand-file-name +literate-config-file doom-private-dir))
      (elc (expand-file-name (concat +literate-config-dest-file "c") doom-private-dir)))
  ;; If config is pre-compiled, then load that
  (when (file-newer-than-file-p org elc)
    ;; We tangle in a separate, blank process because loading it here would load
    ;; all of :lang org, which will be more expensive than it needs to be.
    (or (zerop (call-process
                "emacs" nil nil nil
                "-q" "--batch" "-l" "ob-tangle" "--eval"
                (format "(org-babel-tangle-file \"%s\" \"%s\" \"emacs-lisp\")"
                        org +literate-config-dest-file)))
        (error "There was a problem tangling your literate config!"))

    ;; Then byte-compile it!
    (require 'bytecomp)
    (byte-compile-file +literate-config-dest-file)))

;; No need to load the resulting file. Doom will do this for us after all
;; modules have finished loading.
