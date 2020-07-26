;;; config/literate/autoload.el -*- lexical-binding: t; -*-

(defvar +literate-config-file
  (concat doom-private-dir "config.org")
  "The file path of your literate config file.")

(defvar +literate-config-cache-file
  (concat doom-cache-dir "literate-last-compile")
  "The file path that `+literate-config-file' will be tangled to, then
byte-compiled from.")

;;;###autoload
(defun +literate-tangle-h ()
  "Tangles `+literate-config-file' if it has changed."
  (print! (start "Compiling your literate config..."))
  (print-group!
   (let* ((default-directory doom-private-dir)
          (org  (expand-file-name +literate-config-file))
          (dest (concat (file-name-sans-extension +literate-config-file) ".el"))
          (backup (make-temp-file "config.org.backup")))
     (and (require 'ox)
          (require 'ob-tangle)
          (unwind-protect
              (letf! ((defun message (msg &rest args)
                        (when msg
                          (print! (info "%s") (apply #'format msg args))))
                      ;; Prevent infinite recursion due to
                      ;; recompile-on-save hooks later.
                      (org-mode-hook nil))
                ;; We do the ol' switcheroo because `org-babel-tangle'
                ;; writes changes to the current file, which would be
                ;; imposing on the user.
                (copy-file org backup t)
                (with-current-buffer (find-file-noselect org)
                  ;; Tangling doesn't expand #+INCLUDE directives, so we
                  ;; do it ourselves, since includes are so useful for
                  ;; literate configs!
                  (org-export-expand-include-keyword)
                  (org-babel-tangle nil dest))
                t)
            (ignore-errors (copy-file backup org t))
            (ignore-errors (delete-file backup)))
          ;; Write an empty file to serve as our mtime cache
          (with-temp-file +literate-config-cache-file)))))

;;;###autoload
(add-hook 'org-mode-hook #'+literate-enable-recompile-h)

;;;###autoload
(defalias '+literate/reload #'doom/reload)

;;;###autoload
(defun +literate-enable-recompile-h ()
  "Enable literate-compiling-on-save in the current buffer."
  (add-hook 'after-save-hook #'+literate-recompile-maybe-h nil 'local))

;;;###autoload
(defun +literate-recompile-maybe-h ()
  "Recompile literate config to `doom-private-dir'.

We assume any org file in `doom-private-dir' is connected to your literate
config, and should trigger a recompile if changed."
  (when (file-in-directory-p buffer-file-name doom-private-dir)
    (+literate-tangle-h)))
