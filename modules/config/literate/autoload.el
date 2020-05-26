;;; config/literate/autoload.el -*- lexical-binding: t; -*-

(defvar +literate-config-file
  (concat doom-private-dir "config.org")
  "The file path of your literate config file.")

(defvar +literate-config-cache-file
  (concat doom-cache-dir "literate-last-compile")
  "The file path that `+literate-config-file' will be tangled to, then
byte-compiled from.")

;;;###autoload
(defun +literate-tangle-h (&optional force-p)
  "Tangles `+literate-config-file' if it has changed."
  (let ((default-directory doom-private-dir))
    (when (or (file-newer-than-file-p +literate-config-file
                                      +literate-config-cache-file)
              force-p)
      (print! (start "Compiling your literate config..."))
      (print-group!
       (let* ((org  (expand-file-name +literate-config-file))
              (dest (concat (file-name-sans-extension +literate-config-file) ".el"))
              (output (get-buffer-create "*org-tangle*")))
         (unwind-protect
             ;; We tangle in a separate, blank process because loading it here
             ;; would load all of :lang org (very expensive!).
             (and (require 'ob-tangle)
                  (letf! (defun message (msg &rest args)
                           (print! (info "%s") (apply #'format msg args)))
                    (org-babel-tangle-file org dest))
                  ;; Write the cache file to serve as our mtime cache
                  (with-temp-file +literate-config-cache-file))
           (kill-buffer output)))))))

;;;###autoload
(after! org
  ;; Recompile our literate config if we modify it
  (add-hook 'after-save-hook #'+literate-recompile-maybe-h))

;;;###autoload
(defalias '+literate/reload #'doom/reload)

;;;###autoload
(defun +literate-recompile-maybe-h ()
  "Recompile config.org if we're editing an org file in our DOOMDIR.

We assume any org file in `doom-private-dir' is connected to your literate
config, and should trigger a recompile if changed."
  (when (and (eq major-mode 'org-mode)
             (file-in-directory-p buffer-file-name doom-private-dir))
    (+literate-tangle-h 'force)))
