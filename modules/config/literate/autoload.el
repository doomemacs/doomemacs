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
          (org    (expand-file-name +literate-config-file))
          (dest   (concat (file-name-sans-extension +literate-config-file) ".el")))
     (and (require 'ox)
          (require 'ob-tangle)
          (unwind-protect
              (letf! ((defun message (msg &rest args)
                        (when msg
                          (print! (info "%s") (apply #'format msg args))))
                      ;; Prevent infinite recursion due to recompile-on-save
                      ;; hooks later.
                      (org-mode-hook nil)
                      ;; Operate on a copy because `org-babel-tangle' has
                      ;; side-effects we don't want to impose on the User's
                      ;; config permanently.
                      (backup (make-temp-file (concat (file-name-nondirectory org) "."))))
                (copy-file org backup t)
                (with-current-buffer (find-file-noselect backup)
                  ;; Tangling won't ordinarily expand #+INCLUDE directives
                  (org-export-expand-include-keyword)
                  (org-babel-tangle nil dest)
                  (kill-buffer (current-buffer)))
                t)
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
