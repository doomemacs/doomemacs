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
          (dest   (concat (file-name-sans-extension +literate-config-file) ".el"))
          ;; Operate on a copy because `org-babel-tangle' has side-effects we
          ;; don't want to impose on the User's config permanently.
          (backup (make-temp-file (concat (file-name-nondirectory org) "."))))
     (unwind-protect
         (and (require 'ox)
              (require 'ob-tangle)
              (letf! ((defun message (msg &rest args)
                        (when msg
                          (print! (info "%s") (apply #'format msg args))))
                      ;; Prevent infinite recursion due to recompile-on-save
                      ;; hooks later.
                      (org-mode-hook nil))
                ;; Tangling won't ordinarily expand #+INCLUDE directives, and it
                ;; modifies the buffer so we must do it in a copy to prevent
                ;; stepping on the user's toes.
                (with-temp-file backup
                  (let ((buffer-file-name backup)
                        (org-inhibit-startup t)
                        org-mode-hook)
                    (insert-file-contents org)
                    (org-mode)
                    (org-export-expand-include-keyword)
                    (org-babel-tangle nil dest)))
                t)
              ;; Write an empty file to serve as our mtime cache
              (with-temp-file +literate-config-cache-file))
       (ignore-errors (delete-file backup))))))

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
