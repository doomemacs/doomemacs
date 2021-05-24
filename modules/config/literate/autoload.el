;;; config/literate/autoload.el -*- lexical-binding: t; -*-

(defvar +literate-config-file
  (concat doom-private-dir "config.org")
  "The file path of your literate config file.")

(defvar +literate-config-cache-file
  (concat doom-cache-dir "literate-last-compile")
  "The file path that `+literate-config-file' will be tangled to, then
byte-compiled from.")

(defvar org-mode-hook)
(defvar org-inhibit-startup)

;;;###autoload (add-hook 'org-mode-hook #'+literate-enable-recompile-h)

;;;###autoload
(defun +literate-tangle-h ()
  "Tangles `+literate-config-file' if it has changed."
  (and (not (getenv "__NOTANGLE"))
       (require 'ox nil t)
       (require 'ob-tangle nil t)
       (letf! ((default-directory doom-private-dir)
               (target +literate-config-file)
               (cache +literate-config-cache-file)
               (dest (expand-file-name (concat doom-module-config-file ".el")))
               ;; Ensure output conforms to the formatting of all doom CLIs
               (defun message (msg &rest args)
                 (when msg
                   (print! (info "%s") (apply #'format msg args)))))
         (print! (start "Compiling your literate config..."))
         (print-group!
          (let (;; Do as little unnecessary work as possible in these org files.
                (org-startup-indented nil)
                (org-startup-folded nil)
                (vc-handled-backends nil)
                ;; Prevent unwanted entries in recentf, or formatters, or
                ;; anything that could be on these hooks, really. Nothing else
                ;; should be touching these files (particularly in interactive
                ;; sessions).
                (write-file-functions nil)
                (before-save-hook nil)
                (after-save-hook nil)
                ;; Prevent infinite recursion due to recompile-on-save hooks
                ;; later, and speed up `org-mode' init.
                (org-mode-hook nil)
                (org-inhibit-startup t)
                ;; Allow evaluation of src blocks at tangle-time (would abort
                ;; them otherwise). This is a security hazard, but Doom will
                ;; trust that you know what you're doing!
                (org-confirm-babel-evaluate nil))
            (org-babel-tangle-file target dest))
          ;; Write an empty file to serve as our mtime cache
          (with-temp-file cache)
          (if doom-interactive-p t
            (message "Restarting..." )
            (throw 'exit "__DOOMRESTART=1 __NOTANGLE=1 $@"))))))

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
  (and (file-in-directory-p
        buffer-file-name (file-name-directory +literate-config-file))
       (+literate-tangle-h)))
