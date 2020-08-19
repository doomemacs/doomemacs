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


;;;###autoload
(defun +literate-tangle-h ()
  "Tangles `+literate-config-file' if it has changed."
  (print! (start "Compiling your literate config..."))
  (print-group!
   (and (not (getenv "NOTANGLE"))
        (require 'ox nil t)
        (require 'ob-tangle nil t)
        (letf! ((default-directory doom-private-dir)
                (target +literate-config-file)
                (cache +literate-config-cache-file)
                (dest (concat (file-name-sans-extension target) ".el"))
                ;; Operate on a copy because `org-babel-tangle' has
                ;; side-effects we need to undo immediately as not to
                ;; overwrite the user's config; it's bad ettiquite.
                (backup (make-temp-file (concat (file-name-nondirectory target) ".")))

                ;; HACK A hack to prevent ob-tangle from operating relative to
                ;;      the backup file and thus tangling to the wrong
                ;;      destinations.
                (defun org-babel-tangle-single-block (&rest args)
                  (let* ((spec (apply org-babel-tangle-single-block args))
                         (file (nth 1 spec))
                         (file (if (file-equal-p file backup) target file))
                         (file (if org-babel-tangle-use-relative-file-links
                                   (file-relative-name file)
                                 file)))
                    (setf (nth 1 spec) file)
                    spec))
                ;; Ensure output conforms to the formatting of all doom CLIs
                (defun message (msg &rest args)
                  (when msg
                    (print! (info "%s") (apply #'format msg args)))))
          (unwind-protect
              (with-temp-file backup
                (insert-file-contents target)
                (let ((buffer-file-name backup)
                      ;; Prevent unwanted entries in recentf, or formatters, or
                      ;; anything that could be on these hooks, really. Nothing
                      ;; else should be touching these files (particularly in
                      ;; interactive sessions).
                      (write-file-functions nil)
                      (before-save-hook nil)
                      (after-save-hook nil)
                      ;; Prevent infinite recursion due to recompile-on-save
                      ;; hooks later, and speed up `org-mode' init.
                      (org-mode-hook nil)
                      (org-inhibit-startup t))
                  (org-mode)
                  (with-silent-modifications
                    ;; Tangling won't ordinarily expand #+INCLUDE directives,
                    ;; so I do it myself.
                    (org-export-expand-include-keyword)
                    (org-babel-tangle nil dest))))
            (ignore-errors (delete-file backup)))
          ;; Write an empty file to serve as our mtime cache
          (with-temp-file cache)
          (if doom-interactive-p t
            (message "Restarting..." )
            (doom-cli-execute-lines-after "NOTANGLE=1 \"$@\"")
            (kill-emacs 0))))))

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
