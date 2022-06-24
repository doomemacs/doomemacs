;;; config/literate/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (add-hook 'org-mode-hook #'+literate-enable-recompile-h)

(defvar +literate-config-file
  (concat doom-private-dir "config.org")
  "The file path of your literate config file.")

(defvar +literate-tangle--async-proc nil)
(defvar +literate-tangle--async-proc-start-time nil)

(defvar org-mode-hook)
(defvar org-inhibit-startup)

(defun +literate-tangle (target dest &optional dir)
  "Tangle TARGET org file to DEST."
  (and (require 'ox nil t)
       (require 'ob-tangle nil t)
       (let* ((default-directory (or dir default-directory))
              (target (expand-file-name target))
              (dest   (expand-file-name dest)))
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
          t))))

(defun +literate-tangle--sync ()
  "Tangles `+literate-config-file' if it has changed."
  (and (not (getenv "__NOTANGLE"))
       (+literate-tangle +literate-config-file
                         (concat doom-module-config-file ".el")
                         doom-private-dir)
       (always (print! "Restarting..."))
       (exit! "__DOOMRESTART=1 __NOTANGLE=1 $@")))

(defun +literate-tangle--async ()
  "Tangles `+literate-config-file' using an async Emacs process."
  (unless (getenv "__NOTANGLE")
    (when +literate-tangle--async-proc
      (message "Killing outdated tangle process...")
      (set-process-sentinel +literate-tangle--async-proc #'ignore)
      (kill-process +literate-tangle--async-proc)
      (sit-for 0.3)) ; ensure the message is seen for a bit
    (setq +literate-tangle--async-proc-start-time (float-time)
          +literate-tangle--async-proc
          ;; See `+literate-tangle--sync' for an explanation of the (progn ...) below.
          (start-process "tangle-config"
                         (get-buffer-create " *tangle config*")
                         "emacs" "--batch"
                         "--eval"
                         (prin1-to-string
                          `(progn
                             (require 'cl-lib)
                             (require 'subr-x)
                             (load ,(doom-path doom-core-dir "autoload/print"))
                             (funcall #',(symbol-function #'+literate-tangle)
                                    ,+literate-config-file
                                    ,(concat doom-module-config-file ".el")
                                    ,doom-private-dir)))))
    (add-hook 'kill-emacs-hook #'+literate-tangle-check-finished-h)
    (set-process-sentinel +literate-tangle--async-proc #'+literate-tangle--async-sentinel)
    (run-at-time nil nil (lambda () (message "Tangling config.org"))) ; ensure shown after a save message
    "Tangling config.org..."))

(defun +literate-tangle--async-sentinel (process signal)
  (cond
   ((and (eq 'exit (process-status process))
         (= 0 (process-exit-status process)))
    (message "Tangled config.org sucessfully (took %.1fs)"
             (- (float-time) +literate-tangle--async-proc-start-time))
    (setq +literate-tangle--async-proc nil))
   ((memq (process-status process) '(exit signal))
    (pop-to-buffer (get-buffer " *tangle config*"))
    (message "Failed to tangle config.org (after %.1fs)"
             (- (float-time) +literate-tangle--async-proc-start-time))
    (setq +literate-tangle--async-proc nil))))


;;
;;; Commands

;;;###autoload
(defalias '+literate/reload #'doom/reload)


;;
;;; Hooks

;;;###autoload
(defun +literate-tangle-h ()
  "Tangles `+literate-config-file' if it has changed.
This is performed with an asyncronous Emacs process, except when
`noninteractive' is nil."
  (if noninteractive
      (+literate-tangle--sync)
    (+literate-tangle--async)))

;;;###autoload
(defun +literate-tangle-check-finished-h ()
  "When a tangle is still in progress, ask the user if they want to wait for it."
  (when (and (process-live-p +literate-tangle--async-proc)
             (yes-or-no-p "Config is currently retangling, would you please wait a few seconds?"))
    (switch-to-buffer " *tangle config*")
    (signal 'quit nil)))

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
        (buffer-file-name (buffer-base-buffer))
        (file-name-directory +literate-config-file))
       (+literate-tangle-h)))
