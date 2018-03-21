;;; core/autoload/debug.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom/what-face (&optional pos)
  "Shows all faces and overlay faces at point.

Interactively prints the list to the echo area. Noninteractively, returns a list
whose car is the list of faces and cadr is the list of overlay faces."
  (interactive)
  (let* ((pos (or pos (point)))
         (faces (let ((face (get-text-property pos 'face)))
                  (if (keywordp (car-safe face))
                      (list face)
                    (cl-loop for f in (doom-enlist face) collect f))))
         (overlays (cl-loop for ov in (overlays-at pos (1+ pos))
                            nconc (doom-enlist (overlay-get ov 'face)))))
    (cond ((called-interactively-p 'any)
           (message "%s %s\n%s %s"
                    (propertize "Faces:" 'face 'font-lock-comment-face)
                    (if faces
                        (cl-loop for face in faces
                                 if (listp face)
                                   concat (format "'%s " face)
                                 else
                                   concat (concat (propertize (symbol-name face) 'face face) " "))
                      "n/a ")
                    (propertize "Overlays:" 'face 'font-lock-comment-face)
                    (if overlays
                        (cl-loop for ov in overlays
                                 concat (concat (propertize (symbol-name ov) 'face ov) " "))
                      "n/a")))
          (t
           (and (or faces overlays)
                (list faces overlays))))))

;;;###autoload
(defun doom-active-minor-modes ()
  "Get a list of active minor-mode symbols."
  (cl-loop for mode in minor-mode-list
           if (and (boundp mode) (symbol-value mode))
           collect mode))

;;;###autoload
(defun doom/what-minor-mode (mode)
  "Get information on an active minor mode. Use `describe-minor-mode' for a
selection of all minor-modes, active or not."
  (interactive
   (list (completing-read "Minor mode: "
                          (doom-active-minor-modes))))
  (describe-minor-mode-from-symbol
   (cl-typecase mode
     (string (intern mode))
     (symbol mode)
     (t (error "Expected a symbol/string, got a %s" (type-of mode))))))

;;;###autoload
(defun doom/am-i-secure ()
  "Test to see if your root certificates are securely configured in emacs."
  (declare (interactive-only t))
  (interactive)
  (unless (string-match-p "\\_<GNUTLS\\_>" system-configuration-features)
    (warn "gnutls support isn't built into Emacs, there may be problems"))
  (if-let* ((bad-hosts
             (cl-loop for bad
                      in '("https://wrong.host.badssl.com/"
                           "https://self-signed.badssl.com/")
                      if (condition-case _e
                             (url-retrieve-synchronously bad)
                           (error nil))
                      collect bad)))
      (error (format "tls seems to be misconfigured (it got %s)."
                     bad-hosts))
    (url-retrieve "https://badssl.com"
                  (lambda (status)
                    (if (or (not status) (plist-member status :error))
                        (warn "Something went wrong.\n\n%s" (pp-to-string status))
                      (message "Your trust roots are set up properly.\n\n%s" (pp-to-string status))
                      t)))))

(defvar doom--profiler nil)
;;;###autoload
(defun doom/toggle-profiler ()
  "Toggle the Emacs profiler. Starts it if isn't running. Stops it and pops up
the profiling report otherwise."
  (interactive)
  (if (not doom--profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq doom--profiler (not doom--profiler)))

;;;###autoload
(defun doom-info ()
  "Returns diagnostic information about the current Emacs session in markdown,
ready to be pasted in a bug report on github."
  (require 'vc-git)
  (let ((default-directory doom-emacs-dir))
    (format
     (concat "- OS: %s (%s)\n"
             "- Emacs: %s (%s)\n"
             "- Doom: %s (%s %s)\n"
             "- Graphic display: %s (daemon: %s)\n"
             "- System features: %s\n"
             "- Details:\n"
             "  ```elisp\n"
             "  uname -a:  %s\n"
             "  modules:   %s\n"
             "  packages:  %s\n"
             "  elc dirs:  %s\n"
             "  exec-path: %s\n"
             "  ```\n")
     system-type system-configuration
     emacs-version (format-time-string "%b %d, %Y" emacs-build-time)
     doom-version
     (if-let* ((branch (vc-git--symbolic-ref "core/core.el")))
         branch
       "n/a")
     (if-let* ((rev (vc-git-working-revision "core/core.el")))
         (format "https://github.com/hlissner/doom-emacs/commit/%s" rev)
       "n/a")
     (display-graphic-p) (daemonp)
     (bound-and-true-p system-configuration-features)
     ;; details
     (with-temp-buffer
       (unless (zerop (call-process "uname" nil t nil "-a"))
         (insert (format "%s" system-type)))
       (string-trim (buffer-string)))
     (or (cl-loop with cat = nil
                  for key being the hash-keys of doom-modules
                  if (or (not cat) (not (eq cat (car key))))
                  do (setq cat (car key)) and collect cat
                  else collect
                  (let ((flags (doom-module-get cat (cdr key) :flags)))
                    (if (equal flags '(t))
                        (cdr key)
                      (list (cdr key) flags))))
         "n/a")
     (or (let (packages)
           (ignore-errors
             (require 'async)
             ;; collect these in another session to protect this
             ;; session's state
             (async-get
              (async-start
               `(lambda ()
                  (setq load-path ',load-path)
                  (load ,(expand-file-name "core/core.el" doom-emacs-dir))
                  (load ,(expand-file-name "init.el" doom-emacs-dir))
                  (load ,(expand-file-name "core/autoload/packages.el" doom-emacs-dir))
                  (doom-get-packages))
               (lambda (p) (setq packages p))))
             (mapcar (lambda (x)
                       (if (cdr x)
                           (format "%s" x)
                         (symbol-name (car x))))
                     (cl-sort packages #'string-lessp :key (lambda (x) (symbol-name (car x)))))))
         "n/a")
     (or (ignore-errors
           (cl-delete-duplicates
            (cl-loop for file in (append (reverse (directory-files-recursively doom-core-dir "\\.elc$"))
                                         (cl-loop for dir in doom-modules-dirs
                                                  nconc (directory-files-recursively dir "\\.elc$")))
                     collect (file-relative-name (file-name-directory file) doom-emacs-dir))
            :test #'equal))
         "n/a")
     exec-path)))

;;;###autoload
(defun doom/info ()
  "Collects some debug information about your Emacs session, formats it into
markdown and copies it to your clipboard, ready to be pasted into bug reports!"
  (declare (interactive-only t))
  (interactive)
  (if noninteractive
      (message "%s" (doom-info))
    (message "Generating Doom info...")
    (kill-new (doom-info))
    (message "Done! Copied to clipboard.")))

;;;###autoload
(defun doom/toggle-debug-mode ()
  (interactive)
  (setq doom-debug-mode (not doom-debug-mode))
  (toggle-debug-on-error))
