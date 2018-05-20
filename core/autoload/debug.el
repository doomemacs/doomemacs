;;; core/autoload/debug.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-info ()
  "Returns diagnostic information about the current Emacs session in markdown,
ready to be pasted in a bug report on github."
  (doom-initialize)
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
             "  ```")
     system-type system-configuration
     emacs-version (format-time-string "%b %d, %Y" emacs-build-time)
     doom-version
     (if-let* ((branch (vc-git--symbolic-ref "core/core.el")))
         branch
       "n/a")
     (if-let* ((rev (vc-git-working-revision "core/core.el")))
         rev
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
                    (if flags
                        `(,(cdr key) ,@flags)
                      (cdr key))))
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
             (cl-loop for pkg in (cl-sort packages #'string-lessp
                                          :key (lambda (x) (symbol-name (car x))))
                      collect (if (cdr pkg)
                                  (format "%s" pkg)
                                (symbol-name (car pkg))))))
         "n/a")
     (or (ignore-errors
           (cl-delete-duplicates
            (cl-loop for file in (append (reverse (directory-files-recursively doom-core-dir "\\.elc$"))
                                         (cl-loop for dir in doom-modules-dirs
                                                  nconc (directory-files-recursively dir "\\.elc$")))
                     collect (file-relative-name (file-name-directory file) doom-emacs-dir))
            :test #'equal))
         "n/a")
     ;; abbreviate $HOME to hide username
     (mapcar #'abbreviate-file-name exec-path))))


;;
;; Commands
;;

;;;###autoload
(defun doom//info ()
  "Collects some debug information about your Emacs session, formats it into
markdown and copies it to your clipboard, ready to be pasted into bug reports!"
  (declare (interactive-only t))
  (interactive)
  (message "Generating Doom info...")
  (if noninteractive
      (print! (doom-info))
    (kill-new (doom-info))
    (message "Done! Copied to clipboard.")))

;;;###autoload
(defun doom//am-i-secure ()
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
      (error "tls seems to be misconfigured (it got %s)."
             bad-hosts)
    (url-retrieve "https://badssl.com"
                  (lambda (status)
                    (if (or (not status) (plist-member status :error))
                        (warn "Something went wrong.\n\n%s" (pp-to-string status))
                      (message "Your trust roots are set up properly.\n\n%s" (pp-to-string status))
                      t)))))

;;;###autoload
(defun doom//version ()
  "Display the current version of Doom & Emacs, including the current Doom
branch and commit."
  (interactive)
  (require 'vc-git)
  (print! "Doom v%s (Emacs v%s)\nBranch: %s\nCommit: %s."
          doom-version
          emacs-version
          (or (vc-git--symbolic-ref doom-core-dir)
              "n/a")
          (or (vc-git-working-revision doom-core-dir)
              "n/a")))


;;
;; Profiling
;;

(defvar doom--profiler nil)
;;;###autoload
(defun doom//toggle-profiler ()
  "Toggle the Emacs profiler. Run it again to see the profiling report."
  (interactive)
  (if (not doom--profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq doom--profiler (not doom--profiler)))

;;;###autoload
(defun doom//profile-emacs ()
  "Profile the startup time of Emacs in the background with ESUP.
If INIT-FILE is non-nil, profile that instead of USER-INIT-FILE."
  (interactive)
  (require 'esup)
  (let ((init-file esup-user-init-file))
    (message "Starting esup...")
    (esup-reset)
    (setq esup-server-process (esup-server-create (esup-select-port)))
    (setq esup-server-port (process-contact esup-server-process :service))
    (message "esup process started on port %s" esup-server-port)
    (let ((process-args `("*esup-child*"
                          "*esup-child*"
                          ,esup-emacs-path
                          "-q"
                          "-L" ,esup-load-path
                          "-l" "esup-child"
                          ,(format "--eval=(esup-child-run \"%s\" \"%s\" %d)"
                                   init-file
                                   esup-server-port
                                   esup-depth)
                          "--eval=(run-hooks 'after-init-hook 'emacs-startup-hook 'window-setup-hook)")))
      (when esup-run-as-batch-p
        (setq process-args (append process-args '("--batch"))))
      (setq esup-child-process (apply #'start-process process-args)))
    (set-process-sentinel esup-child-process 'esup-child-process-sentinel)))

