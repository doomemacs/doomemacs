;;; core/autoload/debug.el -*- lexical-binding: t; -*-

(defun doom-template-insert (template)
  "TODO"
  (let ((file (expand-file-name (format "templates/%s" template) doom-core-dir)))
    (when (file-exists-p file)
      (insert-file-contents file))))

;;;###autoload
(defun doom-info ()
  "Returns diagnostic information about the current Emacs session in markdown,
ready to be pasted in a bug report on github."
  (require 'vc-git)
  (let ((default-directory doom-emacs-dir)
        (doom-modules (doom-modules)))
    (format
     (concat "- OS: %s (%s)\n"
             "- Emacs: %s (%s)\n"
             "- Doom: %s (%s %s)\n"
             "- Graphic display: %s (daemon: %s)\n"
             "- System features: %s\n"
             "- Details:\n"
             "  ```elisp\n"
             "  elc count: %s\n"
             "  uname -a:  %s\n"
             "  modules:   %s\n"
             "  packages:  %s\n"
             "  exec-path: %s\n"
             "  ```")
     system-type system-configuration
     emacs-version (format-time-string "%b %d, %Y" emacs-build-time)
     doom-version
     (or (vc-git--symbolic-ref "core/core.el")
         "n/a")
     (or (vc-git-working-revision "core/core.el")
         "n/a")
     (display-graphic-p) (daemonp)
     (bound-and-true-p system-configuration-features)
     ;; details
     (length (doom-files-in `(,@doom-modules-dirs
                              ,doom-core-dir
                              ,doom-private-dir)
                            :type 'files :match "\\.elc$"))
     (if IS-WINDOWS
         "n/a"
       (with-temp-buffer
         (unless (zerop (call-process "uname" nil t nil "-a"))
           (insert (format "%s" system-type)))
         (string-trim (buffer-string))))
     (or (cl-loop with cat = nil
                  for key being the hash-keys of doom-modules
                  if (or (not cat) (not (eq cat (car key))))
                  do (setq cat (car key))
                  and collect cat
                  and collect (cdr key)
                  else collect
                  (let ((flags (doom-module-get cat (cdr key) :flags)))
                    (if flags
                        `(,(cdr key) ,@flags)
                      (cdr key))))
         "n/a")
     (or (ignore-errors
           (require 'use-package)
           (cl-loop for (name . plist) in (doom-get-packages :private t)
                    if (use-package-plist-delete (copy-seq plist) :private)
                    collect (format "%s" (cons name it))
                    else
                    collect (symbol-name name)))
         "n/a")
     ;; abbreviate $HOME to hide username
     (mapcar #'abbreviate-file-name exec-path))))


;;
;; Commands

;;;###autoload
(defun doom/info ()
  "Collects some debug information about your Emacs session, formats it into
markdown and copies it to your clipboard, ready to be pasted into bug reports!"
  (interactive)
  (message "Generating Doom info...")
  (if noninteractive
      (print! (doom-info))
    (kill-new (doom-info))
    (message "Done! Copied to clipboard.")))

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
      (error "tls seems to be misconfigured (it got %s)."
             bad-hosts)
    (url-retrieve "https://badssl.com"
                  (lambda (status)
                    (if (or (not status) (plist-member status :error))
                        (warn "Something went wrong.\n\n%s" (pp-to-string status))
                      (message "Your trust roots are set up properly.\n\n%s" (pp-to-string status))
                      t)))))

;;;###autoload
(defun doom/version ()
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

;;;###autoload
(defun doom/copy-backtrace ()
  "Copy the contents of the *Backtrace* window into your clipboard for easy
pasting into a bug report or discord."
  (interactive)
  (if-let* ((buf (get-buffer "*Backtrace*")))
      (with-current-buffer buf
        (kill-new
         (string-trim (buffer-string))))
    (user-error "No backtrace buffer detected")))


;;; Vanilla sandbox

(defvar doom--sandbox-init-doom-p nil)

(defun doom--run-vanilla-sandbox (&optional load-doom-p)
  (interactive)
  (let ((contents (buffer-string))
        (file (make-temp-file "doom-sandbox-")))
    (with-temp-file file
      (insert
       (prin1-to-string
        `(cond (,load-doom-p
                (setq doom-private-dir "/tmp/does/not/exist"
                      doom-modules ,doom-modules)
                (load ,user-init-file))
               ((setq package--init-file-ensured t
                      package-user-dir ,package-user-dir
                      package-archives ',package-archives
                      user-emacs-directory ,doom-emacs-dir)
                (package-initialize))))
       "\n(unwind-protect (progn\n" contents "\n)\n"
       (format "(delete-file %S))" file)))
    (let ((args (list "-Q" "-l" file)))
      (require 'restart-emacs)
      (condition-case e
          (cond ((display-graphic-p)
                 (if (memq system-type '(windows-nt ms-dos))
                     (restart-emacs--start-gui-on-windows args)
                   (restart-emacs--start-gui-using-sh args)))
                ((memq system-type '(windows-nt ms-dos))
                 (user-error "Cannot start another Emacs from Windows shell."))
                ((suspend-emacs
                  (format "%s %s -nw; fg"
                          (shell-quote-argument (restart-emacs--get-emacs-binary))
                          (string-join (mapcar #'shell-quote-argument args) " ")))))
        (error
         (delete-file file)
         (signal (car e) (cdr e)))))))

(defun doom--run-vanilla-doom-sandbox ()
  (interactive)
  (doom--run-vanilla-sandbox t))

;;;###autoload
(defun doom/open-vanilla-sandbox ()
  "Open an Emacs Lisp buffer destinated to run in a blank Emacs session (and
optionally load only Doom and its modules, without your private config).

This provides a testbed for debugging code without Doom (or your private config)
standing in the way, and without sacrificing access to installed packages."
  (interactive)
  (let* ((buffer-name "*doom:vanilla-sandbox*")
         (exists (get-buffer buffer-name))
         (buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (emacs-lisp-mode)
      (local-set-key (kbd "C-c C-c") #'doom--run-vanilla-sandbox)
      (local-set-key (kbd "C-c C-d") #'doom--run-vanilla-doom-sandbox)
      (local-set-key (kbd "C-c C-k") #'kill-this-buffer)
      (setq header-line-format "C-c C-c to run the session / C-c C-d to run it with vanilla Doom loaded / C-c C-k to abort it")
      (setq-local default-directory doom-emacs-dir)
      (unless (buffer-live-p exists)
        (doom-template-insert "VANILLA_SANDBOX"))
      (goto-char (point-max)))
    (pop-to-buffer buf)))


;;; Reporting bugs

(defun doom--open-bug-report ()
  "TODO"
  (interactive)
  (let ((url "https://github.com/hlissner/doom-emacs/issues/new?body="))
    ;; TODO Refactor me
    (save-restriction
      (widen)
      (goto-char (point-min))
      (re-search-forward "^-------------------------------------------------------------------\n" nil t)
      (skip-chars-forward " \n\t")
      (condition-case e
          (progn
            (save-excursion
              (when (and (re-search-backward "\\+ [ ] " nil t)
                         (not (y-or-n-p "You haven't checked all the boxes. Continue anyway?")))
                (error "Aborted submit")))
            (narrow-to-region (point) (point-max))
            (let ((text (buffer-string)))
              ;; `url-encode-url' doesn't encode ampersands
              (setq text (replace-regexp-in-string "&" "%26" text))
              (setq url (url-encode-url (concat url text)))
              ;; HACK: encode some characters according to HTML URL Encoding Reference
              ;; http://www.w3schools.com/tags/ref_urlencode.asp
              (setq url (replace-regexp-in-string "#" "%23" url))
              (setq url (replace-regexp-in-string ";" "%3B" url))
              (browse-url url)))
        (error (signal (car e) (car e)))))))

;;;###autoload
(defun doom/open-bug-report ()
  "Open a markdown buffer destinated to populate the New Issue page on Doom
Emacs' issue tracker.

If called when a backtrace buffer is present, it and the output of `doom-info'
will be automatically appended to the result."
  (interactive)
  ;; TODO Refactor me
  (let ((backtrace
         (when (get-buffer "*Backtrace*")
           (with-current-buffer "*Backtrace*"
             (string-trim
              (buffer-substring-no-properties
               (point-min)
               (min (point-max) 1000))))))
        (buf (get-buffer-create "*doom:vanilla-sandbox*")))
    (with-current-buffer buf
      (erase-buffer)
      (condition-case _ (gfm-mode)
        (error (text-mode)))
      (doom-template-insert "SUBMIT_BUG_REPORT")
      (goto-char (point-max))
      (let ((pos (point)))
        (save-excursion
          (insert
           "\n" (doom-info) "\n"
           (if (and backtrace (not (string-empty-p backtrace)))
               (format "\n<details>\n<summary>Backtrace</summary>\n\n```\n%s\n```\n</details>\n"
                       backtrace)
             "")))
        (local-set-key (kbd "C-c C-c") #'doom--open-bug-report)
        (local-set-key (kbd "C-c C-k") #'kill-this-buffer)
        (setq header-line-format "C-c C-c to submit / C-c C-k to close")
        ;;
        (narrow-to-region (point-min) pos)
        (goto-char (point-min)))
      (pop-to-buffer buf))))


;;; Profiling

(defvar doom--profiler nil)
;;;###autoload
(defun doom/toggle-profiler ()
  "Toggle the Emacs profiler. Run it again to see the profiling report."
  (interactive)
  (if (not doom--profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq doom--profiler (not doom--profiler)))

;;;###autoload
(defun doom/profile-emacs ()
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
    (let ((process-args
           (append `("*esup-child*"
                     "*esup-child*"
                     ,esup-emacs-path
                     "-Q"
                     "--eval=(setq after-init-time nil)"
                     "-L" ,esup-load-path)
                   (when (bound-and-true-p early-init-file)
                     `("-l" ,early-init-file))
                   `("-l" "esup-child"
                     ,(format "--eval=(let ((load-file-name \"%s\")) (esup-child-run \"%s\" \"%s\" %d))"
                              init-file
                              init-file
                              esup-server-port
                              esup-depth)
                     "--eval=(doom|run-all-startup-hooks)"))))
      (when esup-run-as-batch-p
        (setq process-args (append process-args '("--batch"))))
      (setq esup-child-process (apply #'start-process process-args)))
    (set-process-sentinel esup-child-process 'esup-child-process-sentinel)))

;;;###autoload
(advice-add #'esup :override #'doom/profile-emacs)
