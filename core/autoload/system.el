;;; system.el
(provide 'core-lib-system)

;;;###autoload
(defun doom-system-os ()
  "Returns the OS: arch, debian, macos, general linux, cygwin or windows."
  (let ((gnu-linux-p (eq system-type 'gnu/linux)))
    (cond ((and gnu-linux-p (file-exists-p "/etc/arch-release"))
           'arch)
          ((and gnu-linux-p (file-exists-p "/etc/debian_version"))
           'debian)
          (gnu-linux-p
           'linux)
          ((eq system-type 'darwin)
           'macos)
          ((memq system-type '(windows-nt cygwin))
           'windows)
          (t (error "Unknown OS: %s" system-type)))))

;;;###autoload
(defun doom-sh (&rest args)
  "Runs a shell command and prints any output to the DOOM buffer."
  (error "doom-sh not implemented yet"))

;;;###autoload
(defun doom-async-sh (&rest args)
  "Like `doom-sh', but runs command asynchronously."
  (error "doom-async-sh not implemented yet"))

;;;###autoload
(defun doom-sudo (&rest args)
  "Like `doom-sh', but runs as root (prompts for password)."
  (error "doom-sudo not implemented yet"))

;;;###autoload
(defun doom-fetch (fetcher location dest)
  "Clone a remote version-controlled repo at REPO-URL to PATH, if it exists.
Requires the corresponding client, e.g. git for git repos, hg for mercurial,
etc."
  (let* ((command (pcase fetcher
                    (:github "git clone --recursive https://github.com/%s.git")
                    (:git    "git clone --recursive %s")
                    (:gist   "git clone https://gist.github.com/%s.git")
                    (_ (error "%s is not a valid fetcher" fetcher))))
         (argv (s-split-up-to " " command 1))
         (args (format (car (cdr argv)) location))
         (bin (executable-find (car argv)))
         (fn (if noninteractive 'shell-command 'async-shell-command))
         buf)
    (unless bin
      (error "%s couldn't be found" command))
    (if (file-exists-p dest)
        (message "Resource already exists locally, skipping")
      (apply fn (format "%s %s '%s'" bin args dest) (unless noninteractive (list buf)))
      (if noninteractive
          (message "Cloning %s -> %s" location (file-relative-name dest doom-modules-dir))
        (doom-popup-buffer buf)
        (with-current-buffer buf
          (when (featurep 'evil)
            (evil-change-state 'normal))
          (set-buffer-modified-p nil))))))
