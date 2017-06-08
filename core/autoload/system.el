;;; core/autoload/system.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-system-os (&optional os)
  "Returns the OS: arch, debian, macos, general linux, cygwin or windows. If OS
is given, returns t if it matches the current system, and nil otherwise."
  (let* ((gnu-linux-p (eq system-type 'gnu/linux))
         (type (cond ((and gnu-linux-p (file-exists-p "/etc/arch-release"))
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
    (or (and os (eq os type))
        type)))

;;;###autoload
(defun doom-sh (command &rest args)
  "Runs a shell command and prints any output to the DOOM buffer."
  (let ((cmd-list (split-string command " ")))
    (cond ((equal (car cmd-list) "sudo")
           (apply #'doom-sudo (string-join (cdr cmd-list) " ") args))
          ((let ((bin (executable-find "npm")))
             (and (file-exists-p bin)
                  (not (file-writable-p bin))))
           (apply #'doom-sudo (string-join cmd-list " ") args))
          (t
           (princ (shell-command-to-string (apply #'format command args)))))))

(defvar tramp-verbose)
;;;###autoload
(defun doom-sudo (command &rest args)
  "Like `doom-sh', but runs as root (prompts for password)."
  (let ((tramp-verbose 2))
    (with-current-buffer (get-buffer-create "*doom-sudo*")
      (unless (string-prefix-p "/sudo::/" default-directory)
        (cd "/sudo::/"))
      (princ (shell-command-to-string (apply #'format command args))))))

;;;###autoload
(defun doom-fetch (fetcher location dest)
  "Clone a remote version-controlled repo at REPO-URL to PATH, if it exists.
Requires the corresponding client, e.g. git for git repos, hg for mercurial,
etc."
  (let* ((command (pcase fetcher
                    (:github "git clone --depth 1 --recursive https://github.com/%s.git")
                    (:git    "git clone --depth 1 --recursive %s")
                    (:gist   "git clone https://gist.github.com/%s.git")
                    ;; TODO Add hg
                    (_ (error "%s is not a valid fetcher" fetcher))))
         (argv (split-string command " " t))
         (args (format (string-join (cdr argv) " ") location))
         (bin (executable-find (car argv)))
         (dest (expand-file-name dest)))
    (unless bin
      (error "%s couldn't be found" command))
    (unless (file-directory-p dest)
      (funcall (if noninteractive
                   (lambda (&rest args) (princ (shell-command-to-string args)))
                 #'async-shell-command)
               (format "%s %s %s" bin args (shell-quote-argument dest)))
      (message! "Cloning %s -> %s" location dest))))

