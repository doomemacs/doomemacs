;;; lisp/lib/system.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-system-distro ()
  "Return a symbol representing the installed distro."
  (with-memoization (get 'doom-system-distro 'cached-value)
    (cond (doom--system-windows-p 'windows)
          (doom--system-macos-p     'macos)
          ((ignore-errors
             (with-file-contents! "/etc/os-release"
               (when (re-search-forward "^ID=\"?\\([^\"\n]+\\)\"?" nil t)
                 (intern (downcase (match-string 1)))))))
          ;; A few redundancies in case os-release fails us
          ((file-exists-p "/etc/debian_version")
           'debian)
          ((executable-find "nixos-version")
           'nixos)
          ((and (or (file-exists-p "/etc/config.scm")
                    (file-directory-p "/run/current-system"))
                (executable-find "guix"))
           'guix)
          ('linux))))

;;;###autoload
(defun doom-system-distro-version ()
  "Return a distro name and version string."
  (letf! (defun sh (&rest args) (cdr (apply #'doom-call-process args)))
    (let ((distro (doom-system-distro)))
      (cond
       ((eq distro 'windows)
        (format "Windows %s" "Unknown")) ; TODO
       ((eq distro 'macos)
        (format "MacOS %s" (sh "sw_vers" "-productVersion")))
       ((executable-find "nixos-version")
        (format "NixOS %s" (sh "nixos-version")))
       ((executable-find "lsb_release")
        (sh "lsb_release" "-s" "-d"))
       ((ignore-errors
          (with-file-contents! "/etc/os-release"
            (when (re-search-forward "^PRETTY_NAME=\"?\\([^\"\n]+\\)\"?" nil t)
              (match-string 1)))))
       ((when-let (files (doom-glob "/etc/*-release"))
          (truncate-string-to-width
           (replace-regexp-in-string
            "\n" " " (doom-file-read (car files) :end 73) nil t)
           64 nil nil "...")))
       ((concat "Unknown " (sh "uname" "-v")))))))

;;;###autoload
(defun doom-system-distro-icon ()
  "Display icon for the installed distro."
  (with-memoization (get 'doom-system-distro-icon 'cached-value)
    (propertize
     (pcase (doom-system-distro)
       (`windows (nerd-icons-faicon "nf-fa-windows"))
       (`macos (nerd-icons-faicon "nf-fa-apple"))
       (`arch "\uF303")
       (`debian "\uF306")
       (`raspbian "\uF315")
       (`ubuntu "\uF31b")
       (`elementary "\uF309")
       (`fedora "\uF30a")
       (`coreos "\uF305")
       (`gentoo "\uF30d")
       (`mageia "\uF310")
       (`centos "\uF304")
       ((or `opensuse `tumbleweed) "\uF314")
       (`sabayon "\uF317")
       (`slackware "\uF319")
       (`linuxmint "\uF30e")
       (`alpine "\uF300")
       (`aosc "\uF301")
       (`nixos "\uF313")
       (`devuan "\uF307")
       (`manjaro "\uF312")
       ((or `void `artix) "\uF17c")
       (_ (nerd-icons-faicon "nf-fa-linux")))
     'face '(:height 1)
     'display '(raise 0))))

;;;###autoload
(defun doom-system-cpus ()
  "Return the max number of processing units on this system.
Tries to be portable. Returns 1 if cannot be determined."
  ;; REVIEW: Replace with `num-processors' once 27.x support is dropped.
  (with-memoization (get 'doom-system-cpus 'cached-value)
    (if (fboundp 'num-processors)
        (num-processors) ; added in Emacs 28.1
      (let ((cpus
             (cond ((fboundp 'w32-get-nproc)
                    (w32-get-nproc))
                   ((getenv "NUMBER_OF_PROCESSORS"))
                   ((executable-find "nproc")
                    (doom-call-process "nproc"))
                   ((executable-find "sysctl")
                    (doom-call-process "sysctl" "-n" "hw.ncpu")))))
        (max
         1 (or (cl-typecase cpus
                 (integer cpus)
                 (string
                  (condition-case _
                      (string-to-number cpus)
                    (wrong-type-argument
                     (user-error "NUMBER_OF_PROCESSORS contains an invalid value: %S"
                                 cpus))))
                 (cons
                  (if (zerop (car cpus))
                      (string-to-number (cdr cpus))
                    (user-error "Failed to look up number of processors, because:\n\n%s"
                                (cdr cpus)))))
               1))))))

(provide 'doom-lib '(system))
;;; system.el ends here
