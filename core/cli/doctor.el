;;; core/cli/doctor.el -*- lexical-binding: t; -*-

(defvar doom-warnings ())
(defvar doom-errors ())

;;; Helpers
(defun elc-check-dir (dir)
  (dolist (file (directory-files-recursively dir "\\.elc$"))
    (when (file-newer-than-file-p (concat (file-name-sans-extension file) ".el")
                                  file)
      (warn! "%s is out-of-date" (abbreviate-file-name file)))))

(defmacro assert! (condition message &rest args)
  `(unless ,condition
     (error! ,message ,@args)))


;;; Logging
(defmacro error!   (&rest args)
  `(progn (unless inhibit-message (print! (error ,@args)))
          (push (format! (error ,@args)) doom-errors)))
(defmacro warn!    (&rest args)
  `(progn (unless inhibit-message (print! (warn ,@args)))
          (push (format! (warn ,@args)) doom-warnings)))
(defmacro success! (&rest args)
  `(print! (green ,@args)))
(defmacro section! (&rest args)
  `(print! (bold (blue ,@args))))

(defmacro explain! (&rest args)
  `(print-group! (print! (autofill ,@args))))


;;
;;; CLI commands

(defcli! (doctor doc) ()
  "Diagnoses common issues on your system.

The Doom doctor is essentially one big, self-contained elisp shell script that
uses a series of simple heuristics to diagnose common issues on your system.
Issues that could intefere with Doom Emacs.

Doom modules may optionally have a doctor.el file to run their own heuristics
in."
  :bare t
  (print! "The doctor will see you now...\n")

  ;; REVIEW Refactor me
  (print! (start "Checking your Emacs version..."))
  (when EMACS27+
    (warn! "Emacs %s detected. Emacs HEAD is unstable and may cause errors."
           emacs-version))

  (print! (start "Checking for Emacs config conflicts..."))
  (when (file-exists-p "~/.emacs")
    (warn! "Detected an ~/.emacs file, which may prevent Doom from loading")
    (explain! "If Emacs finds an ~/.emacs file, it will ignore ~/.emacs.d, where Doom is "
              "typically installed. If you're seeing a vanilla Emacs splash screen, this "
              "may explain why. If you use Chemacs, you may ignore this warning."))

  (print! (start "Checking for private config conflicts..."))
  (let ((xdg-dir (concat (or (getenv "XDG_CONFIG_HOME")
                             "~/.config")
                         "/doom/"))
        (doom-dir (or (getenv "DOOMDIR")
                      "~/.doom.d/")))
    (when (and (not (file-equal-p xdg-dir doom-dir))
               (file-directory-p xdg-dir)
               (file-directory-p doom-dir))
      (print! (warn "Detected two private configs, in %s and %s")
              (abbreviate-file-name xdg-dir)
              doom-dir)
      (explain! "The second directory will be ignored, as it has lower precedence.")))

  (print! (start "Checking for stale elc files..."))
  (elc-check-dir user-emacs-directory)

  (print! (start "Checking Doom Emacs..."))
  (condition-case-unless-debug ex
      (print-group!
       (let ((doom-interactive-mode 'doctor))
         (doom-initialize 'force)
         (doom-initialize-core)
         (doom-initialize-modules))

       (print! (success "Initialized Doom Emacs %s") doom-version)
       (print!
        (if (hash-table-p doom-modules)
            (success "Detected %d modules" (hash-table-count doom-modules))
          (warn "Failed to load any modules. Do you have an private init.el?")))

       (print! (success "Detected %d packages") (length doom-packages))

       (print! (start "Checking Doom core for irregularities..."))
       (print-group!
        ;; Check for oversized problem files in cache that may cause unusual/tremendous
        ;; delays or freezing. This shouldn't happen often.
        (dolist (file (list "savehist" "projectile.cache"))
          (when-let (size (ignore-errors (doom-file-size file doom-cache-dir)))
            (when (> size 1048576) ; larger than 1mb
              (warn! "%s is too large (%.02fmb). This may cause freezes or odd startup delays"
                     file (/ size 1024))
              (explain! "Consider deleting it from your system (manually)"))))

        (unless (ignore-errors (executable-find doom-projectile-fd-binary))
          (warn! "Couldn't find the `fd' binary; project file searches will be slightly slower")
          (unless (executable-find "rg")
            (warn! "Couldn't find the `rg' binary either; project file searches will be even slower")))

        (require 'projectile)
        (when (projectile-project-root "~")
          (warn! "Your $HOME is recognized as a project root")
          (explain! "Doom will disable bottom-up root search, which may reduce the accuracy of project\n"
                    "detection."))

        ;; There should only be one
        (when (and (file-equal-p doom-private-dir "~/.config/doom")
                   (file-directory-p "~/.doom.d"))
          (print! (warn "Both %S and '~/.doom.d' exist on your system")
                  (path doom-private-dir))
          (explain! "Doom will only load one of these (~/.config/doom takes precedence). Possessing\n"
                    "both is rarely intentional; you should one or the other."))

        ;; Check for fonts
        (if (not (executable-find "fc-list"))
            (warn! "Warning: unable to detect fonts because fontconfig isn't installed")
          ;; all-the-icons fonts
          (when (and (pcase system-type
                       (`gnu/linux (concat (or (getenv "XDG_DATA_HOME")
                                               "~/.local/share")
                                           "/fonts/"))
                       (`darwin "~/Library/Fonts/"))
                     (require 'all-the-icons nil t))
            (with-temp-buffer
              (insert (cdr (doom-call-process "fc-list" "" "file")))
              (dolist (font all-the-icons-font-names)
                (if (save-excursion (re-search-backward font nil t))
                    (success! "Found font %s" font)
                  (print! (warn "Warning: couldn't find %S font") font)
                  (explain! "You can install it by running `M-x all-the-icons-install-fonts' within Emacs.\n\n"
                            "This could also mean you've installed them in non-standard locations, in which "
                            "case feel free to ignore this warning.")))))))

       (print! (start "Checking for stale elc files in your DOOMDIR..."))
       (when (file-directory-p doom-private-dir)
         (print-group!
          (elc-check-dir doom-private-dir)))

       (when doom-modules
         (print! (start "Checking your enabled modules..."))
         (advice-add #'require :around #'doom-shut-up-a)
         (maphash (lambda (key plist)
                    (let (doom-local-errors
                          doom-local-warnings)
                      (let (doom-errors
                            doom-warnings)
                        (condition-case-unless-debug ex
                            (let ((doctor-file   (doom-module-path (car key) (cdr key) "doctor.el"))
                                  (packages-file (doom-module-path (car key) (cdr key) "packages.el")))
                              (cl-loop for name in (let (doom-packages
                                                         doom-disabled-packages)
                                                     (load packages-file 'noerror 'nomessage)
                                                     (mapcar #'car doom-packages))
                                       unless (or (doom-package-get name :disable)
                                                  (eval (doom-package-get name :ignore))
                                                  (doom-package-built-in-p name)
                                                  (doom-package-installed-p name))
                                       do (print! (error "%s is not installed") name))
                              (let ((inhibit-message t))
                                (load doctor-file 'noerror 'nomessage)))
                          (file-missing (error! "%s" (error-message-string ex)))
                          (error (error! "Syntax error: %s" ex)))
                        (when (or doom-errors doom-warnings)
                          (print-group!
                           (print! (start (bold "%s %s")) (car key) (cdr key))
                           (print! "%s" (string-join (append doom-errors doom-warnings) "\n")))
                          (setq doom-local-errors doom-errors
                                doom-local-warnings doom-warnings)))
                      (appendq! doom-errors doom-local-errors)
                      (appendq! doom-warnings doom-local-warnings)))
                  doom-modules)))
    (error
     (warn! "Attempt to load DOOM failed\n  %s\n"
            (or (cdr-safe ex) (car ex)))
     (setq doom-modules nil)))

  ;; Final report
  (message "")
  (dolist (msg (list (list doom-errors "error" 'red)
                     (list doom-warnings "warning" 'yellow)))
    (when (car msg)
      (print! (color (nth 2 msg)
                     (if (cdr msg)
                         "There are %d %ss!"
                       "There is %d %s!")
                     (length (car msg)) (nth 1 msg)))))
  (unless (or doom-errors doom-warnings)
    (success! "Everything seems fine, happy Emacs'ing!"))
  t)
