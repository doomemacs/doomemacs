;;; lisp/cli/doctor.el --- userland heuristics and Emacs diagnostics -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar doom-doctor--warnings ())
(defvar doom-doctor--errors ())


;;
;;; DSL

(defun elc-check-dir (dir)
  (dolist (file (directory-files-recursively dir "\\.elc$"))
    (when (file-newer-than-file-p (concat (file-name-sans-extension file) ".el")
                                  file)
      (warn! "%s is out-of-date" (abbreviate-file-name file)))))

(defmacro assert! (condition message &rest args)
  `(unless ,condition
     (error! ,message ,@args)))

(defmacro error!   (&rest args)
  `(progn (unless inhibit-message (print! (error ,@args)))
          (push (format! (error ,@args)) doom-doctor--errors)))

(defmacro warn!    (&rest args)
  `(progn (unless inhibit-message (print! (warn ,@args)))
          (push (format! (warn ,@args)) doom-doctor--warnings)))

(defmacro success! (&rest args)
  `(print! (green ,@args)))

(defmacro section! (&rest args)
  `(print! (bold (blue ,@args))))

(defmacro explain! (&rest args)
  `(print-group! (print! (p ,@args))))


;;
;;; CLI commands

(defcli! ((doctor doc)) ()
  "Diagnoses common issues on your system.

The Doom doctor is essentially one big, self-contained elisp shell script that
uses a series of simple heuristics to diagnose common issues on your system.
Issues that could intefere with Doom Emacs.

Doom modules may optionally have a doctor.el file to run their own heuristics
in."
  :benchmark nil
  (print! "The doctor will see you now...\n")

  (print! (start "Checking your Emacs version..."))
  (print-group!
    (cond ((or (> emacs-major-version 30)
               (string-match-p ".\\([56]0\\|9[0-9]\\)$" emacs-version))
           (warn! "Detected a development version of Emacs (%s)" emacs-version)
           (if (> emacs-major-version 30)
               (explain! "This is the bleeding edge of Emacs. As it is constantly changing, Doom will not "
                         "(officially) support it. If you've found a stable commit, great! But be cautious "
                         "about updating Emacs too eagerly!\n")
             (explain! "A version that ends in .50, .60, or .9X indicates a build of Emacs in between "
                       "stable releases (i.e. development builds). Doom does not support these well.\n"))
           (explain! "Because development (or bleeding edge) builds are prone to random breakage, "
                     "there will be a greater burden on you to investigate and deal with issues. "
                     "Please make extra sure that your issue is reproducible on a stable version "
                     "(between 27.1 and 30.1) before reporting them to Doom's issue tracker!\n"
                     "\n"
                     "If this doesn't phase you, read the \"Why does Doom not support Emacs HEAD\" QnA "
                     "in Doom's FAQ. It offers some advice for debugging and surviving issues on the "
                     "bleeding edge. Failing that, the latest stable release of Emacs will always be "
                     "Doom's best supported version of Emacs."))
          ((= emacs-major-version 27)
           (warn! "Emacs 27 is supported, but not for long!")
           (explain! "Doom will drop 27.x support sometime mid-2025. It's recommended that you upgrade "
                     "to the latest stable release (currently 30.1). It is better supported, faster, and "
                     "more stable.")))

    (when (and (version= emacs-version "29.4") (featurep 'pgtk))
      (warn! "Detected emacs-pgtk 29.4!")
      (explain! "If you are experiencing segfaults (crashes), consider downgrading to 29.3 or "
                "upgrading to 30.1+. A known bug in 29.4 causes intermittent crashes. "
                "See doomemacs#7915 for details.")))

  (print! (start "Checking for Doom's prerequisites..."))
  (print-group!
    (if (not (executable-find "git"))
        (error! "Couldn't find git on your machine! Doom's package manager won't work.")
      (save-match-data
        (let* ((version
                (cdr (doom-call-process "git" "version")))
               (version
                (and (string-match "git version \\([0-9]+\\(?:\\.[0-9]+\\)\\{2\\}\\)" version)
                     (match-string 1 version))))
          (if version
              (when (version< version "2.23")
                (error! "Git %s detected! Doom requires git 2.23 or newer!"
                        version))
            (warn! "Cannot determine Git version. Doom requires git 2.23 or newer!")))))

    (unless (executable-find "rg")
      (error! "Couldn't find the `rg' binary; this a hard dependecy for Doom, file searches may not work at all")))

  (print! (start "Checking for Emacs config conflicts..."))
  (print-group!
    (unless (or (file-equal-p doom-emacs-dir "~/.emacs.d")
                (file-equal-p doom-emacs-dir "~/.config/emacs"))
      (print! (warn "Doom is installed in a non-standard location"))
      (explain! "The standard locations are ~/.config/emacs or ~/.emacs.d. Emacs will fail "
                "to load Doom if it is not explicitly told where to look for it. In Emacs 29+, "
                "this is possible with the --init-directory option:\n\n"
                "  $ emacs --init-directory '" (abbreviate-file-name doom-emacs-dir) "'\n\n"
                "However, Emacs 27-28 users have no choice but to move Doom to a standard "
                "location.\n\n"
                "Chemacs users may ignore this warning, however."))
    (let (found?)
      (dolist (file '("~/_emacs" "~/.emacs" "~/.emacs.el" "~/.emacs.d" "~/.config/emacs"))
        (when (and (file-exists-p file)
                   (not (file-equal-p file doom-emacs-dir)))
          (setq found? t)
          (print! (warn "Found another Emacs config: %s (%s)")
                  file (if (file-directory-p file) "directory" "file"))))
      (when found?
        (explain! "Having multiple Emacs configs may prevent Doom from loading properly. Emacs "
                  "will load the first it finds and ignore the rest. If Doom isn't starting up "
                  "correctly (e.g. you get a vanilla splash screen), make sure that only one of "
                  "these exist.\n\n"
                  "Chemacs users may ignore this warning."))))

  (print! (start "Checking for missing Emacs features..."))
  (print-group!
    (unless (functionp 'json-serialize)
      (warn! "Emacs was not built with native JSON support")
      (explain! "Users will see a substantial performance gain by building Emacs with "
                "jansson support (i.e. a native JSON library), particularly LSP users. "
                "You must install a prebuilt Emacs binary with this included, or compile "
                "Emacs with the --with-json option."))
    (unless (featurep 'native-compile)
      (warn! "Emacs was not built with native compilation support")
      (explain! "Users will see a substantial performance gain by building Emacs with "
                "native compilation support, availible in emacs 28+."
                "You must install a prebuilt Emacs binary with this included, or compile "
                "Emacs with the --with-native-compilation option.")))

  (print! (start "Checking for private config conflicts..."))
  (print-group!
    (let* ((xdg-dir (concat (or (getenv "XDG_CONFIG_HOME")
                                "~/.config")
                            "/doom/"))
           (doom-dir (or (getenv "DOOMDIR")
                         "~/.doom.d/"))
           (dir (if (file-directory-p xdg-dir)
                    xdg-dir
                  doom-dir)))
      (when (file-equal-p dir doom-emacs-dir)
        (print! (error "Doom was cloned to %S, not ~/.emacs.d or ~/.config/emacs"
                       (path dir)))
        (explain! "Doom's source and your private Doom config have to live in separate directories. "
                  "Putting them in the same directory (without changing the DOOMDIR environment "
                  "variable) will cause errors on startup."))
      (when (and (not (file-equal-p xdg-dir doom-dir))
                 (file-directory-p xdg-dir)
                 (file-directory-p doom-dir))
        (print! (warn "Detected two private configs, in %s and %s")
                (abbreviate-file-name xdg-dir)
                doom-dir)
        (explain! "The second directory will be ignored, as it has lower precedence."))))

  (print! (start "Checking for common environmental issues..."))
  (print-group!
    (when (or (string-match-p "/fish$" shell-file-name)
              (string-match-p "/nu\\(?:\\.exe\\)?$" shell-file-name))
      (print! (warn "Detected a non-POSIX $SHELL"))
      (explain! "Non-POSIX shells (particularly Fish and Nushell) can cause unpredictable issues "
                "with any Emacs utilities that spawn child processes from shell commands (like "
                "diff-hl and in-Emacs terminals). To get around this, configure Emacs to use a "
                "POSIX shell internally, e.g.\n\n"
                "  ;;; add to $DOOMDIR/config.el:\n"
                "  (setq shell-file-name (executable-find \"bash\"))\n\n"
                "Emacs' terminal emulators can be safely configured to use your original $SHELL:\n\n"
                "  ;;; add to $DOOMDIR/config.el:\n"
                (format "  (setq-default vterm-shell \"%s\")\n" shell-file-name)
                (format "  (setq-default explicit-shell-file-name \"%s\")\n" shell-file-name)))

    (condition-case e
        (when (featurep :system 'windows)
          (let ((filea (expand-file-name "__testfile1" temporary-file-directory))
                (fileb (expand-file-name "__testfile2" temporary-file-directory)))
            (unwind-protect
                (progn
                  (with-temp-file fileb)
                  (make-symbolic-link fileb filea)
                  (not (file-symlink-p filea)))
              (delete-file filea)
              (delete-file fileb))))
      ('file-error
       (when (equal (cons (nth 1 e) (nth 2 e))
                    (cons "Making symbolic link" "Operation not permitted"))
         (print! (warn "Symlinks are not enabled on this operating system"))
         (explain! "In the near future, Doom will make extensive use of symlinks to save space "
                   "and simplify package and profile management. Without symlinks, much of it "
                   "won't be functional. To get around this, you have three options:"
                   "\n\n"
                   "  - Enabling 'Developer Mode' in the Windows settings (search for 'Developer "
                   "    Settings' in the start menu). This will warn you about its effect on system "
                   "    security, but this can be ignored. If it bothers you, consider another option "
                   "    below.\n"
                   "  - Running your shell (cmd or powershell) in administrator mode anytime you "
                   "    need to use the 'doom' script. Also, the `doom/reload' command won't work "
                   "    unless Emacs itself is launched in administrator mode.\n"
                   "  - Install Emacs in WSL 1/2; the native Linux environment it creates supports "
                   "    symlinks out of the box and is the best option (as Emacs is generally more "
                   "    stable, predictable, and faster there).\n\n")))))

  (print! (start "Checking for stale elc files..."))
  (elc-check-dir doom-core-dir)
  (elc-check-dir doom-modules-dir)
  (elc-check-dir (doom-path doom-local-dir "straight" straight-build-dir))

  (print! (start "Checking for problematic git global settings..."))
  (if (executable-find "git")
      (when (zerop (car (doom-call-process "git" "config" "--global" "--get-regexp" "^url\\.git://github\\.com")))
        (warn! "Detected insteadOf rules in your global gitconfig.")
        (explain! "Doom's package manager heavily relies on git. In particular, many of its packages "
                  "are hosted on github. Rewrite rules like these will break it:\n\n"
                  "  [url \"git://github.com\"]\n"
                  "  insteadOf = https://github.com\n\n"
                  "Please remove them from your gitconfig or use a conditional includeIf rule to "
                  "only apply your rewrites to specific repositories. See "
                  "'https://git-scm.com/docs/git-config#_includes' for more information."))
    (error! "Couldn't find the `git' binary; this a hard dependecy for Doom!"))

  (print! (start "Checking Doom Emacs..."))
  (condition-case-unless-debug ex
      (print-group!
        (doom-initialize t)
        (doom-startup)
        (require 'straight)

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
                       file (/ size 1024 1024.0))
                (explain! "Consider deleting it from your system (manually)"))))

          (unless (ignore-errors (executable-find doom-fd-executable))
            (warn! "Couldn't find the `fd' binary; project file searches will be slightly slower"))

          (require 'projectile)
          (when (projectile-project-root "~")
            (warn! "Your $HOME is recognized as a project root")
            (explain! "Emacs will assume $HOME is the root of any project living under $HOME. If this isn't\n"
                      "desired, you will need to remove \".git\" from `projectile-project-root-files-bottom-up'\n"
                      "(a variable), e.g.\n\n"
                      "  (after! projectile\n"
                      "    (setq projectile-project-root-files-bottom-up\n"
                      "          (remove \".git\" projectile-project-root-files-bottom-up)))"))

          ;; There should only be one
          (when (and (file-equal-p doom-user-dir "~/.config/doom")
                     (file-directory-p "~/.doom.d"))
            (print! (warn "Both %S and '~/.doom.d' exist on your system")
                    (path doom-user-dir))
            (explain! "Doom will only load one of these (~/.config/doom takes precedence). Possessing\n"
                      "both is rarely intentional; you should one or the other."))

          ;; Check for fonts
          (if (not (executable-find "fc-list"))
              (warn! "Warning: unable to detect fonts because fontconfig isn't installed")
            ;; nerd-icons fonts
            (when (and (pcase system-type
                         (`gnu/linux (concat (or (getenv "XDG_DATA_HOME")
                                                 "~/.local/share")
                                             "/fonts/"))
                         (`darwin "~/Library/Fonts/"))
                       (require 'nerd-icons nil t))
              (with-temp-buffer
                (cl-destructuring-bind (status . output)
                    (doom-call-process "fc-list" "" "family")
                  (if (not (zerop status))
                      (print! (error "There was an error running `fc-list'. Is fontconfig installed correctly?"))
                    (insert output)
                    (if (re-search-backward nerd-icons-font-family nil t)
                        (success! "Found %s" nerd-icons-font-family)
                      (print! (warn "Failed to locate '%s' font on your system") nerd-icons-font-family)
                      (explain! "This font is required for icons in Doom Emacs. To download and install "
                                "them, do one of the following:\n\n"
                                "  - Execute `M-x nerd-icons-install-fonts' from within Doom Emacs (NOTE: "
                                "    on Windows this command will only download them; the fonts must then "
                                "    be installed manually afterwards).\n"
                                "  - Download and install 'Symbols Nerd Font' from https://nerdfonts.com "
                                "    or via your OS package manager. (You'll need to change the "
                                "    `nerd-icons-font-names' and/or `nerd-icons-font-family' variables to "
                                "    reflect a non-standard file or font family name).\n"))))))))

        (print! (start "Checking for stale elc files in your DOOMDIR..."))
        (when (file-directory-p doom-user-dir)
          (print-group!
            (elc-check-dir doom-user-dir)))

        (when doom-modules
          (print! (start "Checking your enabled modules..."))
          (advice-add #'require :around #'doom-shut-up-a)
          (pcase-dolist (`(,group . ,name) (doom-module-list))
            (with-doom-context 'doctor
              (let (doom-local-errors
                    doom-local-warnings)
                (let (doom-doctor--errors
                      doom-doctor--warnings)
                  (condition-case-unless-debug ex
                      (with-doom-module (cons group name)
                        (let ((doctor-file   (doom-module-expand-path (cons group name) "doctor.el"))
                              (packages-file (doom-module-expand-path (cons group name) doom-module-packages-file)))
                          (when packages-file
                            (cl-loop with doom-output-indent = 6
                                     for name in (with-doom-context 'package
                                                   (let* (doom-packages
                                                          doom-disabled-packages)
                                                     (load packages-file 'noerror 'nomessage)
                                                     (mapcar #'car doom-packages)))
                                     unless (or (doom-package-get name :disable)
                                                (eval (doom-package-get name :ignore))
                                                (plist-member (doom-package-get name :recipe) :local-repo)
                                                (locate-library (symbol-name name))
                                                (doom-package-built-in-p name)
                                                (doom-package-installed-p name))
                                     do (print! (error "Missing emacs package: %S") name)))
                          (when doctor-file
                            (let ((inhibit-message t))
                              (load doctor-file 'noerror 'nomessage)))))
                    (file-missing (error! "%s" (error-message-string ex)))
                    (error (error! "Syntax error: %s" ex)))
                  (when (or doom-doctor--errors doom-doctor--warnings)
                    (print-group!
                      (print! (start (bold "%s %s")) group name)
                      (print! "%s" (string-join (append doom-doctor--errors doom-doctor--warnings) "\n")))
                    (setq doom-local-errors doom-doctor--errors
                          doom-local-warnings doom-doctor--warnings)))
                (cl-callf append doom-doctor--errors doom-local-errors)
                (cl-callf append doom-doctor--warnings doom-local-warnings))))))
    (error
     (warn! "Attempt to load DOOM failed\n  %s\n"
            (or (cdr-safe ex) (car ex)))
     (setq doom-modules nil)))

  ;; Final report
  (terpri)
  (dolist (msg (list (list doom-doctor--warnings "warning" 'yellow)
                     (list doom-doctor--errors "error" 'red)))
    (when (car msg)
      (print! (color (nth 2 msg)
                     (if (cdar msg)
                         "There are %d %ss!"
                       "There is %d %s!")
                     (length (car msg)) (nth 1 msg)))))
  (unless (or doom-doctor--errors doom-doctor--warnings)
    (success! "Everything seems fine, happy Emacs'ing!"))
  (exit! :pager? "+G"))

(provide 'doom-cli-doctor)
;;; doctor.el ends here
