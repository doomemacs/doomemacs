;;; lisp/doom-straight.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Emacs package management is opinionated, and so is Doom. Doom uses `straight'
;; to create a declarative, lazy-loaded, and (nominally) reproducible package
;; management system. We use `straight' over `package' because the latter is
;; tempermental. ELPA sources suffer downtime occasionally and often fail to
;; build packages when GNU Tar is unavailable (e.g. MacOS users start with BSD
;; tar). Known gnutls errors plague the current stable release of Emacs (26.x)
;; which bork TLS handshakes with ELPA repos (mainly gnu.elpa.org). See
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=3434.
;;
;; What's worse, you can only get the latest version of packages through ELPA.
;; In an ecosystem that is constantly changing, this is more frustrating than
;; convenient. Straight (and Doom) can do rolling release, but it is opt-in.
;;
;; Interacting with this package management system is done through Doom's
;; bin/doom script. Find out more about it by running 'doom help' (I highly
;; recommend you add the script to your PATH). Here are some highlights:
;;
;; - `doom install`: a wizard that guides you through setting up Doom and your
;;   private config for the first time.
;; - `doom sync`: your go-to command for making sure Doom is in optimal
;;   condition. It ensures all unneeded packages are removed, all needed ones
;;   are installed, and all metadata associated with them is generated.
;; - `doom upgrade`: upgrades Doom Emacs and your packages to the latest
;;   versions. There's also 'bin/doom sync -u' for updating only your packages.
;;
;; How this works is: the system reads packages.el files located in each
;; activated module, your private config (`doom-user-dir'), and one in
;; `doom-core-dir'. These contain `package!' declarations that tell DOOM what
;; packages to install and where from.
;;
;; All that said, you can still use package.el's commands, but 'doom sync' will
;; purge ELPA packages.
;;
;;; Code:

(setq straight-base-dir (file-truename doom-local-dir)
      straight-repository-branch "develop"
      ;; Since byte-code is rarely compatible across different versions of
      ;; Emacs, it's best we build them in separate directories, per emacs
      ;; version.
      straight-build-dir (format "build-%s" emacs-version)
      straight-cache-autoloads nil ; we already do this, and better.
      ;; Doom doesn't encourage you to modify packages in place. Disabling this
      ;; makes 'doom sync' instant (once everything set up), which is much nicer
      ;; UX than the several seconds modification checks.
      straight-check-for-modifications nil
      ;; We handle package.el ourselves (and a little more comprehensively)
      straight-enable-package-integration nil
      ;; Before switching to straight, `doom-local-dir' would average out at
      ;; around 100mb with half Doom's modules at ~230 packages. Afterwards, at
      ;; around 1gb. With shallow cloning, that is reduced to ~400mb. This has
      ;; no affect on packages that are pinned, however (run 'doom sync --gc' to
      ;; compact those after-the-fact). Some packages break when shallow cloned
      ;; (like magit and org), but we'll deal with that elsewhere.
      straight-vc-git-default-clone-depth '(1 single-branch))

(with-eval-after-load 'straight
  ;; HACK: Doom relies on deferred compilation, which spares the user 20-50min
  ;;   of compilation at install time, but subjects them to ~50% CPU activity
  ;;   when starting Emacs for the first time. To complete this, straight.el
  ;;   needs to be told not to do native-compilation, but it won't obey
  ;;   `straight-disable-native-compile'.
  ;;
  ;;   It *will* obey `straight--native-comp-available', though. Trouble is:
  ;;   it's a constant; it resets itself when straight is loaded, so it must be
  ;;   changed afterwards.
  (setq straight--native-comp-available nil)
  ;; `let-alist' is built into Emacs 26 and onwards
  (add-to-list 'straight-built-in-pseudo-packages 'let-alist))

(defadvice! doom--read-pinned-packages-a (fn &rest args)
  "Read `:pin's in `doom-packages' on top of straight's lockfiles."
  :around #'straight--lockfile-read-all
  (append (apply fn args) ; lockfiles still take priority
          (doom-package-pinned-alist)))

;; HACK: This fixes an issue introduced in emacs-mirror/emacs@0d383b592c2f and
;;   is present in >=29: Straight.el uses `loaddefs-generate' if it is
;;   available, which activates `emacs-lisp-mode' to read autoloads files, but
;;   does so without suppressing its hooks. Some packages (like overseer) add
;;   hooks to `emacs-lisp-mode-hook' in their autoloads, and once triggered,
;;   they will try to load their dependencies (like dash or pkg-info), causing
;;   file errors.
;; REVIEW: Report this upstream.
(defadvice! doom--fix-loaddefs-generate--parse-file-a (fn &rest args)
  :around #'loaddefs-generate--parse-file
  (let (emacs-lisp-mode-hook)
    (apply fn args)))


;;
;;; Hacks

;; Straight was designed primarily for interactive use, in an interactive Emacs
;; session, but Doom does its package management in the terminal. Some things
;; must be modified get straight to behave and improve its UX for our users.

(defvar doom-straight--auto-options
  '(("has diverged from"
     . "^Reset [^ ]+ to ")
    ("but recipe specifies a URL of"
     . "Delete remote \"[^\"]+\", re-create it with correct URL")
    ("has a merge conflict:"
     . "^Abort merge$")
    ("has a dirty worktree:"
     . "^Discard changes$")
    ("^In repository \"[^\"]+\", [^ ]+ (on branch \"main\") is ahead of default branch \"master\""
     . "^Checkout branch \"master\"")
    ("^In repository \"[^\"]+\", [^ ]+ (on branch \"[^\"]+\") is ahead of default branch \"[^\"]+\""
     . "^Checkout branch \"")
    ("^In repository "
     . "^Reset branch \\|^Delete remote [^,]+, re-create it with correct URL\\|^Checkout \""))
  "A list of regexps, mapped to regexps.

Their CAR is tested against the prompt, and CDR is tested against the presented
option, and is used by `straight-vc-git--popup-raw' to select which option to
recommend.

It may not be obvious to users what they should do for some straight prompts,
so Doom will recommend the one that reverts a package back to its (or target)
original state.")

;; HACK Remove dired & magit options from prompt, since they're inaccessible in
;;      noninteractive sessions.
(advice-add #'straight-vc-git--popup-raw :override #'straight--popup-raw)

;; HACK: `native-comp' only respects `native-comp-jit-compilation-deny-list'
;;   when native-compiling packages in interactive sessions. It ignores the
;;   variable when, say, straight is building packages. This advice forces it to
;;   obey it, even when used by straight (but only in the CLI).
(defadvice! doom-straight--native--compile-async-skip-p (fn files &optional recursively load selector)
  :around #'native-compile-async
  (let (file-list)
    (dolist (file-or-dir (ensure-list files))
      (cond ((file-directory-p file-or-dir)
             (dolist (file (if recursively
                               (directory-files-recursively
                                file-or-dir comp-valid-source-re)
                             (directory-files file-or-dir
                                              t comp-valid-source-re)))
               (push file file-list)))
            ((file-exists-p file-or-dir)
             (push file-or-dir file-list))
            ((signal 'native-compiler-error
                     (list "Not a file nor directory" file-or-dir)))))
    (funcall fn (seq-remove (lambda (file)
                              (seq-some (lambda (re) (string-match-p re file))
                                        native-comp-deferred-compilation-deny-list))
                            file-list)
             recursively load selector)))

;; HACK Replace GUI popup prompts (which hang indefinitely in tty Emacs) with
;;      simple prompts.
(defadvice! doom-straight--fallback-to-y-or-n-prompt-a (fn &optional prompt noprompt?)
  :around #'straight-are-you-sure
  (or noprompt?
      (if noninteractive
          (y-or-n-p (format! "%s" (or prompt "")))
        (funcall fn prompt))))

(defun doom-straight--recommended-option-p (prompt option)
  (cl-loop for (prompt-re . opt-re) in doom-straight--auto-options
           if (string-match-p prompt-re prompt)
           return (string-match-p opt-re option)))

(defadvice! doom-straight--no-compute-prefixes-a (fn &rest args)
  :around #'straight--build-autoloads
  (eval-when-compile
    (or (require 'loaddefs-gen nil 'noerror)
        (require 'autoload)))
  (let (autoload-compute-prefixes)
    (apply fn args)))

(defadvice! doom-straight--suppress-confirm-a (&rest _)
  :before-until #'straight-are-you-sure
  (and (bound-and-true-p doom-cli--context)
       (doom-cli-context-suppress-prompts-p doom-cli--context)))

(defadvice! doom-straight--fallback-to-tty-prompt-a (fn prompt actions)
  "Modifies straight to prompt on the terminal when in noninteractive sessions."
  :around #'straight--popup-raw
  (if (bound-and-true-p async-in-child-emacs)
      (error "Straight prompt: %s" prompt)
    (let ((doom-straight--auto-options doom-straight--auto-options))
      ;; We can't intercept C-g, so no point displaying any options for this key
      ;; when C-c is the proper way to abort batch Emacs.
      (cl-callf2 delq 'assoc actions)
      ;; HACK: Remove actions that don't work in noninteractive Emacs (like
      ;;   opening dired or magit).
      (setq actions
            (cl-remove-if (lambda (o)
                            (string-match-p "^\\(?:Magit\\|Dired\\)" (nth 1 o)))
                          actions))
      (if (doom-cli-context-suppress-prompts-p doom-cli--context)
          (cl-loop for (_key desc func) in actions
                   when desc
                   when (doom-straight--recommended-option-p prompt desc)
                   return (funcall func))
        (print! (start "%s") (red prompt))
        (print-group!
          (terpri)
          (let (recommended options)
            (print-group!
              (print! " 1) Abort")
              (cl-loop for (_key desc func) in actions
                       when desc
                       do (push func options)
                       and do
                       (print! "%2s) %s" (1+ (length options))
                               (if (doom-straight--recommended-option-p prompt desc)
                                   (progn
                                     (setq doom-straight--auto-options nil
                                           recommended (length options))
                                     (green (concat desc " (Choose this if unsure)")))
                                 desc))))
            (terpri)
            (let* ((options
                    (cons (lambda ()
                            (let ((doom-output-indent 0))
                              (terpri)
                              (print! (warn "Aborted")))
                            (doom-cli--exit 1 doom-cli--context))
                          (nreverse options)))
                   (prompt
                    (format! "How to proceed? (%s%s) "
                             (mapconcat #'number-to-string
                                        (number-sequence 1 (length options))
                                        ", ")
                             (if (not recommended) ""
                               (format "; don't know? Pick %d" (1+ recommended)))))
                   answer fn)
              (while (null (nth (setq answer (1- (read-number prompt))) options))
                (print! (warn "%s is not a valid answer, try again.") answer))
              (funcall (nth answer options)))))))))

(setq straight-arrow " > ")
(defadvice! doom-straight--respect-print-indent-a (string &rest objects)
  "Same as `message' (which see for STRING and OBJECTS) normally.
However, in batch mode, print to stdout instead of stderr."
  :override #'straight--output
  (let ((msg (apply #'format string objects)))
    (save-match-data
      (when (string-match (format "^%s\\(.+\\)$" (regexp-quote straight-arrow)) msg)
        (setq msg (match-string 1 msg))))
    (and (string-match-p "^\\(Cloning\\|\\(Reb\\|B\\)uilding\\) " msg)
         (not (string-suffix-p "...done" msg))
         (doom-print (concat "> " msg) :format t))))

(defadvice! doom-straight--ignore-gitconfig-a (fn &rest args)
  "Prevent user and system git configuration from interfering with git calls."
  :around #'straight--process-call
  (with-environment-variables
      (("GIT_CONFIG" nil)
       ("GIT_CONFIG_NOSYSTEM" "1")
       ("GIT_CONFIG_GLOBAL" (or (getenv "DOOMGITCONFIG")
                                "/dev/null")))
    (apply fn args)))

;; If the repo failed to clone correctly (usually due to a connection failure),
;; straight proceeds as normal until a later call produces a garbage result
;; (typically, when it fails to fetch the remote branch of the empty directory).
;; This causes Straight to throw an otherwise cryptic type error when it tries
;; to sanitize the result for its log buffer.
;;
;; This error is a common source of user confusion and false positive bug
;; reports, so this advice catches them to regurgitates a more cogent
;; explanation.
(defadvice! doom-straight--throw-error-on-no-branch-a (fn &rest args)
  :around #'straight--process-log
  (letf! ((defun shell-quote-argument (&rest args)
            (unless (car args)
              (error "Package was not properly cloned due to a connection failure, please try again later"))
            (apply shell-quote-argument args)))
    (apply fn args)))

(defadvice! doom-straight--regurgitate-empty-string-error-a (fn &rest args)
  :around #'straight-vc-git-local-repo-name
  (condition-case-unless-debug e
      (apply fn args)
    (wrong-type-argument
   (if (eq (cadr e) 'stringp)
       (error "Package was not properly cloned due to a connection failure, please try again later")
     (signal (car e) (cdr e))))))

(provide 'doom-straight)
;;; doom-packages.el ends here
