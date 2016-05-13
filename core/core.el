;;; core.el --- The heart of the beast
;;
;;; Naming conventions:
;;
;;   narf-…     A public variable/constant or function
;;   narf--…    An internal variable or function (non-interactive)
;;   narf/…     An autoloaded interactive function
;;   narf:…     An ex command
;;   narf|…     A hook
;;   narf*…     An advising function
;;   narf.…     Custom prefix commands
;;   …!         Macro
;;
;; Autoloaded functions are in {core,modules}/defuns/defuns-*.el
;;
;;;

(setq-default
 ;; stop package.el from being annoying. I rely solely on Cask.
 package--init-file-ensured t
 package-enable-at-startup nil
 package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.org/packages/")
   ("org" . "http://orgmode.org/elpa/"))

 ad-redefinition-action            'accept      ; silence the advised function warnings
 compilation-always-kill            t           ; kill compilation process before spawning another
 compilation-ask-about-save         nil         ; save all buffers before compiling
 compilation-scroll-output          t           ; scroll with output while compiling
 delete-by-moving-to-trash          t
 echo-keystrokes                    0.02        ; show me what I type
 ediff-diff-options                 "-w"
 ediff-split-window-function       'split-window-horizontally   ; side-by-side diffs
 ediff-window-setup-function       'ediff-setup-windows-plain   ; no extra frames
 enable-recursive-minibuffers       nil         ; no minibufferception
 idle-update-delay                  2           ; update a little less often
 inhibit-startup-echo-area-message  "hlissner"  ; username shuts up emacs
 inhibit-startup-screen             t           ; don't show emacs start screen
 initial-major-mode                'text-mode   ; initial scratch buffer mode
 initial-scratch-message            nil
 major-mode                        'text-mode
 ring-bell-function                'ignore      ; silence of the bells!
 save-interprogram-paste-before-kill nil
 sentence-end-double-space          nil

 ;; remove annoying ellipsis when printing sexp in message buffer
 eval-expression-print-length       nil
 eval-expression-print-level        nil

 ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
 minibuffer-prompt-properties
 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

 bookmark-save-flag                 t
 bookmark-default-file              (concat narf-temp-dir "/bookmarks")

 ;; Disable all backups (that's what git/dropbox are for)
 history-length                     1000
 vc-make-backup-files               nil
 auto-save-default                  nil
 auto-save-list-file-name           (concat narf-temp-dir "/autosave")
 make-backup-files                  nil
 create-lockfiles                   nil
 backup-directory-alist            `((".*" . ,(concat narf-temp-dir "/backup/")))

 ;; Remember undo history
 undo-tree-auto-save-history        nil
 undo-tree-history-directory-alist `(("." . ,(concat narf-temp-dir "/undo/"))))

;; UTF-8 please
(setq locale-coding-system    'utf-8)   ; pretty
(set-terminal-coding-system   'utf-8)   ; pretty
(set-keyboard-coding-system   'utf-8)   ; pretty
(set-selection-coding-system  'utf-8)   ; please
(prefer-coding-system         'utf-8)   ; with sugar on top
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))


;;
;; Variables
;;

(defvar narf-leader-prefix "," "Prefix key for <leader> maps")
(defvar narf-localleader-prefix "\\" "Prefix key for <localleader> maps")

;; Buffers/Files
(defvar narf-unreal-buffers '("^ ?\\*.+\\*"
                              image-mode
                              dired-mode
                              reb-mode
                              messages-buffer-mode)
  "A list of regexps or modes whose buffers are considered unreal, and will be
ignored when using `narf:next-real-buffer' and `narf:previous-real-buffer', and
killed by `narf/kill-unreal-buffers'.

`narf/kill-real-buffer' will also gloss over these buffers when finding a new
buffer to display.")

(defvar narf-ignore-buffers '("*Completions*" "*Compile-Log*" "*inferior-lisp*"
                              "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*"
                              "*Buffer List*" "*Ibuffer*" "*NeoTree*" "
                              *NeoTree*" "*esh command on file*" "*WoMan-Log*"
                              "*compilation*" "*use-package*" "*quickrun*"
                              "*eclim: problems*" "*Flycheck errors*"
                              "*popwin-dummy*"
                              ;; Helm
                              "*helm*" "*helm recentf*" "*helm projectile*"
                              "*helm imenu*" "*helm company*" "*helm buffers*"
                              "*Helm Css SCSS*" "*helm-ag*" "*helm-ag-edit*"
                              "*Helm Swoop*" "*helm M-x*" "*helm mini*"
                              "*Helm Completions*" "*Helm Find Files*"
                              "*helm mu*" "*helm mu contacts*"
                              "*helm-mode-describe-variable*"
                              "*helm-mode-describe-function*"
                              ;; Org
                              "*Org todo*" "*Org Links*" "*Agenda Commands*")
  "List of buffer names to ignore when using `winner-undo', or `winner-redo'")

(defvar narf-cleanup-processes-alist '(("pry" . ruby-mode)
                                       ("irb" . ruby-mode)
                                       ("ipython" . python-mode))
  "An alist of (process-name . major-mode), that `narf:cleanup-processes' checks
before killing processes. If there are no buffers with matching major-modes, it
gets killed.")

(defvar narf-project-root-files
  '(".git" ".hg" ".svn" ".project" "local.properties" "project.properties"
    "rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt"
    "build.gradle" "Gemfile" "requirements.txt" "tox.ini" "package.json"
    "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml"
    "mix.exs")
  "A list of files that count as 'project files', which determine whether a
folder is the root of a project or not.")

;; Fringe/margins
(defvar narf-fringe-size 6 "Default width to use for the fringes.")


;;
;; Bootstrap
;;

(require 'f)
(autoload 'awhen "anaphora" "" nil 'macro)
(autoload 'aif "anaphora" "" nil 'macro)
(autoload 'use-package "use-package" "" nil 'macro)
(unless (require 'autoloads nil t)
  (load (concat narf-emacs-dir "/scripts/generate-autoloads.el"))
  (require 'autoloads))
(require 'core-defuns)

(eval-when-compile
  (setq use-package-verbose nil)

  ;; Make any folders needed
  (mapc (lambda (dir)
          (let ((path (concat narf-temp-dir dir)))
            (unless (file-exists-p path)
              (make-directory path t))))
        '("" "/undo" "/backup")))

(use-package persistent-soft
  :commands (persistent-soft-store
             persistent-soft-fetch
             persistent-soft-exists-p
             persistent-soft-flush
             persistent-soft-location-readable
             persistent-soft-location-destroy)
  :init (defvar pcache-directory (concat narf-temp-dir "/pcache/")))

(use-package async
  :commands (async-start
             async-start-process
             async-get
             async-wait
             async-inject-variables))

;;
;; We add this to `after-init-hook' to allow errors to stop this advice
(add-hook! after-init
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (cl-flet ((process-list ())) ad-do-it)))

(defun display-startup-echo-area-message ()
  (message ":: Loaded in %s" (emacs-init-time)))

(provide 'core)
;;; core.el ends here
