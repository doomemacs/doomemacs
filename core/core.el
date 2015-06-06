;;; core.el --- The heart of the beast
;;
;;; Naming conventions:
;;
;;   narf-*       A public variable/constant or function
;;   narf--*      A private variable or function (non-interactive)
;;   narf/*       An autoloaded interactive function
;;   narf:*       An ex command
;;   narf|*       A hook
;;   @*           Macro call
;;
;;; Bootstrap:

(fset '! 'eval-when-compile)

(defconst narf-emacs-dir     user-emacs-directory)
(defconst narf-core-dir      (! (concat narf-emacs-dir "core/")))
(defconst narf-modules-dir   (! (concat narf-emacs-dir "modules/")))
(defconst narf-contrib-dir   (! (concat narf-emacs-dir "contrib/")))
(defconst narf-private-dir   (! (concat narf-emacs-dir "private/")))
(defconst narf-elpa-dir      (! (concat narf-emacs-dir ".cask/" emacs-version "/elpa/")))
(defconst narf-temp-dir      (concat narf-private-dir "cache/" (system-name) "/"))
(defconst narf-snippet-dirs  (! (list (concat narf-private-dir "snippets/")
                                      (concat narf-private-dir "templates/"))))

(! (defun --subdirs (path)
     (let ((result '())
           (paths (ignore-errors (directory-files path t "^[^.]" t))))
       (dolist (file paths)
         (when (file-directory-p file)
           (add-to-list 'result file)))
       result)))

;; Scan various folders to populate the load-dirs
(setq custom-theme-load-path
      (! (append (--subdirs (concat narf-private-dir "themes/"))
                 custom-theme-load-path)))
(setq load-path
      (! (setq load-path (append (list narf-core-dir narf-contrib-dir narf-modules-dir narf-private-dir)
                                 (list (concat narf-core-dir "defuns"))
                                 load-path
                                 (--subdirs narf-contrib-dir)
                                 (--subdirs narf-contrib-dir)))
         (require 'cask)
         (cask-initialize)
         load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'benchmark)    ; records load times in `require-times'; also see `list-times'
(require 'autoloads nil t) ; generate autoloads with `make autoloads`
(require 'core-vars)
(require 'core-defuns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(! (require 's)
   (require 'dash)
   (require 'f)

   (add-to-list 'load-path (concat narf-core-dir "macros/"))

   (setq use-package-verbose narf-debug-mode)
   ;; (setq use-package-expand-minimally (not narf-debug-mode))
   (require 'use-package)

   (defun use-package--add-keyword (keyword after)
     (setq use-package-keywords
           (-insert-at (-find-index (lambda (key) (eq key after)) use-package-keywords)
                       keyword use-package-keywords)))

   (progn ; add :after to use-package
     (use-package--add-keyword :after :load-path)
     (setq use-package-keywords
           (-insert-at (--find-index (eq it :load-path) use-package-keywords)
                       :after use-package-keywords))

     (defalias 'use-package-normalize/:after 'use-package-normalize-symlist)

     (defun use-package-handler/:after (name-symbol keyword arg rest state)
       (let ((body (use-package-process-keywords name-symbol rest state)))
         (if (null arg)
             body
           (use-package-concat
            (use-package-process-keywords name-symbol
             (use-package-sort-keywords (use-package-plist-maybe-put rest :defer t)) state)
            (apply #'nconc
                   (mapcar (lambda (feature)
                             `(,(macroexpand `(@after ,feature (require ',name-symbol)))))
                           (delete-dups arg))))))))

   (progn ; add :map for in-house key binding macro
     (use-package--add-keyword :map :bind)

     (defalias 'use-package-normalize/:map 'use-package-normalize-forms)

     ;; TODO: Write :map
     (defun use-package-handler/:map (name-symbol keyword arg rest state)
       (use-package-process-keywords name-symbol rest state))
     )
   )
(require 'diminish)

;; Emacs configuration ;;;;;;;;;;;;;;;;;

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (eq system-type 'windows-nt))

;;; UTF-8 please
(setq locale-coding-system    'utf-8)   ; pretty
(set-terminal-coding-system   'utf-8)   ; pretty
(set-keyboard-coding-system   'utf-8)   ; pretty
(set-selection-coding-system  'utf-8)   ; please
(prefer-coding-system         'utf-8)   ; with sugar on top

(fset 'yes-or-no-p 'y-or-n-p)           ; y/n instead of yes/no

(setq-default
 confirm-kill-emacs  (lambda (prompt) (y-or-n-p ">> Gee, I dunno Brain... Are you sure?"))

 gc-cons-threshold                  20000000    ; avoid garbage collection
 enable-recursive-minibuffers       t           ; minibufferception
 echo-keystrokes                    0.02        ; show me those keystrokes
 ring-bell-function                'ignore      ; silence of the bells!

 inhibit-startup-screen             t           ; don't show emacs start screen
 inhibit-startup-echo-area-message  "hlissner"  ; username shuts up emacs
 initial-major-mode                'text-mode   ; initial scratch buffer mode
 initial-scratch-message            nil

 compilation-always-kill            t
 compilation-ask-about-save         nil
 compilation-scroll-output          t

 sentence-end-double-space          nil         ; sentences end with periods. period.

 ediff-diff-options                 "-w"
 ediff-split-window-function       'split-window-horizontally   ; side-by-side diffs
 ediff-window-setup-function       'ediff-setup-windows-plain   ; no extra frames

 ;; Don't save clipboard contents into kill-ring before replacing them
 save-interprogram-paste-before-kill nil

 ;; Don't let the cursor go into minibuffer prompt
 ;; from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
 minibuffer-prompt-properties
 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

 ;; remove annoying ellipsis when printing sexp in message buffer
 eval-expression-print-length       nil
 eval-expression-print-level        nil

 history-length                     1000

 ;; Backups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Disable all backups (that's what git/dropbox are for)
 bookmark-save-flag                 t
 bookmark-default-file              (! (concat narf-temp-dir "bookmarks"))
 auto-save-default                  nil
 auto-save-list-file-name           (! (concat narf-temp-dir "autosave"))
 ;; In case I want to reactivate backup files
 make-backup-files                  nil
 create-lockfiles                   nil
 backup-directory-alist            `((".*" . ,(! (concat narf-temp-dir "backup/"))))

 ;; Remember undo history
 undo-tree-auto-save-history t
 undo-tree-history-directory-alist `(("." . ,(! (concat narf-temp-dir "undo/")))))

;; Make any folders needed
(! (dolist (file '("" "undo" "backup"))
     (let ((path (concat narf-temp-dir file)))
       (unless (file-exists-p path)
         (make-directory path t)))))

;; Save cursor location across sessions. Only save for files that exist.
(require 'saveplace)
(setq save-place-file (! (concat narf-temp-dir "saveplace")))
(@add-hook find-file (if (file-exists-p (buffer-file-name)) (setq save-place t)))

;; Save history across sessions
(require 'savehist)
(setq savehist-file (! (concat narf-temp-dir "savehist"))
      savehist-additional-variables
      '(kill-ring global-mark-ring search-ring regexp-search-ring extended-command-history))
(savehist-mode 1)

(require 'recentf)
(setq recentf-save-file (! (concat narf-temp-dir "recentf"))
      recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                        "/\\.cache/.+" "emacs\\.d/workgroups/.+$" ".emacs.workgroup"
                        "/company-statistics-cache.el$")
      recentf-max-menu-items 0
      recentf-max-saved-items 250
      recentf-auto-cleanup 600)
(recentf-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package help-fns+ ; Improved help commands
  :commands (describe-buffer describe-command describe-file
             describe-keymap describe-option describe-option-of-type))

(use-package popwin :config (popwin-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond (IS-MAC      (require 'core-os-osx))
      (IS-LINUX    (require 'core-os-linux))
      (IS-WINDOWS  (require 'core-os-win32)))

(require 'core-ui)
(require 'core-evil)
;; (require 'core-editor)
;; (require 'core-completion)
;; (require 'core-syntax-checker)
;; (require 'core-snippets)
;; (require 'core-templates)
;; (require 'core-project)
;; (require 'core-vcs)
;; (require 'core-sessions)
;; (require 'core-quickrun)

;; (@add-hook after-init
;;   (use-package my-bindings)
;;   (use-package my-commands))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package server :config (unless (server-running-p) (server-start)))


(provide 'core)
;;; core.el ends here
