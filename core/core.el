;;; core.el --- The heart of the beast
;;
;;; Naming conventions:
;;
;;   narf-...     A public variable/constant or function
;;   narf--...    An internal variable or function (non-interactive)
;;   narf/...     An autoloaded interactive function
;;   narf:...     An ex command
;;   narf|...     A hook
;;   narf*...     An advising function
;;   ...!         Macro
;;
;;; Bootstrap:

(unless (require 'autoloads nil t)
  (load (concat narf-emacs-dir "scripts/generate-autoloads.el"))
  (require 'autoloads))

(require 'core-vars)
(require 'core-defuns)
(require 'diminish)

;; NARF!
(define-minor-mode narf-mode "Narf, yoink, poit."
  :global t
  :init-value t
  :keymap (make-sparse-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 's)
  (require 'dash)
  (require 'f)

  (defvar use-package-verbose narf-debug-mode)
  ;; (setq use-package-expand-minimally (not narf-debug-mode))
  (require 'use-package)

  (defun use-package--add-keyword (keyword after)
    (setq use-package-keywords
          (-insert-at (-find-index (lambda (key) (eq key after)) use-package-keywords)
                      keyword use-package-keywords)))

  (progn ; remap :bind to bind! macro instead of bind-keys
    ;; (defun use-package-handler/:bind
    ;;     (name-symbol keyword arg rest state &optional override)
    ;;   (let ((commands (mapcar #'cdr arg)))
    ;;     (use-package-concat
    ;;      (use-package-process-keywords name-symbol
    ;;        (use-package-sort-keywords
    ;;         (use-package-plist-maybe-put rest :defer t))
    ;;        (use-package-plist-append state :commands commands))
    ;;      `((ignore (,bind! ,@arg))))))
    )

  (progn ; add :after to use-package
    (use-package--add-keyword :after :load-path)

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
                            `((after! ,feature (require ',name-symbol))))
                          (delete-dups arg))))))))

  ;; Make any folders needed
  (dolist (file '("" "undo" "backup"))
    (let ((path (concat narf-temp-dir file)))
      (unless (file-exists-p path)
        (make-directory path t)))))

;; Emacs configuration ;;;;;;;;;;;;;;;;;

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
 major-mode                        'text-mode

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

;; Save history across sessions
(require 'savehist)
(setq savehist-file (! (concat narf-temp-dir "savehist"))
      savehist-additional-variables
      '(kill-ring global-mark-ring search-ring regexp-search-ring extended-command-history))
(savehist-mode 1)

(require 'recentf)
(setq recentf-save-file (! (concat narf-temp-dir "recentf"))
      recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                        "emacs\\.d/private/cache/.+" "emacs\\.d/workgroups/.+$" "wg-default"
                        "/company-statistics-cache.el$")
      recentf-max-menu-items 0
      recentf-max-saved-items 250
      recentf-auto-cleanup 600)
(recentf-mode 1)

(require 'popwin)
(popwin-mode 1)

;; Save cursor location across sessions. Only save for files that exist.
(use-package saveplace
  :defer t
  :config (setq save-place-file (! (concat narf-temp-dir "saveplace")))
  :init
  (add-hook! find-file
    (if (file-exists-p (buffer-file-name))
        (require 'saveplace)
        (setq save-place t))))

(use-package help-fns+ ; Improved help commands
  :commands (describe-buffer describe-command describe-file
             describe-keymap describe-option describe-option-of-type))

(use-package server
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(add-hook! after-init
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (flet ((process-list ())) ad-do-it)))

(provide 'core)
;;; core.el ends here
