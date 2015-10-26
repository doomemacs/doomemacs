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
;;;

(setq-default
 ad-redefinition-action            'accept      ; silence the advised function warnings
 compilation-always-kill            t
 compilation-ask-about-save         nil
 compilation-scroll-output          t
 confirm-kill-emacs  (lambda (prompt) (y-or-n-p ">> Gee, I dunno Brain... Are you sure?"))
 echo-keystrokes                    0.02        ; show me those keystrokes
 ediff-diff-options                 "-w"
 ediff-split-window-function       'split-window-horizontally   ; side-by-side diffs
 ediff-window-setup-function       'ediff-setup-windows-plain   ; no extra frames
 enable-recursive-minibuffers       t           ; minibufferception
 history-length                     1000
 inhibit-startup-echo-area-message  "hlissner"  ; username shuts up emacs
 inhibit-startup-screen             t           ; don't show emacs start screen
 initial-major-mode                'text-mode   ; initial scratch buffer mode
 initial-scratch-message            nil
 major-mode                        'text-mode
 ring-bell-function                'ignore      ; silence of the bells!
 save-interprogram-paste-before-kill nil
 sentence-end-double-space          nil

 ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

 ;; remove annoying ellipsis when printing sexp in message buffer
 eval-expression-print-length       nil
 eval-expression-print-level        nil

 ;; Disable all backups (that's what git/dropbox are for)
 bookmark-save-flag                 t
 bookmark-default-file              (! (concat narf-temp-dir "bookmarks"))
 auto-save-default                  nil
 auto-save-list-file-name           (! (concat narf-temp-dir "autosave"))

 ;; In case I want to reactivate backup files
 make-backup-files                  nil
 create-lockfiles                   nil
 backup-directory-alist            `((".*" . ,(concat narf-temp-dir "backup/")))

 ;; Remember undo history
 undo-tree-auto-save-history t
 undo-tree-history-directory-alist `(("." . ,(concat narf-temp-dir "undo/"))))

;;; UTF-8 please
(setq locale-coding-system    'utf-8)   ; pretty
(set-terminal-coding-system   'utf-8)   ; pretty
(set-keyboard-coding-system   'utf-8)   ; pretty
(set-selection-coding-system  'utf-8)   ; please
(prefer-coding-system         'utf-8)   ; with sugar on top
(fset 'yes-or-no-p 'y-or-n-p)           ; y/n instead of yes/no


;; Bootstrap ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (require 'autoloads nil t)
  (load (concat narf-emacs-dir "scripts/generate-autoloads.el"))
  (require 'autoloads))
(require 'core-vars)
(require 'core-defuns)
(require 'diminish)

(autoload 'use-package "use-package" "" nil 'macro)
(eval-when-compile
  (setq use-package-verbose nil)

  ;; Make any folders needed
  (dolist (file '("" "undo" "backup"))
    (let ((path (concat narf-temp-dir file)))
      (unless (file-exists-p path)
        (make-directory path t)))))

;; Save history across sessions
(use-package savehist
  :config
  (setq savehist-file (concat narf-temp-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-additional-variables
        '(kill-ring search-ring regexp-search-ring))
  (savehist-mode 1)

  ;; text properties severely bloat the history so delete them (courtesy of PythonNut)
  (defun unpropertize-savehist ()
    (mapc (lambda (list)
            (with-demoted-errors
                (when (boundp list)
                  (set list (mapcar #'substring-no-properties (eval list))))))
          '(kill-ring
            minibuffer-history
            helm-grep-history
            helm-ff-history
            file-name-history
            read-expression-history
            extended-command-history
            evil-ex-history)))
  (add-hook 'kill-emacs-hook #'unpropertize-savehist)
  (add-hook 'savehist-save-hook #'unpropertize-savehist))

(use-package recentf
  :config
  (setq recentf-save-file (concat narf-temp-dir "recentf")
        recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                          "emacs\\.d/private/cache/.+" "emacs\\.d/workgroups/.+$" "wg-default"
                          "/company-statistics-cache.el$")
        recentf-max-menu-items 0
        recentf-max-saved-items 250
        recentf-auto-cleanup 600)
  (recentf-mode 1))

(use-package saveplace
  :defer t
  :config (setq save-place-file (concat narf-temp-dir "saveplace"))
  :init
  ;; Save cursor location across sessions. Only save for files that exist.
  (add-hook! find-file
    (if (file-exists-p (buffer-file-name))
        (require 'saveplace)
        (setq save-place t))))

(use-package help-fns+ ; Improved help commands
  :commands (describe-buffer describe-command describe-file
             describe-keymap describe-option describe-option-of-type))

(defun narf-init ()
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (flet ((process-list ())) ad-do-it))
  (defun display-startup-echo-area-message ()
    (message ">>> Loaded in %s" (emacs-init-time)))
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(provide 'core)
;;; core.el ends here
