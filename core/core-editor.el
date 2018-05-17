;;; core-editor.el -*- lexical-binding: t; -*-

(defvar doom-large-file-size 1
  "Size (in MB) above which the user will be prompted to open the file literally
to avoid performance issues. Opening literally means that no major or minor
modes are active and the buffer is read-only.")

(defvar doom-large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
    doc-view-mode doc-view-mode-maybe ebrowse-tree-mode pdf-view-mode)
  "Major modes that `doom|check-large-file' will ignore.")

(setq-default
 vc-follow-symlinks t
 ;; Save clipboard contents into kill-ring before replacing them
 save-interprogram-paste-before-kill t
 ;; Bookmarks
 bookmark-default-file (concat doom-etc-dir "bookmarks")
 bookmark-save-flag t
 ;; Formatting
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
 word-wrap t
 ;; Scrolling
 hscroll-margin 1
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 0
 hscroll-margin 2
 scroll-preserve-screen-position t
 ;; Whitespace (see `editorconfig')
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 tabify-regexp "^\t* [ \t]+" ; for :retab
 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50)

;; ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

(defun doom|check-large-file ()
  "Check if the buffer's file is large (see `doom-large-file-size'). If so, ask
for confirmation to open it literally (read-only, disabled undo and in
fundamental-mode) for performance sake."
  (let ((size (nth 7 (file-attributes buffer-file-name))))
    (when (and (not (memq major-mode doom-large-file-modes-list))
               size (> size (* 1024 1024 doom-large-file-size))
               (y-or-n-p
                (format (concat "%s is a large file, open literally to "
                                "avoid performance issues?")
                        (file-relative-name buffer-file-name))))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))
(add-hook 'find-file-hook #'doom|check-large-file)


;;
;; Built-in plugins
;;

(push '("/[A-Z]+$" . text-mode) auto-mode-alist)

(electric-indent-mode -1) ; enabled by default in Emacs 25+. No thanks.

(when (and (display-graphic-p)
           (require 'server nil t)
           (not (server-running-p)))
  (server-start))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; revert buffers for changed files
(def-package! autorevert
  :defer doom-before-switch-buffer-hook
  :config
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode +1))

;; persist variables across sessions
(def-package! savehist
  :defer (pre-command-hook . 1)
  :config
  (setq savehist-file (concat doom-cache-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode +1))

;; persistent point location in buffers
(def-package! saveplace
  :defer doom-before-switch-buffer-hook
  :config
  (setq save-place-file (concat doom-cache-dir "saveplace"))
  (defun doom*recenter-on-load-saveplace (&rest _)
    "Recenter on cursor when loading a saved place."
    (if buffer-file-name (ignore-errors (recenter))))
  (advice-add #'save-place-find-file-hook
              :after-while #'doom*recenter-on-load-saveplace)
  (save-place-mode +1))

;; Keep track of recently opened files
(def-package! recentf
  :defer (pre-command-hook . 1)
  :commands recentf-open-files
  :config
  (setq recentf-save-file (concat doom-cache-dir "recentf")
        recentf-auto-cleanup 60
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list #'file-remote-p "\\.\\(gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
              "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              ;; ignore private DOOM temp files (but not all of them)
              (concat "^" (file-truename doom-local-dir))))
  (recentf-mode +1))


;;
;; Core Plugins
;;

;; Auto-close delimiters and blocks as you type
(def-package! smartparens
  :defer doom-before-switch-buffer-hook
  :commands (sp-pair sp-local-pair sp-with-modes)
  :config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0
        sp-max-pair-length 3)

  ;; smartparens conflicts with evil-mode's replace state
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode)

  (sp-local-pair '(xml-mode nxml-mode php-mode) "<!--" "-->"
                 :post-handlers '(("| " "SPC")))

  (smartparens-global-mode +1))

;; Branching undo
(def-package! undo-tree
  :defer doom-before-switch-buffer-hook
  :config
  ;; persistent undo history is known to cause undo history corruption, which
  ;; can be very destructive! So disable it!
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist
        (list (cons "." (concat doom-cache-dir "undo-tree-hist/"))))
  (global-undo-tree-mode +1))


;;
;; Autoloaded Plugins
;;

(def-package! command-log-mode
  :commands (command-log-mode global-command-log-mode)
  :config
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode t))

(def-package! expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word)
  :config
  (defun doom*quit-expand-region ()
    (when (memq last-command '(er/expand-region er/contract-region))
      (er/contract-region 0)))
  (advice-add #'evil-escape :before #'doom*quit-expand-region)
  (advice-add #'doom/escape :before #'doom*quit-expand-region))

(def-package! helpful
  :commands (helpful-callable helpful-function helpful-macro helpful-command
             helpful-key helpful-variable helpful-at-point)
  :init
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)

  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key))

(def-package! pcre2el
  :commands rxt-quote-pcre)

(provide 'core-editor)
;;; core-editor.el ends here
