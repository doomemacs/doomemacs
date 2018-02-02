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
 scroll-preserve-screen-position t
 ;; Whitespace (see `editorconfig')
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 tabify-regexp "^\t* [ \t]+" ; for :retab
 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50
 ;; whitespace-mode
 whitespace-line-column fill-column
 whitespace-style
 '(face indentation tabs tab-mark spaces space-mark newline newline-mark
   trailing lines-tail)
 whitespace-display-mappings
 '((tab-mark ?\t [?› ?\t])
   (newline-mark ?\n [?¬ ?\n])
   (space-mark ?\  [?·] [?.])))

;; ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

(defun doom|check-large-file ()
  "Check if the buffer's file is large (see `doom-large-file-size'). If so, ask
for confirmation to open it literally (read-only, disabled undo and in
fundamental-mode) for performance sake."
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and (not (memq major-mode doom-large-file-modes-list))
               size (> size (* 1024 1024 doom-large-file-size))
               (y-or-n-p
                (format (concat "%s is a large file, open literally to "
                                "avoid performance issues?")
                        (file-relative-name filename))))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))
(add-hook 'find-file-hook #'doom|check-large-file)

(push '("/LICENSE$" . text-mode) auto-mode-alist)


;;
;; Built-in plugins
;;

;; revert buffers for changed files
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; enabled by default in Emacs 25+. No thanks.
(electric-indent-mode -1)

;; savehist / saveplace
(setq savehist-file (concat doom-cache-dir "savehist")
      savehist-save-minibuffer-history t
      savehist-autosave-interval nil ; save on kill only
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      save-place-file (concat doom-cache-dir "saveplace"))
(add-hook! 'doom-init-hook #'(savehist-mode save-place-mode))
(defun doom*recenter-on-load-saveplace (&rest _)
  "Recenter on cursor when loading a saved place."
  (if buffer-file-name (ignore-errors (recenter))))
(advice-add #'save-place-find-file-hook :after-while #'doom*recenter-on-load-saveplace)

;; Keep track of recently opened files
(def-package! recentf
  :hook (doom-init . recentf-mode)
  :config
  (setq recentf-save-file (concat doom-cache-dir "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              ;; ignore private DOOM temp files (but not all of them)
              (concat "^" (file-truename doom-local-dir)))))


;;
;; Core Plugins
;;

;; Handles whitespace (tabs/spaces) settings externally. This way projects can
;; specify their own formatting rules.
(def-package! editorconfig
  :hook (doom-init . editorconfig-mode)
  :config
  ;; editorconfig cannot procure the correct settings for extension-less files.
  ;; Executable scripts with a shebang line, for example. So why not use Emacs'
  ;; major mode to drop editorconfig a hint? This is accomplished by temporarily
  ;; appending an extension to `buffer-file-name' when we talk to editorconfig.
  (defvar doom-editorconfig-mode-alist
    '((sh-mode     . "sh")
      (python-mode . "py")
      (ruby-mode   . "rb")
      (perl-mode   . "pl")
      (php-mode    . "php"))
    "An alist mapping major modes to extensions. Used by
`doom*editorconfig-smart-detection' to give editorconfig filetype hints.")

  (defun doom*editorconfig-smart-detection (orig-fn &rest args)
    "Retrieve the properties for the current file. If it doesn't have an
extension, try to guess one."
    (let ((buffer-file-name
           (if (file-name-extension buffer-file-name)
               buffer-file-name
             (format "%s%s" buffer-file-name
                     (let ((ext (cdr (assq major-mode doom-editorconfig-mode-alist))))
                       (or (and ext (concat "." ext))
                           ""))))))
      (apply orig-fn args)))
  (advice-add #'editorconfig-call-editorconfig-exec :around #'doom*editorconfig-smart-detection)

  ;; Editorconfig makes indentation too rigid in Lisp modes, so tell
  ;; editorconfig to ignore indentation. I prefer dynamic indentation support
  ;; built into Emacs.
  (dolist (mode '(emacs-lisp-mode lisp-mode))
    (map-delete editorconfig-indentation-alist mode))

  (defvar whitespace-style)
  (defun doom|editorconfig-whitespace-mode-maybe (&rest _)
    "Show whitespace-mode when file uses TABS (ew)."
    (when indent-tabs-mode
      (let ((whitespace-style '(face tabs tab-mark trailing-lines tail)))
        (whitespace-mode +1))))
  (add-hook 'editorconfig-custom-hooks #'doom|editorconfig-whitespace-mode-maybe))

(def-package! editorconfig-conf-mode
  :mode "\\.?editorconfig$")

;; Auto-close delimiters and blocks as you type
(def-package! smartparens
  :config
  (smartparens-global-mode +1)
  (require 'smartparens-config)

  (setq sp-autowrap-region nil ; let evil-surround handle this
        sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0
        sp-max-pair-length 3)

  ;; disable smartparens in evil-mode's replace state (they conflict)
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode)

  (sp-local-pair '(xml-mode nxml-mode php-mode) "<!--" "-->"
                 :post-handlers '(("| " "SPC"))))

;; Branching undo
(def-package! undo-tree
  :hook (doom-init . global-undo-tree-mode)
  :config
  ;; persistent undo history is known to cause undo history corruption, which
  ;; can be very destructive! So disable it!
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist
        (list (cons "." (concat doom-cache-dir "undo-tree-hist/")))))


;;
;; Autoloaded Plugins
;;

(def-package! ace-link
  :commands (ace-link-help ace-link-org))

(def-package! avy
  :commands (avy-goto-char-2 avy-goto-line)
  :config
  (setq avy-all-windows nil
        avy-background t))

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

(def-package! smart-forward
  :commands (smart-up smart-down smart-backward smart-forward))

(provide 'core-editor)
;;; core-editor.el ends here
