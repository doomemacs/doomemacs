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
 bookmark-default-file (concat doom-cache-dir "bookmarks")
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

(defun doom|ediff-use-existing-frame ()
  "Use existing frame instead of creating a new one."
  (setq ediff-diff-options           "-w"
        ediff-split-window-function  #'split-window-horizontally
        ;; no extra frames
        ediff-window-setup-function  #'ediff-setup-windows-plain))
(add-hook 'ediff-load-hook #'doom|ediff-use-existing-frame)

(defun doom|dont-kill-scratch-buffer ()
  "Don't kill the scratch buffer."
  (or (not (string= (buffer-name) "*scratch*"))
      (ignore (bury-buffer))))
(add-hook 'kill-buffer-query-functions #'doom|dont-kill-scratch-buffer)

(defun doom*delete-trailing-whitespace (orig-fn &rest args)
  "Don't affect trailing whitespace on current line."
  (let ((linestr (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))
    (apply orig-fn args)
    (when (and (if (featurep 'evil) (evil-insert-state-p) t)
               (string-match-p "^[\s\t]*$" linestr))
      (insert linestr))))
(advice-add #'delete-trailing-whitespace :around #'doom*delete-trailing-whitespace)

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

;; Automatic minor modes
(defvar doom-auto-minor-mode-alist '()
  "Alist mapping filename patterns to corresponding minor mode functions, like
`auto-mode-alist'. All elements of this alist are checked, meaning you can
enable multiple minor modes for the same regexp.")

(defun doom|enable-minor-mode-maybe ()
  "Check file name against `doom-auto-minor-mode-alist'."
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist doom-auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match-p (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(add-hook 'find-file-hook #'doom|enable-minor-mode-maybe)

;; ensure indirect buffers have buffer-file-name
(defun doom*set-indirect-buffer-filename (orig-fn base-buffer name &optional clone)
  "In indirect buffers, `buffer-file-name' is nil, which can cause problems
with functions that require it (like modeline segments)."
  (let ((file-name (buffer-file-name base-buffer))
        (buffer (funcall orig-fn base-buffer name clone)))
    (when (and file-name buffer)
      (with-current-buffer buffer
        (unless buffer-file-name
          (setq buffer-file-name file-name
                buffer-file-truename (file-truename file-name)))))
    buffer))
(advice-add #'make-indirect-buffer :around #'doom*set-indirect-buffer-filename)


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
      savehist-save-minibuffer-hisstory t
      savehist-autosave-interval nil ; save on kill only
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      save-place-file (concat doom-cache-dir "saveplace"))
(add-hook! 'doom-init-hook #'(savehist-mode save-place-mode))

;; Keep track of recently opened files
(def-package! recentf
  :init (add-hook 'doom-init-hook #'recentf-mode)
  :config
  (setq recentf-save-file (concat doom-cache-dir "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(abbreviate-file-name)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              ;; ignore private DOOM temp files (but not all of them)
              (concat "^" (replace-regexp-in-string
                           (concat "@" (regexp-quote (system-name)))
                           "@" (abbreviate-file-name doom-host-dir))))))


;;
;; Core Plugins
;;

;; Handles whitespace (tabs/spaces) settings externally. This way projects can
;; specify their own formatting rules.
(def-package! editorconfig
  :demand t
  :mode ("\\.?editorconfig$" . editorconfig-conf-mode)
  :init
  (def-setting! :editorconfig (action value)
    ":add or :remove an entry in `editorconfig-indentation-alist'."
    (cond ((eq action :add)
           `(push ,value editorconfig-indentation-alist))
          ((eq action :remove)
           `(setq editorconfig-indentation-alist
                  (assq-delete-all ,value editorconfig-indentation-alist)))
          (t (error "%s is an invalid action for :editorconfig"
                    action))))

  :config
  (add-hook 'doom-init-hook #'editorconfig-mode)

  ;; Editorconfig makes indentation weird in Lisp modes, so we disable it. It
  ;; still applies other project settings (e.g. tabs vs spaces) though.
  (set! :editorconfig :remove 'emacs-lisp-mode)
  (set! :editorconfig :remove 'lisp-mode)

  (defvar whitespace-style)
  (defun doom|editorconfig-whitespace-mode-maybe (&rest _)
    "Show whitespace-mode when file uses TABS (ew)."
    (when indent-tabs-mode
      (let ((whitespace-style '(face tabs tab-mark trailing-lines tail)))
        (whitespace-mode +1))))
  (add-hook 'editorconfig-custom-hooks #'doom|editorconfig-whitespace-mode-maybe))

;; Auto-close delimiters and blocks as you type
(def-package! smartparens
  :demand t
  :config
  (setq sp-autowrap-region nil ; let evil-surround handle this
        sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0
        sp-max-pair-length 3)

  (add-hook 'doom-init-hook #'smartparens-global-mode)
  (require 'smartparens-config)
  ;; Smartparens interferes with Replace mode
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode)

  ;; Auto-close more conservatively
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))

  (sp-local-pair 'css-mode "/*" "*/"
                 :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC")))
  (sp-local-pair '(sh-mode markdown-mode) "`" nil
                 :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-local-pair '(xml-mode nxml-mode php-mode) "<!--" "-->"
                 :post-handlers '(("| " "SPC"))))

;; Branching undo
(def-package! undo-tree
  :demand t
  :config
  (global-undo-tree-mode +1)

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

(def-package! ace-window
  :commands (ace-window ace-swap-window ace-delete-window
             ace-select-window ace-delete-other-windows)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))

(def-package! avy
  :commands (avy-goto-char-2 avy-goto-line)
  :config
  (setq avy-all-windows nil
        avy-background t))

(def-package! command-log-mode
  :commands (command-log-mode global-command-log-mode)
  :config
  (set! :popup "*command-log*" :size 40 :align 'right :noselect t)
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode t))

(def-package! expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

(def-package! help-fns+ ; Improved help commands
  :commands (describe-buffer describe-command describe-file
             describe-keymap describe-option describe-option-of-type))

(def-package! imenu-anywhere
  :commands (ido-imenu-anywhere ivy-imenu-anywhere helm-imenu-anywhere))

(def-package! imenu-list :commands imenu-list-minor-mode)

(def-package! pcre2el :commands rxt-quote-pcre)

(def-package! smart-forward
  :commands (smart-up smart-down smart-backward smart-forward))

(def-package! wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :config
  (setq wgrep-auto-save-buffer t))

(provide 'core-editor)
;;; core-editor.el ends here
