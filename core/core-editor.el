;;; core-editor.el --- filling the editor shaped hole in the Emacs OS

(setq-default
 shift-select-mode t ; activate mark on shift-click
 ;; Save clipboard contents into kill-ring before replacing them
 save-interprogram-paste-before-kill t
 ;; Bookmarks
 bookmark-default-file (concat doom-cache-dir "/bookmarks")
 bookmark-save-flag t
 ;; Formatting
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
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
 tabify-regexp "^\t* [ \t]+"  ; for :retab
 whitespace-line-column fill-column
 whitespace-style
 '(face tabs tab-mark trailing lines-tail)
 whitespace-display-mappings
 '((tab-mark ?\t [?› ?\t]) (newline-mark 10  [36 10]))
 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50
 visual-fill-column-center-text nil
 word-wrap t)

;; Save point across sessions
(require 'saveplace)
(setq save-place-file (concat doom-cache-dir "saveplace"))
(when (>= emacs-major-version 25)
  (save-place-mode +1))

;; Save history across sessions
(require 'savehist)
(setq savehist-file (concat doom-cache-dir "savehist")
      savehist-save-minibuffer-history t
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

;; Remove text-property cruft from history
(defun doom|unpropertize-savehist ()
  (mapc (lambda (list)
          (when (boundp list)
            (set list (mapcar 'substring-no-properties (eval list)))))
        '(kill-ring minibuffer-history helm-grep-history helm-ff-history
          file-name-history read-expression-history extended-command-history
          evil-ex-history)))
(add-hook 'kill-emacs-hook    'doom|unpropertize-savehist)
(add-hook 'savehist-save-hook 'doom|unpropertize-savehist)

;; Keep track of recently opened files
(require 'recentf)
(setq recentf-save-file (concat doom-cache-dir "recentf")
      recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                        "emacs\\.d/private/cache/.+" "emacs\\.d/workgroups/.+$"
                        "wg-default" "/company-statistics-cache.el$"
                        "^/var/folders/.+$" "^/tmp/.+")
      recentf-max-menu-items 0
      recentf-max-saved-items 250
      recentf-filename-handlers '(abbreviate-file-name))
(@quiet (recentf-mode 1))

;; Ediff
(@add-hook ediff-load
  (setq ediff-diff-options           "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)) ; no extra frames

;; revert buffers for changed files
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)


;;
;; Core Plugins
;;

;; Handles whitespace (tabs/spaces) settings externally. This way projects can
;; specify their own formatting rules.
(@def-package editorconfig :demand t
  :mode ("\\.?editorconfig$" . editorconfig-conf-mode)
  :config (editorconfig-mode +1)
  ;; Show whitespace in tabs indentation mode
  (@add-hook 'editorconfig-custom-hooks
    (if indent-tabs-mode (whitespace-mode +1))))

;; Auto-close delimiters and blocks as you type
(@def-package smartparens :demand t
  :init
  (setq sp-autowrap-region nil          ; let evil-surround handle this
        sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0
        sp-max-pair-length 3)

  :config
  (smartparens-global-mode 1)
  (require 'smartparens-config)
  ;; Smartparens interferes with Replace mode
  (add-hook 'evil-replace-state-entry-hook 'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook  'turn-on-smartparens-mode)
  ;; Auto-close more conservatively
  (sp-pair "'" nil :unless '(sp-point-after-word-p))
  (sp-pair "\"" nil :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-same-p))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))

  (sp-local-pair
   'css-mode "/*" "*/" :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC")))
  (sp-local-pair '(sh-mode markdown-mode) "`" nil
    :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-local-pair '(xml-mode nxml-mode php-mode)
                 "<!--" "-->"   :post-handlers '(("| " "SPC"))))


;;
;; Autoloaded Plugins
;;

(@def-package ace-link
  :commands (ace-link-help ace-link-org))

(@def-package ace-window
  :commands ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))

(@def-package avy
  :commands (avy-goto-char-2 avy-goto-line)
  :config
  (setq avy-all-windows nil
        avy-background t))

(@def-package command-log-mode
  :commands (command-log-mode global-command-log-mode)
  :config
  (@set :popup "*command-log*" :size 40 :align 'right :noselect t)
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode t))

(@def-package emr
  :commands (emr-show-refactor-menu emr-declare-command)
  :config (emr-initialize)
  (define-key popup-menu-keymap [escape] 'keyboard-quit))

(@def-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

(@def-package goto-last-change :commands goto-last-change)

(@def-package help-fns+ ; Improved help commands
  :commands (describe-buffer describe-command describe-file
             describe-keymap describe-option describe-option-of-type))

(@def-package imenu-anywhere
  :commands (ido-imenu-anywhere ivy-imenu-anywhere helm-imenu-anywhere))

(@def-package imenu-list :commands imenu-list-minor-mode)

(@def-package pcre2el :commands rxt-quote-pcre)

(@def-package rotate-text
  :commands (rotate-text rotate-text-backward)
  :config
  (push '("true" "false") rotate-text-words))

(@def-package smart-forward
  :commands (smart-up smart-down smart-backward smart-forward))

(@def-package swiper :commands (swiper swiper-all))

(@def-package wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :config
  (@set :popup "^\\*ivy-occur counsel-ag" :size 25 :select t :regexp t)
  (setq wgrep-auto-save-buffer t)
  (advice-add 'wgrep-abort-changes :after 'doom/popup-close)
  (advice-add 'wgrep-finish-edit :after 'doom/popup-close))

(provide 'core-editor)
;;; core-editor.el ends here
