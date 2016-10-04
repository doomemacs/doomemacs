;;; core-editor.el

(global-auto-revert-mode 1)    ; revert buffers for changed files
;; Enable syntax highlighting for older emacs
(unless (bound-and-true-p global-font-lock-mode)
  (global-font-lock-mode t))

(setq-default
 ;; Formatting
 delete-trailing-lines nil
 fill-column 80
 ;; Spaces, not tabs
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50
 visual-fill-column-center-text nil
 word-wrap t
 ;; Scrolling
 hscroll-margin 1
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 0
 scroll-preserve-screen-position t
 ;; Regions
 shift-select-mode t
 ;; Whitespace
 tabify-regexp "^\t* [ \t]+"
 whitespace-line-column fill-column
 whitespace-style '(face tabs tab-mark
                    trailing indentation lines-tail)
 whitespace-display-mappings
 '((tab-mark ?\t [?› ?\t])
   (newline-mark 10 [36 10])))

;; Save point across sessions
(require 'saveplace)
(setq-default
 save-place-file (concat doom-temp-dir "/saveplace")
 save-place t)
(when (>= emacs-major-version 25)
  (save-place-mode +1))

;; Save history across sessions
(require 'savehist)
(setq savehist-file (concat doom-temp-dir "/savehist")
      savehist-save-minibuffer-history t
      savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

;; Remove text property cruft from history
(defun unpropertize-savehist ()
  (mapc (lambda (list)
          (when (boundp list)
            (set list (mapcar 'substring-no-properties (eval list)))))
        '(kill-ring minibuffer-history helm-grep-history helm-ff-history
          file-name-history read-expression-history extended-command-history
          evil-ex-history)))
(add-hook 'kill-emacs-hook    'unpropertize-savehist)
(add-hook 'savehist-save-hook 'unpropertize-savehist)

;; Keep track of recently opened files
(require 'recentf)
(setq recentf-save-file (concat doom-temp-dir "/recentf")
      recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                        "emacs\\.d/private/cache/.+" "emacs\\.d/workgroups/.+$"
                        "wg-default" "/company-statistics-cache.el$")
      recentf-max-menu-items 0
      recentf-max-saved-items 250
      recentf-auto-cleanup 600
      recentf-filename-handlers '(abbreviate-file-name))
(recentf-mode 1)

;; window config undo/redo
(setq winner-dont-bind-my-keys t)
(require 'winner)
;; Ignore all special buffers
(advice-add 'winner-window-list :filter-return 'doom*winner-window-list)
(defun doom*winner-window-list (windows)
  (-remove (lambda (win) (string-match-p "^\\s-*\\*" (buffer-name (window-buffer win))))
           windows))
(winner-mode 1)


;; Let editorconfig handle global whitespace settings
(use-package editorconfig :demand t
  :mode ("\\.?editorconfig$" . editorconfig-conf-mode)
  :config (editorconfig-mode +1)
  (push 'doom-mode editorconfig-exclude-modes)
  ;; Show whitespace in tabs indentation mode
  (add-hook! 'editorconfig-custom-hooks
    (if indent-tabs-mode (whitespace-mode +1))))


;;
;; Hooks 'n hacks
;;

(associate! makefile-gmake-mode :match "/Makefile$")
(add-hook! special-mode (setq truncate-lines nil))
;; If file is oversized...
(add-hook! find-file
  (when (> (buffer-size) 1048576)
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (visual-line-mode)))

(defadvice delete-trailing-whitespace
    (around delete-trailing-whitespace-ignore-line activate)
  "Don't delete trailing whitespace on current line, if in insert mode."
  (let ((spaces (1- (current-column)))
        (linestr (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))
    ad-do-it
    (when (and (evil-insert-state-p)
               (string-match-p "^[\s\t]*$" linestr))
      (insert linestr))))

;; Disable by default, please
(electric-indent-mode -1)
;; Smarter, keyword-based electric-indent (see `def-electric!')
(defvar doom-electric-indent-p nil)
(defvar-local doom-electric-indent-words '())
(setq electric-indent-chars '(?\n ?\^?))
(push (lambda (c)
        (when (and (eolp) doom-electric-indent-words)
          (save-excursion
            (backward-word)
            (looking-at-p
             (concat "\\<" (regexp-opt doom-electric-indent-words))))))
      electric-indent-functions)


;;
;; Plugins
;;

(use-package ace-window
  :commands ace-window
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-scope 'frame
                aw-background t))

(use-package ace-link :commands (ace-link-help ace-link-org))

(use-package avy
  :commands (avy-goto-char-2 avy-goto-line)
  :config (setq avy-all-windows nil
                avy-background t))

(use-package command-log-mode
  :commands (clm/command-log-buffer command-log-mode global-command-log-mode)
  :config (setq command-log-mode-is-global t))

(use-package dumb-jump
  :commands (dumb-jump-go dumb-jump-quick-look dumb-jump-back)
  :config
  (setq dumb-jump-default-project doom-emacs-dir)
  (dumb-jump-mode +1))

(use-package emr
  :commands (emr-show-refactor-menu emr-declare-command)
  :config
  (emr-initialize)
  (define-key popup-menu-keymap [escape] 'keyboard-quit))

(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

(use-package goto-last-change :commands goto-last-change)

(use-package hideshow
  :commands (hs-minor-mode hs-toggle-hiding hs-already-hidden-p)
  :config (setq hs-isearch-open t)
  :init
  (advice-add 'evil-toggle-fold :before 'doom*load-hs-minor-mode)
  ;; Prettify code folding in emacs
  (define-fringe-bitmap 'hs-marker [16 48 112 240 112 48 16] nil nil 'center)
  (defface hs-face '((t (:background "#ff8")))
    "Face to hightlight the ... area of hidden regions"
    :group 'hideshow)
  (defface hs-fringe-face '((t (:foreground "#888")))
    "Face used to highlight the fringe on folded regions"
    :group 'hideshow)
  (setq hs-set-up-overlay
        (lambda (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (let* ((marker-string "*")
                   (display-string (concat " " (all-the-icons-octicon "ellipsis" :v-adjust 0) " "))
                   (len (length display-string)))
              (put-text-property 0 1 'display
                                 (list 'right-fringe 'hs-marker 'hs-fringe-face)
                                 marker-string)
              (put-text-property 0 1 'face 'hs-face display-string)
              (put-text-property (1- len) len 'face 'hs-face display-string)
              (put-text-property 1 (1- len)
                                 'face `(:inherit hs-face :family ,(all-the-icons-octicon-family) :height 1.1)
                                 display-string)
              (overlay-put ov 'before-string marker-string)
              (overlay-put ov 'display display-string))))))

(use-package help-fns+ ; Improved help commands
  :commands (describe-buffer describe-command describe-file
             describe-keymap describe-option describe-option-of-type))

(use-package imenu-list
  :commands imenu-list-minor-mode
  :config
  (setq imenu-list-mode-line-format nil
        imenu-list-position 'right
        imenu-list-size 32)
  (map! :map imenu-list-major-mode-map
        :n [escape] 'doom/imenu-list-quit
        :n "RET" 'imenu-list-goto-entry
        :n "SPC" 'imenu-list-display-entry
        :n [tab] 'hs-toggle-hiding))

(use-package re-builder
  :commands (re-builder reb-mode-buffer-p)
  :init (add-hook 'reb-mode-hook 'doom|reb-cleanup)
  :config
  (evil-set-initial-state 'reb-mode 'insert)
  (setq reb-re-syntax 'string)
  (map! :map rxt-help-mode-map
        :n [escape] 'kill-buffer-and-window
        :map reb-mode-map
        :n "C-g" 'reb-quit
        :n [escape] 'reb-quit
        :n [backtab] 'reb-change-syntax))

(use-package pcre2el :commands (rxt-quote-pcre))

(use-package rotate-text
  :commands (rotate-text rotate-text-backward)
  :config (push '("true" "false") rotate-text-words))

(use-package smart-forward
  :commands (smart-up smart-down smart-left smart-right))

(use-package smartparens
  :config
  (setq sp-autowrap-region nil          ; let evil-surround handle this
        sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0
        sp-max-pair-length 5)

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
  (sp-with-modes '(xml-mode nxml-mode php-mode)
    (sp-local-pair "<!--" "-->"   :post-handlers '(("| " "SPC")))))

(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (setq smex-save-file (concat doom-temp-dir "/smex-items"))
  (smex-initialize))

(use-package swiper :commands (swiper swiper-all))

(use-package wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :config
  (def-popup! "^\\*ivy-occur counsel-ag" :align below :size 25 :select t :regexp t)
  (setq wgrep-auto-save-buffer t)
  (advice-add 'wgrep-abort-changes :after 'doom/popup-close)
  (advice-add 'wgrep-finish-edit :after 'doom/popup-close))


;;
;; Keybinding fixes
;;

;; This section is dedicated to bindings that "fix" certain keys so that they
;; behave more like vim (or how I like it).

;; Line-wise mouse selection on margin
(map! "<left-margin> <down-mouse-1>" 'doom/mouse-drag-line
      "<left-margin> <mouse-1>"      'doom/mouse-select-line
      "<left-margin> <drag-mouse-1>" 'doom/mouse-select-line)

;; Restores "dumb" indentation to the tab key. This rustles a lot of peoples'
;; jimmies, apparently, but it's how I like it.
(map! :i "<tab>"     'doom/dumb-indent
      :i "<backtab>" 'doom/dumb-dedent
      :i "<C-tab>"   'indent-for-tab-command
      :i "<A-tab>"   (λ! (insert "\t"))
      ;; No dumb-tab for lisp
      (:map lisp-mode-map        :i [remap doom/dumb-indent] 'indent-for-tab-command)
      (:map emacs-lisp-mode-map  :i [remap doom/dumb-indent] 'indent-for-tab-command)
      ;; Highjacks space/backspace to:
      ;;   a) eat spaces on either side of the cursor, if present ( | ) -> (|)
      ;;   b) allow backspace to delete space-indented blocks intelligently
      ;;   c) but do none of this when inside a string
      :i "SPC"                          'doom/inflate-space-maybe
      :i [remap delete-backward-char]   'doom/deflate-space-maybe
      :i [remap newline]                'doom/newline-and-indent
      ;; Smarter move-to-beginning-of-line
      :i [remap move-beginning-of-line] 'doom/move-to-bol
      ;; Restore bash-esque keymaps in insert mode; C-w and C-a already exist
      :i "C-e" 'doom/move-to-eol
      :i "C-u" 'doom/backward-kill-to-bol-and-indent
      ;; Fixes delete
      :i "<kp-delete>" 'delete-char
      ;; Fix osx keymappings and then some
      :i "<M-left>"   'doom/move-to-bol
      :i "<M-right>"  'doom/move-to-eol
      :i "<M-up>"     'beginning-of-buffer
      :i "<M-down>"   'end-of-buffer
      :i "<C-up>"     'smart-up
      :i "<C-down>"   'smart-down
      ;; Fix emacs motion keys
      :i "A-b"      'evil-backward-word-begin
      :i "A-w"      'evil-forward-word-begin
      :i "A-e"      'evil-forward-word-end
      ;; Textmate-esque insert-line before/after
      :i "<M-return>"    'evil-open-below
      :i "<S-M-return>"  'evil-open-above
      ;; insert lines in-place)
      :n "<M-return>"    (λ! (save-excursion (evil-insert-newline-below)))
      :n "<S-M-return>"  (λ! (save-excursion (evil-insert-newline-above)))
      ;; Make ESC quit all the things
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map)
        [escape] 'abort-recursive-edit
        "C-r" 'evil-paste-from-register)

      (:map (evil-ex-search-keymap read-expression-map)
        "C-w" 'backward-kill-word
        "C-u" 'backward-kill-sentence
        "C-b" 'backward-word)

      (:map evil-ex-completion-map "C-a" 'move-beginning-of-line)

      (:after view
        (:map view-mode-map "<escape>" 'View-quit-all))

      (:after help-mode
        (:map help-map
          ;; Remove slow/annoying help subsections
          "h" nil
          "g" nil)))

;; Fix certain keys in the terminal
(unless window-system
  (map! :map key-translation-map
        "TAB" [tab]))

;; Common unicode characters
(map! :map key-translation-map
      "A-o" (kbd "ø")
      "A-O" (kbd "Ø")
      "A--" (kbd "–")
      "A-_" (kbd "—")
      "A-8" (kbd "•")
      "A-*" (kbd "°")
      "A-p" (kbd "π"))

(provide 'core-editor)
;;; core-editor.el ends here
