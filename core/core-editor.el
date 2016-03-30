;;; core-editor.el

(setq-default
 ;; spaces instead of tabs
 indent-tabs-mode        nil
 tab-always-indent       t
 tab-width               4

 require-final-newline   t
 delete-trailing-lines   nil
 fill-column             90
 line-spacing            0
 word-wrap               t
 truncate-lines                  t
 truncate-partial-width-windows  50

 visual-fill-column-center-text nil
 confirm-nonexistent-file-or-buffer nil

 ;; Sane scroll settings
 scroll-margin           0
 scroll-conservatively   1001
 scroll-preserve-screen-position t
 hscroll-step   1
 hscroll-margin 1

 shift-select-mode t
 tabify-regexp "^\t* [ \t]+"
 whitespace-line-column fill-column
 whitespace-style '(face tabs tab-mark newline newline-mark
                    trailing indentation lines-tail)
 whitespace-display-mappings
 '((tab-mark   ?\t   [?> ?\t])
   (newline-mark 10 [36 10])))

(require 'saveplace)
(setq-default
 save-place-file (concat narf-temp-dir "/saveplace")
 save-place t)
(when (>= emacs-major-version 25)
  (save-place-mode +1))


;;
;; Automatic minor modes
;;

(defvar narf-auto-minor-mode-alist '()
  "Alist of filename patterns vs corresponding minor mode functions, see
`auto-mode-alist'. All elements of this alist are checked, meaning you can
enable multiple minor modes for the same regexp.")

(defun narf|enable-minor-mode-maybe ()
  "Check file name against `narf-auto-minor-mode-alist'."
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist narf-auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(add-hook 'find-file-hook 'narf|enable-minor-mode-maybe)


;;
;; Modes 'n hooks
;;

(associate! emacs-lisp-mode     :match "\\(/Cask\\|\\.\\(el\\|gz\\)\\)$")
(associate! makefile-gmake-mode :match "/Makefile$")
(associate! nxml-mode           :match "\\.plist$")
(associate! conf-mode           :match "/\\.?editorconfig$")

(add-hook! special-mode   (setq truncate-lines nil))
(add-hook! change-major-mode-hook
  (when indent-tabs-mode (whitespace-mode +1)))

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

;; If file is oversized...
(add-hook! find-file
  (when (> (buffer-size) 1048576)
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (visual-line-mode)))

;; Smarter electric-indent
(electric-indent-mode -1)      ; on by default
(defvar narf-electric-indent-words '())
(make-variable-buffer-local 'narf-electric-indent-words)
(setq electric-indent-chars '(?\n ?\^?))
(defvar narf-electric-indent-p nil)
(push (lambda (c)
        (when (eolp)
          (save-excursion
            (backward-word)
            (looking-at-p (concat "\\<" (regexp-opt narf-electric-indent-words))))))
      electric-indent-functions)

;;
;; (global-whitespace-mode -1) ; Show whitespace
(global-visual-line-mode 1)    ; wrap buffers
(global-auto-revert-mode 1)    ; revert buffers for changed files
;; Enable syntax highlighting for older emacs
(unless (bound-and-true-p global-font-lock-mode)
  (global-font-lock-mode t))

;; window config undo/redo
(setq winner-dont-bind-my-keys t)
(winner-mode 1)
(add-hook! after-init
  (setq winner-boring-buffers narf-ignore-buffers))


;;
;; Extra modes
;;

(use-package vimrc-mode :mode ("/\\.?g?vimrc$" "\\.vim$" "/\\.vim/rc/.+$"))
;; Data formats
(use-package yaml-mode :mode "\\.ya?ml$")
(use-package toml-mode :mode "\\.toml$")
(use-package json-mode :mode "\\.js\\(on\\|hintrc\\)$")
;; Configuration formats
(use-package dockerfile-mode :mode "/Dockerfile$"
  :config
  (define-docset! dockerfile-mode "docker")
  (define-builder! dockerfile-mode dockerfile-build-buffer "Dockerfile"))


;;
;; Plugins
;;

(use-package avy
  :commands (avy-goto-char-2 avy-goto-line)
  :config (setq avy-all-windows nil
                avy-background t))

(use-package ace-window
  :commands ace-window
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-scope 'frame
                aw-background t))

(use-package editorconfig
  :config
  ;; Don't affect lisp indentation (just `tab-width')
  (setq editorconfig-indentation-alist (delq (assq 'emacs-lisp-mode editorconfig-indentation-alist) editorconfig-indentation-alist))
  (editorconfig-mode +1))

(use-package emr
  :commands (emr-initialize emr-show-refactor-menu emr-declare-command)
  :config (define-key popup-menu-keymap [escape] 'keyboard-quit))

(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

(use-package goto-last-change
  :commands goto-last-change)

(use-package hideshow
  :commands (hs-minor-mode hs-toggle-hiding hs-already-hidden-p)
  :config (setq hs-isearch-open t)
  :init
  (advice-add 'evil-toggle-fold :before 'narf*load-hs-minor-mode)

  ;; Prettify code folding in emacs ;;;;;;
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
            (let* ((marker-string "*fringe-dummy*")
                   (marker-length (length marker-string))
                   (display-string (format " ... " (count-lines (overlay-start ov)
                                                                (overlay-end ov)))))
              (put-text-property 0 marker-length 'display
                                 (list 'right-fringe 'hs-marker 'hs-fringe-face) marker-string)
              (put-text-property 0 (length display-string) 'face 'hs-face display-string)
              (overlay-put ov 'before-string marker-string)
              (overlay-put ov 'display display-string))))))

(use-package rotate-text
  :commands (rotate-text rotate-text-backward)
  :init
  (add-hook! (emacs-lisp-mode lisp-mode)
    (setq rotate-text-local-symbols
          '(("t" "nil")
            ("let" "let*")
            ("when" "unless")
            ("append" "prepend")
            ("advice-add" "advice-remove")
            ("add-hook" "add-hook!" "remove-hook"))))
  :config
  (add-to-list 'rotate-text-words '("true" "false")))

(use-package smart-forward :commands (smart-up smart-down smart-left smart-right))

(use-package smartparens
  :functions sp-insert-pair
  :config
  (setq sp-autowrap-region nil          ; let evil-surround handle this
        sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0)

  (smartparens-global-mode 1)
  (require 'smartparens-config)

  ;; Smartparens interferes with Replace mode
  (add-hook 'evil-replace-state-entry-hook 'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook  'turn-on-smartparens-mode)

  ;; Auto-close more conservatively
  (sp-pair "'" nil :unless '(sp-point-after-word-p))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
                   :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
                   :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
                   :unless '(sp-point-before-word-p sp-point-before-same-p))

  (sp-local-pair 'css-mode "/*" "*/" :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC")))
  (sp-local-pair '(sh-mode markdown-mode) "`" "`" :unless '(sp-point-before-word-p sp-point-before-same-p))

  ;; Markup languages
  (sp-with-modes '(xml-mode nxml-mode php-mode)
    (sp-local-pair "<!--" "-->"   :post-handlers '(("| " "SPC")))))

(use-package help-fns+ ; Improved help commands
  :commands (describe-buffer describe-command describe-file
             describe-keymap describe-option describe-option-of-type))

(use-package re-builder
  :commands (re-builder reb-mode-buffer-p)
  :init (add-hook! reb-mode 'narf|reb-cleanup)
  :config
  (setq reb-re-syntax 'string)
  (evil-set-initial-state 'reb-mode 'insert)

  (map! :map rxt-help-mode-map :n [escape] 'kill-buffer-and-window)
  (map! :map reb-mode-map
        :n "C-g"        'reb-quit
        :n [escape]     'reb-quit
        :n [backtab]    'reb-change-syntax))

(provide 'core-editor)
;;; core-editor.el ends here
