;;; core-editor.el
;; see lib/editor-defuns.el

;;;; Editor behavior ;;;;;;;;;;;;;;;;
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

 ;; Sane scroll settings
 scroll-margin           0
 scroll-conservatively   1001
 scroll-preserve-screen-position t

 hscroll-step 1
 hscroll-margin 1

 shift-select-mode       t
 tabify-regexp "^\t* [ \t]+"
 whitespace-style '(face tabs tab-mark trailing newline indentation newline-mark)
 whitespace-display-mappings
 '((tab-mark   ?\t   [?> ?\t])
   (newline-mark 10 [36 10])))

(require 'saveplace)
(setq save-place-file (concat narf-temp-dir "saveplace")
      save-place t)
(when (>= emacs-major-version 25)
  (save-place-mode +1))


;; Automatic minor modes ;;;;;;;;;;;

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

(add-hook! find-file 'narf|enable-minor-mode-maybe)


;; Modes 'n hooks ;;;;;;;;;;;;;;;;;;;

(associate! applescript-mode    :match "\\.applescript$")
(associate! emacs-lisp-mode     :match "\\(/Cask\\|\\.\\(el\\|gz\\)\\)$")
(associate! makefile-gmake-mode :match "/Makefile$")
(associate! nxml-mode           :match "\\.plist$")
(associate! conf-mode           :match "/\\.?editorconfig$")

(add-hook! help-mode      'visual-line-mode)
(add-hook! special-mode   (setq truncate-lines nil))
(add-hook! python-mode    'electric-indent-local-mode)
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
    (when (string-match-p "^[\s\t]*$" linestr)
      (insert linestr))))

;; If file is oversized...
(add-hook! find-file
  (when (> (buffer-size) 1048576)
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (visual-line-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-whitespace-mode 1)  ; Show whitespace
;; (global-font-lock-mode t)   ; Enable syntax highlighting for older emacs
(electric-indent-mode -1)      ; on by default
(global-auto-revert-mode 1)    ; revert buffers for changed files

;; window config undo/redo
(setq winner-dont-bind-my-keys t)
(winner-mode 1)
(add-hook! after-init
  (setq winner-boring-buffers narf-ignore-buffers))

;; Extra modes ;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vimrc-mode :mode ("/\\.?g?vimrc$" "\\.vim$" "/\\.vim/rc/.+$"))

;; Data formats
(use-package yaml-mode :mode "\\.ya?ml$")
(use-package toml-mode :mode "\\.toml$")
(use-package json-mode :mode "\\.js\\(on\\|hintrc\\)$")

;; Plugins ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package rotate-text
  :commands (rotate-text rotate-text-backward)
  :init
  (add-hook! (emacs-lisp-mode lisp-mode)
    (setq rotate-text-local-symbols
          '(("t" "nil")
            ("let" "let*")
            ("when" "unless")
            ("append" "prepend")
            ("add-hook" "add-hook!" "remove-hook")))))

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
  (add-hook 'evil-replace-state-exit-hook 'turn-on-smartparens-mode)

  ;; Auto-close more conservatively
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
                   :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
                   :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
                   :unless '(sp-point-before-word-p sp-point-before-same-p))

  ;; Support for generics/templates
  (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
    (sp-local-pair "<" ">" :when '(sp-point-after-word-p) :unless '(sp-point-before-same-p)))

  (sp-local-pair '(sh-mode markdown-mode) "`" "`" :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-local-pair 'markdown-mode "```" "```" :post-handlers '(("||\n[i]" "RET")) :unless '(sp-point-before-word-p sp-point-before-same-p))

  (sp-local-pair '(scss-mode css-mode) "/*" "*/" :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC")))

  (defun sp-insert-yasnippet (id action context)
    (forward-char -1)
    (if (sp-point-after-bol-p id action context)
        (yas-expand-from-trigger-key)
      (forward-char)))

  (sp-with-modes '(sh-mode)
    (sp-local-pair "case"  "" :when '(("SPC")) :post-handlers '((:add sp-insert-yasnippet)) :actions '(insert))
    (sp-local-pair "if"    "" :when '(("SPC")) :post-handlers '((:add sp-insert-yasnippet)) :actions '(insert))
    (sp-local-pair "for"   "" :when '(("SPC")) :post-handlers '((:add sp-insert-yasnippet)) :actions '(insert))
    (sp-local-pair "elif"  "" :when '(("SPC")) :post-handlers '((:add sp-insert-yasnippet)) :actions '(insert))
    (sp-local-pair "while" "" :when '(("SPC")) :post-handlers '((:add sp-insert-yasnippet)) :actions '(insert)))

  (sp-with-modes '(c-mode c++-mode objc-mode php-mode java-mode)
    (sp-local-pair "/*" "" :post-handlers '(("||\n[i]*/" "RET") ("| */" "SPC")))

    ;; Doxygen blocks
    (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

  (sp-with-modes '(org-mode)
    (sp-local-pair "\\[" "\\]" :post-handlers '(("| " "SPC")))
    (sp-local-pair "\\(" "\\)" :post-handlers '(("| " "SPC")))
    (sp-local-pair "$$" "$$" :post-handlers '((:add " | ")))
    (sp-local-pair "{" nil))

  ;; Markup languages
  (sp-with-modes '(xml-mode nxml-mode php-mode)
    (sp-local-pair "<!--" "-->"   :post-handlers '(("| " "SPC"))))
  (sp-with-modes '(web-mode php-mode)
    (sp-local-pair "<?" "?>"      :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-local-pair "<?php " " ?>" :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))
  (sp-with-modes '(web-mode)
    (sp-local-pair "{{!--" "--}}" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-local-pair "<%" "%>"      :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-local-pair "{!!" "!!}"    :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-local-pair "{#" "#}"      :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))))

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
