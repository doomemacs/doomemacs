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
 fill-column             80
 word-wrap t
 truncate-lines                  t
 truncate-partial-width-windows  50
 line-spacing 0

 ;; Sane scroll settings
 scroll-margin           0
 scroll-conservatively   1001
 scroll-preserve-screen-position t

 hscroll-step 5
 hscroll-margin 6

 shift-select-mode       t
 tabify-regexp "^\t* [ \t]+"
 whitespace-style '(face tabs tab-mark)
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

(associate! sh-mode             :match "/\\.?z\\(profile\\|login\\|logout\\|shrc\\|shenv\\)?$")
(associate! sh-mode             :match "/\\.?zsh/")
(associate! applescript-mode    :match "\\.applescript$")
(associate! emacs-lisp-mode     :match "Cask$")
(associate! emacs-lisp-mode     :match "\\.el\\.gz$")
(associate! makefile-gmake-mode :match "/Makefile$")
(associate! nxml-mode           :match "\\.plist$")

(add-hook! help-mode      'visual-line-mode)
(add-hook! python-mode    'electric-indent-local-mode)
(add-hook! makefile-mode  'narf|enable-tabs) ; Use normal tabs in makefiles
(add-hook! before-save    'delete-trailing-whitespace)
(add-hook! prog-mode      'narf|enable-comment-hard-wrap)
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
(global-auto-revert-mode 1)    ; revert buffers for changed files
(electric-indent-mode -1)      ; on by default

;; window config undo/redo
(setq winner-dont-bind-my-keys t)
(winner-mode 1)
(add-hook! after-init
  (setq winner-boring-buffers narf-ignore-buffers))


;; Plugins ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package undo-tree
  :config
  (defalias 'redo #'undo-tree-redo)
  (defalias 'undo #'undo-tree-undo)
  ;; http://youtu.be/Z6woIRLnbmE
  (defadvice undo-tree-load-history-hook
      (around undo-tree-load-history-shut-up activate)
    (shut-up! ad-do-it))
  (defadvice undo-tree-save-history-hook
      (around undo-tree-save-history-shut-up activate)
    (shut-up! ad-do-it)))

(use-package avy
  :commands (avy-goto-char-2 avy-goto-line)
  :config (setq avy-all-windows nil
                avy-background t))

(use-package ace-window
  :commands ace-window
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-scope 'frame
                aw-background t))

(use-package emr
  :commands (emr-initialize emr-show-refactor-menu emr-declare-command)
  :config (define-key popup-menu-keymap [escape] 'keyboard-quit))

(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

(use-package goto-last-change
  :commands goto-last-change)

(use-package rotate-text :commands (rotate-word-at-point rotate-region))

(use-package smart-forward :commands (smart-up smart-down smart-left smart-right))

(use-package smartparens
  :functions sp-insert-pair
  :config
  (setq sp-autowrap-region nil          ; let evil-surround handle this
        sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement t
        sp-show-pair-delay 0)

  (smartparens-global-mode 1)
  (require 'smartparens-config)

  ;; Handle newlines + spaces
  (sp-pair "{" "}" :post-handlers '(("||\n[i]" "RET") ("| " " "))
                   :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" ")" :post-handlers '(("||\n[i]" "RET") ("| " " "))
                   :unless '(sp-point-before-word-p sp-point-before-same-p))

  ;; Auto-close more conservatively
  (sp-pair "[" nil  :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "'" nil  :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "\"" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-before-same-p))
  (sp-local-pair 'markdown-mode "```" "```" :post-handlers '(("||\n[i]" "RET")))
  (sp-with-modes '(enh-ruby-mode python-mode shell-script-mode markdown-mode org-mode)
    (sp-local-pair "`" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-before-same-p)))
  (sp-with-modes '(json-mode js2-mode ruby-mode enh-ruby-mode python-mode)
    (sp-local-pair "[" nil :post-handlers '(("||\n[i]" "RET"))))
  (sp-with-modes '(c-mode c++-mode objc-mode java-mode scss-mode css-mode php-mode)
    (sp-local-pair "/* " " */" :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET"))))
  (sp-with-modes '(c-mode c++-mode objc-mode java-mode) ; Support for generics
    (sp-local-pair "<" ">" :when '(sp-point-after-word-p) :unless '(sp-point-before-same-p)))
  (sp-with-modes '(objc-mode scss-mode css-mode)
    (sp-local-pair "/*\n" "\n */" :post-handlers '(("||[i]" "RET"))))
  (sp-with-modes '(c-mode c++-mode php-mode java-mode)
    (sp-local-pair "/*" "" :post-handlers '((" ||\n[i]*/" "RET"))))
  (sp-with-modes '(org-mode)
    (sp-local-pair "\\[" "\\]")
    (sp-local-pair "\\(" "\\)")
    (sp-local-pair "$$" "$$")
    (sp-local-pair "{" nil))

  (after! yasnippet
    (advice-add 'yas-expand :before 'sp-remove-active-pair-overlay))

  (after! web-mode
    (add-hook! web-mode (setq web-mode-enable-auto-pairing nil))
    (defun sp-web-mode-is-code-context (id action context)
      (when (and (eq action 'insert)
                 (not (or (get-text-property (point) 'part-side)
                          (get-text-property (point) 'block-side))))
        t))
    (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))))

(use-package help-fns+ ; Improved help commands
  :commands (describe-buffer describe-command describe-file
             describe-keymap describe-option describe-option-of-type))

(use-package guide-key
  :config
  (setq guide-key/guide-key-sequence '("," "\\")
        guide-key/recursive-key-sequence-flag t
        guide-key/popup-window-position 'bottom)
  (guide-key-mode 1))  ; Enable guide-key-mode

(provide 'core-editor)
;;; core-editor.el ends here
