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

 ;; Sane scroll settings
 scroll-margin           0
 scroll-conservatively   9999
 scroll-preserve-screen-position t

 shift-select-mode       nil

 whitespace-style '(trailing face tabs tab-mark)
 whitespace-display-mappings
 '((tab-mark   ?\t   [?| ?\t] [?\\ ?\t])
   (newline-mark 10 [36 10]))

 truncate-lines                  t      ; do not soft-wrap lines
 truncate-partial-width-windows  nil)


;; Automatic minor modes ;;;;;;;;;;;

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

(associate! text-mode           :match "/LICENSE[^/]*$")
(associate! sh-mode             :match "z\\(profile\\|login\\|logout\\)?$")
(associate! sh-mode             :match "zsh/")
(associate! applescript-mode    :match "\\.applescript$")
(associate! emacs-lisp-mode     :match "Cask$")
(associate! emacs-lisp-mode     :match "\\.el\\.gz$")
(associate! makefile-gmake-mode :match "/Makefile$")
(associate! nxml-mode           :match "\\.plist$")

(add-hook! help-mode       'visual-line-mode)
(add-hook! python-mode     'electric-indent-local-mode)
(add-hook! makefile-mode   'narf|enable-tabs) ; Use normal tabs in makefiles
(add-hook! before-save     'delete-trailing-whitespace)

(add-hook! eldoc-mode (diminish 'eldoc-mode " ?"))

;; Line wrapping
(add-hook! text-mode 'narf|enable-hard-wrap)
(add-hook! prog-mode 'narf|enable-comment-hard-wrap)
(add-hook! auto-fill-mode (diminish 'auto-fill-function))

;; If file is oversized...
(add-hook! find-file
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (visual-line-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-whitespace-mode 1)  ; Show whitespace
(global-font-lock-mode t)      ; Enable syntax highlighting for older emacs
(global-auto-revert-mode 1)    ; revert buffers for changed files
(electric-indent-mode -1)

;; window config undo/redo
(winner-mode 1)
(add-hook! after-init (setq winner-boring-buffers narf-ignore-buffers))


;; Plugins ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package undo-tree
  :defer t
  :config
  ;; Shut up undo-tree's constant complaining: http://youtu.be/Z6woIRLnbmE
  (defadvice undo-tree-load-history-hook (around undo-tree-load-history-shut-up activate)
    (shut-up! ad-do-it))
  (defadvice undo-tree-save-history-hook (around undo-tree-save-history-shut-up activate)
    (shut-up! ad-do-it)))

(use-package ace-jump-mode
  :functions (ace-jump-char-category ace-jump-do)
  :commands  (ace-jump-line-mode ace-jump-char-mode
              ace-jump-word-mode ace-jump-two-chars-mode)
  :init (setq ace-jump-mode-scope 'window
              ace-jump-mode-gray-background t)
  :config
  (defun ace-jump-two-chars-mode (&optional query-char query-char-2)
    "AceJump two chars mode"
    (interactive)

    (evil-half-cursor)
    (setq query-char (or query-char (read-char ">")))
    (setq query-char-2 (or query-char-2 (read-char (concat ">" (string query-char)))))
    (if (eq (ace-jump-char-category query-char) 'other)
        (error "[AceJump] Non-printable character"))

    ;; others : digit , alpha, punc
    (setq ace-jump-query-char query-char)
    (setq ace-jump-current-mode 'ace-jump-char-mode)
    (ace-jump-do (regexp-quote (concat (char-to-string query-char)
                                       (char-to-string query-char-2))))))

(use-package ace-link
  :commands (ace-link-info ace-link-help ace-link-compilation ace-link-custom ace-link-org)
  :init
  (bind!
   (:after help-mode :map help-mode-map         :m "go" 'ace-link-help)
   (:after compile   :map compilation-mode-map  :m "go" 'ace-link-compilation)
   (:after info      :map Info-mode-map         :m "go" 'ace-link-info)
   (:after org       :map org-mode-map          :m "go" 'ace-link-org)))

(use-package ace-window
  :commands ace-window
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-scope 'frame
                aw-background nil))

;; (use-package emr
;;   :commands (emr-initialize emr-show-refactor-menu emr-declare-command)
;;   :bind (:map popup-menu-keymap [escape] 'keyboard-quit))

(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

(use-package goto-last-change :commands goto-last-change)

(use-package hl-todo
  :commands hl-todo-mode
  :init
  (add-hook! prog-mode 'hl-todo-mode)
  (defvar hl-todo-keyword-faces
    '(("TODO" . "#cc9393")
      ("NOTE" . "#d0bf8f")
      ("FIXME" . "#cc9393"))))

(use-package hideshow
  :commands (hs-minor-mode hs-toggle-hiding hs-already-hidden-p)
  :diminish hs-minor-mode)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook! (emacs-lisp-mode js2-mode scss-mode) 'rainbow-delimiters-mode)
  :config (setq rainbow-delimiters-outermost-only-face-count 1))

(use-package rotate-text   :commands (rotate-word-at-point rotate-region))

(use-package smart-forward :commands (smart-up smart-down smart-left smart-right))

(use-package smartparens
  :diminish smartparens-mode
  :functions sp-insert-pair
  :commands (smartparens-global-mode
             sp-pair
             sp-local-pair
             sp-point-in-string-or-comment)
  :init (add-hook! evil-insert-state-entry (unless smartparens-global-mode (smartparens-global-mode 1)))
  :config
  (setq blink-matching-paren t
        sp-autowrap-region nil          ; let evil-surround handle this
        sp-highlight-pair-overlay nil
        sp-show-pair-delay 0)

  (require 'smartparens-config)

  ;; Handle newlines + spaces
  (sp-pair "{" "}"
           :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" ")"
           :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  ;; Auto-close more conservatively
  (sp-pair "[" nil  :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "'" nil  :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "\"" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-before-same-p))
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
  (sp-with-modes '(markdown-mode org-mode)
    (sp-local-pair "*" "*" :unless '(sp-point-after-bol-p sp-point-before-same-p sp-point-after-same-p)))

  (after! yasnippet
    (advice-add 'yas-expand :before 'sp-remove-active-pair-overlay)))

(use-package smex
  :commands (smex smex-major-mode-commands smex-initialize smex-update)
  :init (setq smex-save-file (! (concat narf-temp-dir "smex-items")))
  :config (smex-initialize)

  ;; Hook up smex to auto-update, rather than update on every run
  (defun smex-update-after-load (unused)
    (when (boundp 'smex-cache) (smex-update)))
  (add-hook 'after-load-functions 'smex-update-after-load))

(provide 'core-editor)
;;; core-editor.el ends here
