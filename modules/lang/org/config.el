;;; lang/org/config.el -*- lexical-binding: t; -*-

(defvar +org-dir (expand-file-name "~/org/")
  "The directory where org files are kept.")

;; Sub-modules
(if (featurep! +attach)  (load! "+attach"))
(if (featurep! +babel)   (load! "+babel"))
(if (featurep! +capture) (load! "+capture"))
(if (featurep! +export)  (load! "+export"))
(if (featurep! +present) (load! "+present"))
;; TODO (if (featurep! +publish) (load! "+publish"))


;;
;; Plugins
;;

;; `toc-org'
(setq toc-org-hrefify-default "org")

(def-package! evil-org
  :when (featurep! :feature evil)
  :hook (org-mode . evil-org-mode)
  :hook (org-load . evil-org-set-key-theme)
  :init
  (setq evil-org-key-theme '(navigation insert textobjects))
  (add-hook 'org-load-hook #'+org|setup-evil))

(def-package! evil-org-agenda
  :when (featurep! :feature evil)
  :after org-agenda
  :config (evil-org-agenda-set-keys))


;;
;; Bootstrap
;;

(add-hook! 'org-load-hook
  #'(org-crypt-use-before-save-magic
     +org|setup-ui
     +org|setup-popups-rules
     +org|setup-agenda
     +org|setup-keybinds
     +org|setup-hacks))

(add-hook! 'org-mode-hook
  #'(doom|disable-line-numbers  ; org doesn't really need em
     org-bullets-mode           ; "prettier" bullets
     org-indent-mode            ; margin-based indentation
     toc-org-enable             ; auto-table of contents
     visual-line-mode           ; line wrapping

     +org|enable-auto-reformat-tables
     +org|enable-auto-update-cookies
     +org|smartparens-compatibility-config
     +org|unfold-to-2nd-level-or-point
     +org|show-paren-mode-compatibility))

(after! org
  (defvaralias 'org-directory '+org-dir))


;;
;; `org-mode' hooks
;;

(defun +org|unfold-to-2nd-level-or-point ()
  "My version of the 'overview' #+STARTUP option: expand first-level headings.
Expands the first level, but no further. If point was left somewhere deeper,
unfold to point on startup."
  (unless org-agenda-inhibit-startup
    (when (eq org-startup-folded t)
      (outline-hide-sublevels 2))
    (when (outline-invisible-p)
      (ignore-errors
        (save-excursion
          (outline-previous-visible-heading 1)
          (org-show-subtree))))))

(defun +org|smartparens-compatibility-config ()
  "Instruct `smartparens' not to impose itself in org-mode."
  (after! smartparens
    (defun +org-sp-point-in-checkbox-p (_id action _context)
      (and (eq action 'insert)
           (sp--looking-at-p "\\s-*]")))
    (defun +org-sp-point-at-bol-p (_id action _context)
      (and (eq action 'insert)
           (eq (char-before) ?*)
           (sp--looking-back-p "^\\**" (line-beginning-position))))

    ;; make delimiter auto-closing a little more conservative
    (sp-with-modes 'org-mode
      (sp-local-pair "*" nil :unless '(:add sp-point-before-word-p +org-sp-point-at-bol-p))
      (sp-local-pair "_" nil :unless '(:add sp-point-before-word-p))
      (sp-local-pair "/" nil :unless '(:add sp-point-before-word-p +org-sp-point-in-checkbox-p))
      (sp-local-pair "~" nil :unless '(:add sp-point-before-word-p))
      (sp-local-pair "=" nil :unless '(:add sp-point-before-word-p)))))

(defun +org|enable-auto-reformat-tables ()
  "Realign tables & update formulas when exiting insert mode (`evil-mode')."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org|realign-table-maybe nil t)
    (add-hook 'evil-replace-state-exit-hook #'+org|realign-table-maybe nil t)
    (advice-add #'evil-replace :after #'+org*realign-table-maybe)))

(defun +org|enable-auto-update-cookies ()
  "Update statistics cookies when saving or exiting insert mode (`evil-mode')."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org|update-cookies nil t))
  (add-hook 'before-save-hook #'+org|update-cookies nil t))

(defun +org|show-paren-mode-compatibility ()
  "`show-paren-mode' causes flickering with indentation margins made by
`org-indent-mode', so we simply turn off show-paren-mode altogether."
  (set (make-local-variable 'show-paren-mode) nil))


;;
;; `org-load' hooks
;;

(defun +org|setup-agenda ()
  (setq-default
   org-agenda-dim-blocked-tasks nil
   org-agenda-files (ignore-errors (directory-files +org-dir t "\\.org$" t))
   org-agenda-inhibit-startup t
   org-agenda-skip-unavailable-files t)
  ;; Move the agenda to show the previous 3 days and the next 7 days for a bit
  ;; better context instead of just the current week which is a bit confusing
  ;; on, for example, a sunday
  (setq org-agenda-span 10
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d"))

(defun +org|setup-popups-rules ()
  "Defines popup rules for org-mode (does nothing if :ui popup is disabled)."
  (set! :popups
    '("^\\*\\(?:Agenda Com\\|Calendar\\|Org \\(?:Links\\|Export Dispatcher\\|Select\\)\\)"
      ((slot . -1) (vslot . -1) (size . +popup-shrink-to-fit))
      ((transient . 0)))
    '("^\\*Org Agenda"
      ((size . 0.35))
      ((select . t) (transient)))
    '("^\\*Org Src"
      ((size . 0.3))
      ((quit) (select . t)))
    '("^CAPTURE.*\\.org$"
      ((size . 0.2))
      ((quit) (select . t)))))

(defun +org|setup-ui ()
  "Configures the UI for `org-mode'."
  (setq-default
   org-adapt-indentation nil
   org-cycle-include-plain-lists t
   org-cycle-separator-lines 1
   org-entities-user
   '(("flat"  "\\flat" nil "" "" "266D" "♭")
     ("sharp" "\\sharp" nil "" "" "266F" "♯"))
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-footnote-auto-label 'plain
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars t
   org-hide-leading-stars-before-indent-mode t
   org-image-actual-width nil
   org-indent-indentation-per-level 2
   org-indent-mode-turns-on-hiding-stars t
   org-list-description-max-indent 4
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts t
   org-priority-faces
   `((?a . ,(face-foreground 'error))
     (?b . ,(face-foreground 'warning))
     (?c . ,(face-foreground 'success)))
   org-startup-folded t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column 0
   org-todo-keywords
   '((sequence "[ ](t)" "[-](p)" "[?](m)" "|" "[X](d)")
     (sequence "TODO(T)" "|" "DONE(D)")
     (sequence "NEXT(n)" "ACTIVE(a)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
   org-use-sub-superscripts '{}
   outline-blank-line t

   ;; LaTeX previews are too small and usually render to light backgrounds, so
   ;; this enlargens them and ensures their background (and foreground) match
   ;; the current theme.
   org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")
   org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
   org-format-latex-options
   (plist-put org-format-latex-options
              :background (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                              'default)
                                          :background nil t)))

  ;; Custom links
  (setq org-link-abbrev-alist
        '(("github"      . "https://github.com/%s")
          ("youtube"     . "https://youtube.com/watch?v=%s")
          ("google"      . "https://google.com/search?q=")
          ("gimages"     . "https://google.com/images?q=%s")
          ("gmap"        . "https://maps.google.com/maps?q=%s")
          ("duckduckgo"  . "https://duckduckgo.com/?q=%s")
          ("wolfram"     . "https://wolframalpha.com/input/?i=%s")
          ("doom-repo"   . "https://github.com/hlissner/doom-emacs/%s")))

  (defun +org--relpath (path root)
    (if (and buffer-file-name (file-in-directory-p buffer-file-name root))
        (file-relative-name path)
      path))

  ;; highlight broken links
  (org-link-set-parameters
   "file"
   :face (lambda (path)
           (unless (file-remote-p path)
             (if (file-exists-p path) 'org-link 'error))))

  (eval-when-compile
    (defmacro def-org-file-link! (key dir)
      `(org-link-set-parameters
        ,key
        :complete (lambda () (+org--relpath (+org-link-read-file ,key ,dir) ,dir))
        :follow   (lambda (link) (find-file (expand-file-name link ,dir)))
        :face     (lambda (link)
                    (if (file-exists-p (expand-file-name link ,dir))
                        'org-link
                      'error)))))

  (def-org-file-link! "org" +org-dir)
  (def-org-file-link! "doom" doom-emacs-dir)
  (def-org-file-link! "doom-docs" doom-docs-dir)
  (def-org-file-link! "doom-modules" doom-modules-dir)

  ;; Update UI when theme is changed
  (add-hook 'doom-load-theme-hook #'+org|setup-ui))

(defun +org|setup-keybinds ()
  "Sets up org-mode and evil keybindings. Tries to fix the idiosyncrasies
between the two."
  (defun +org|remove-occur-highlights ()
    "Remove org occur highlights on ESC in normal mode."
    (when org-occur-highlights
      (org-remove-occur-highlights)
      t))
  (add-hook 'doom-escape-hook #'+org|remove-occur-highlights)

  ;; C-a & C-e act like `doom/backward-to-bol-or-indent' and
  ;; `doom/forward-to-last-non-comment-or-eol', but with more org awareness.
  (setq org-special-ctrl-a/e t)

  (add-hook! 'org-tab-first-hook #'(+org|indent-maybe +org|yas-expand-maybe))

  ;; Tell `doom/delete-backward-char' to respect org tables
  (add-hook 'doom-delete-backward-functions #'+org|delete-backward-char)

  (map! :map org-mode-map
        "C-c C-S-l" #'+org/remove-link
        "C-c C-i"   #'org-toggle-inline-images
        [remap doom/backward-to-bol-or-indent]          #'org-beginning-of-line
        [remap doom/forward-to-last-non-comment-or-eol] #'org-end-of-line))

(defun +org|setup-evil ()
  (require 'evil-org)

  (add-hook 'org-tab-first-hook #'+org|toggle-only-current-fold t)

  ;; Fix newline-and-indent behavior in src blocks
  (advice-add #'org-return-indent :after #'+org*return-indent-in-src-blocks)

  (map! :map outline-mode-map
        :n "^" nil
        :n [backtab] nil
        :n "C-j" nil
        :n "C-k" nil
        :n "]" nil
        :n "[" nil

        :map evil-org-mode-map
        :i [backtab] #'+org/dedent
        ;; navigate table cells (from insert-mode)
        :i  "C-l"   #'+org/table-next-field
        :i  "C-h"   #'+org/table-previous-field
        :i  "C-k"   #'+org/table-previous-row
        :i  "C-j"   #'+org/table-next-row
        ;; expand tables (or shiftmeta move)
        :ni "C-S-l" #'+org/table-append-field-or-shift-right
        :ni "C-S-h" #'+org/table-prepend-field-or-shift-left
        :ni "C-S-k" #'org-metaup
        :ni "C-S-j" #'org-metadown
        ;; more intuitive RET keybinds
        :i  "RET"   #'org-return-indent
        :n  "RET"   #'+org/dwim-at-point
        :ni [M-return]   (λ! (+org/insert-item 'below))
        :ni [S-M-return] (λ! (+org/insert-item 'above))
        ;; more org-ish vim motion keys
        :m  "]]"  (λ! (org-forward-heading-same-level nil) (org-beginning-of-line))
        :m  "[["  (λ! (org-backward-heading-same-level nil) (org-beginning-of-line))
        :m  "]h"  #'org-next-visible-heading
        :m  "[h"  #'org-previous-visible-heading
        :m  "]l"  #'org-next-link
        :m  "[l"  #'org-previous-link
        :m  "]s"  #'org-babel-next-src-block
        :m  "[s"  #'org-babel-previous-src-block
        :m  "^"   #'evil-org-beginning-of-line
        :m  "0"   (λ! (let ((visual-line-mode)) (org-beginning-of-line)))
        :n  "gQ"  #'org-fill-paragraph
        ;; sensible code-folding vim keybinds
        :n  "za"  #'+org/toggle-fold
        :n  "zA"  #'org-shifttab
        :n  "zc"  #'outline-hide-subtree
        :n  "zC"  (λ! (outline-hide-sublevels 1))
        :n  "zd"  (lambda (&optional arg) (interactive "p") (outline-hide-sublevels (or arg 3)))
        :n  "zm"  (λ! (outline-hide-sublevels 1))
        :n  "zo"  #'outline-show-subtree
        :n  "zO"  #'outline-show-all
        :n  "zr"  #'outline-show-all

        :localleader
        :n "d" #'org-deadline
        :n "t" #'org-todo
        (:desc "clock" :prefix "c"
          :n "c" #'org-clock-in
          :n "C" #'org-clock-out
          :n "g" #'org-clock-goto
          :n "G" (λ! (org-clock-goto 'select))
          :n "x" #'org-clock-cancel)))

(defun +org|setup-hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (map-put org-link-frame-setup 'file #'find-file)

  ;; Let OS decide what to do with files when opened
  (setq org-file-apps
        `(("pdf" . default)
          ("\\.x?html?\\'" . default)
          (auto-mode . emacs)
          (directory . emacs)
          (t . ,(cond (IS-MAC "open -R \"%s\"")
                      (IS-LINUX "xdg-open \"%s\"")))))

  ;; Don't clobber recentf or current workspace with agenda files
  (defun +org|exclude-agenda-buffers-from-workspace ()
    (let (persp-autokill-buffer-on-remove)
      (persp-remove-buffer org-agenda-new-buffers (get-current-persp) nil)))
  (add-hook 'org-agenda-finalize-hook #'+org|exclude-agenda-buffers-from-workspace)

  (defun +org*exclude-agenda-buffers-from-recentf (orig-fn &rest args)
    (let ((recentf-exclude (list (lambda (_file) t))))
      (apply orig-fn args)))
  (advice-add #'org-get-agenda-file-buffer
              :around #'+org*exclude-agenda-buffers-from-recentf))


;;
;; Built-in libraries
;;

(def-package! org-crypt ; built-in
  :commands org-crypt-use-before-save-magic
  :config
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key user-mail-address))

(def-package! org-clock
  :commands org-clock-save
  :hook (org-mode . org-clock-load)
  :init
  (setq org-clock-persist 'history
        org-clock-persist-file (concat doom-etc-dir "org-clock-save.el"))
  :config
  (add-hook 'kill-emacs-hook #'org-clock-save))

;;
(when (featurep 'org)
  (run-hooks 'org-load-hook))
