;;; module-org.el --- -*- no-byte-compile: t; -*-

(add-hook 'org-load-hook 'doom|org-init t)
(add-hook 'org-load-hook 'doom|org-keybinds t)
(add-hook 'org-mode-hook 'doom|org-hook)

(defvar org-directory (expand-file-name "~/Dropbox/notes/"))

(define-minor-mode evil-org-mode
  "Evil-mode bindings for org-mode."
  :init-value nil
  :lighter    " !"
  :keymap     (make-sparse-keymap) ; defines evil-org-mode-map
  :group      'evil-org)

(require 'module-org-crm)
(require 'module-org-notebook)

;;
(defun doom|org-hook ()
  (evil-org-mode +1)
  (visual-line-mode +1)
  (setq line-spacing 2)
  (org-bullets-mode +1)

  ;; If saveplace places the point in a folded position, unfold it on load
  (when (outline-invisible-p)
    (ignore-errors
      (save-excursion
        (outline-previous-visible-heading 1)
        (org-show-subtree))))

  (defun doom|org-update ()
    (when (file-exists-p buffer-file-name)
      (org-update-statistics-cookies t)))

  (add-hook 'before-save-hook 'doom|org-update nil t)
  (add-hook 'evil-insert-state-exit-hook 'doom|org-update nil t))

(defun doom|org-init ()
  (def-popup! " *Agenda Commands*"  :align below :size 30)
  (def-popup! " *Org todo*"         :align below :size 5   :noselect t)
  (def-popup! "*Calendar*"          :align below :size 0.4)
  (def-popup! "*Org Links*"         :align below :size 5)
  (def-popup! "^\\*Org Agenda.+"    :align below :size 0.4 :regexp t)
  (def-popup! "^\\*Org Src .+\\*$"  :align below :size 0.4 :select t :regexp t)
  (def-popup! "^\\*Org-Babel.*\\*$" :align below :size 0.4 :regexp t)

  (setq-default
   org-export-coding-system 'utf-8

   ;; Appearance
   org-indent-mode-turns-on-hiding-stars t
   org-adapt-indentation nil
   org-blank-before-new-entry '((heading . nil) (plain-list-item . auto))
   org-bullets-bullet-list '("✸" "•" "◦" "•" "◦" "•" "◦")
   org-cycle-separator-lines 1
   org-cycle-include-plain-lists t
   org-ellipsis 'hs-face
   org-entities-user '(("flat" "\\flat" nil "" "" "266D" "♭")
                       ("sharp" "\\sharp" nil "" "" "266F" "♯"))
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-footnote-auto-label 'plain
   org-hide-emphasis-markers t
   org-hide-leading-stars nil
   org-image-actual-width nil
   org-indent-indentation-per-level 2
   org-pretty-entities t
   org-pretty-entities-include-sub-superscripts t
   org-startup-folded t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column 0
   org-use-sub-superscripts '{}

   ;; Behavior
   org-catch-invisible-edits 'show
   org-checkbox-hierarchical-statistics nil
   org-completion-use-ido nil ; Use helm for refiling
   org-confirm-elisp-link-function nil
   org-default-priority ?C
   org-hidden-keywords '(title)
   org-hierarchical-todo-statistics t
   org-log-done t
   org-loop-over-headlines-in-active-region t
   org-outline-path-complete-in-steps nil
   org-refile-use-outline-path t
   org-special-ctrl-a/e t

   ;; Sorting/refiling
   org-archive-location (concat org-directory "/Archived/%s::")
   org-refile-targets '((nil . (:maxlevel . 2))) ; display full path in refile completion

   ;; Agenda
   org-agenda-restore-windows-after-quit nil
   org-agenda-skip-unavailable-files nil
   org-agenda-dim-blocked-tasks nil
   org-agenda-window-setup 'other-frame ; to get org-agenda to behave with shackle...
   org-agenda-inhibit-startup t
   org-agenda-files (f-entries org-directory (lambda (path) (f-ext? path "org")))
   org-todo-keywords '((sequence "[ ](t)" "[-](p)" "|" "[X](d)")
                       (sequence "TODO(T)" "|" "DONE(D)")
                       (sequence "IDEA(i)" "NEXT(n)" "ACTIVE(a)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))


   ;; Babel
   org-confirm-babel-evaluate nil   ; you don't need my permission
   org-src-fontify-natively t       ; make code pretty
   org-src-preserve-indentation t
   org-src-tab-acts-natively t
   org-src-window-setup 'current-window
   org-edit-src-content-indentation 0

   ;; Latex
   org-format-latex-options (plist-put org-format-latex-options :scale 1.4)
   org-highlight-latex-and-related '(latex)
   org-latex-create-formula-image-program 'dvipng
   org-latex-image-default-width ".9\\linewidth"
   org-latex-preview-ltxpng-directory (concat doom-temp-dir "/ltxpng/")
   org-latex-remove-logfiles nil
   org-startup-with-latex-preview nil
   ;; org-latex-packages-alist
   ;; '(("" "gauss" t)
   ;;   ("" "physics" t) TODO Install this)

   org-capture-templates
   '(;; TODO: New Note (note)
     ;; TODO: New Task (todo)
     ;; TODO: New vocabulary word

     ("c" "Changelog" entry
      (file+headline (f-expand "CHANGELOG.org" (doom/project-root)) "Unreleased")
      "* %?")

     ;; ("p" "Project Notes" entry
     ;;  (file+headline org-default-notes-file "Inbox")
     ;;  "* %u %?\n%i" :prepend t)

     ;; ("m" "Major-mode Notes" entry
     ;;  (file+headline org-default-notes-file "Inbox")
     ;;  "* %u %?\n%i" :prepend t)

     ;; ("n" "Notes" entry
     ;;  (file+headline org-default-notes-file "Inbox")
     ;;  "* %u %?\n%i" :prepend t)

     ;; ("v" "Vocab" entry
     ;;  (file+headline (concat org-directory "topics/vocab.org") "Unsorted")
     ;;  "** %i%?\n")
     ))

  (doom-fix-unicode "DejaVu Sans" '(?♭ ?♯))
  (doom-fix-unicode "Hack" '(?× ?∙ ?÷ ?⌉ ?⌈ ?⌊ ?⌋
                             ?∩ ?∪ ?⊆ ?⊂ ?⊄ ?⊇ ?⊃ ?⊅
                             ?⇒ ?⇐ ?⇔ ?↔ ?→ ?≡ ?∴ ?∵ ?⊕ ?∀ ?∃ ?∄ ?∈ ?∉
                             ?∨ ?∧ ?¬))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (ruby . t) (sh . t) (js . t) (css . t)
     (plantuml . t) (emacs-lisp . t) (matlab . t)
     (latex . t) (calc . t) (lisp . t) (lilypond . t)
     ;; (go . t)
     ;; (http . t)
     ;; (rust . t)
     ))

  (let ((ext-regexp (regexp-opt '("GIF" "JPG" "JPEG" "SVG" "TIF" "TIFF" "BMP" "XPM"
                                  "gif" "jpg" "jpeg" "svg" "tif" "tiff" "bmp" "xpm"))))
    (setq iimage-mode-image-regex-alist
          `((,(concat "\\(`?file://\\|\\[\\[\\|<\\|`\\)?\\([-+./_0-9a-zA-Z]+\\."
                      ext-regexp "\\)\\(\\]\\]\\|>\\|'\\)?") . 2)
            (,(concat "<\\(http://.+\\." ext-regexp "\\)>") . 1))))

  ;; Don't open separate windows
  (push '(file . find-file) org-link-frame-setup)

  ;; Reveal files in finder
  (setq org-file-apps '(("\\.org$" . emacs) (t . "open -R \"%s\"")))

  ;; Fontify checkboxes and dividers
  (defface org-list-bullet '((t ())) "Face for list bullets")
  (font-lock-add-keywords
   'org-mode '(("^ *\\([-+]\\|[0-9]+[).]\\) "
                (1 'org-list-bullet))
               ("^ *\\(-----+\\)$"
                (1 'org-meta-line))))

  ;; Enable encryption
  (require 'epa-file)
  (epa-file-enable)
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key user-mail-address
        epa-file-encrypt-to user-mail-address)

  ;; Don't track attachments
  (push (format "/%s.+$" (regexp-quote org-attach-directory)) recentf-exclude)
  ;; Don't clobber recentf with agenda files
  (defun org-is-agenda-file (filename)
    (find (file-truename filename) org-agenda-files :key 'file-truename
          :test 'equal))
  (pushnew 'org-is-agenda-file recentf-exclude)

  ;; Remove highlights on ESC
  (defun doom*org-remove-occur-highlights (&rest args)
    (when (eq major-mode 'org-mode) (org-remove-occur-highlights)))
  (advice-add 'evil-force-normal-state :before 'doom*org-remove-occur-highlights)

  ;; smartparens config
  (sp-with-modes '(org-mode)
    (sp-local-pair "*" "*" :unless '(sp-point-after-word-p sp-point-at-bol-p) :skip-match 'doom/sp-org-skip-asterisk)
    (sp-local-pair "_" "_" :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-symbol-p))
    (sp-local-pair "/" "/" :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-symbol-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "~" "~" :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-symbol-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "=" "=" :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-symbol-p) :post-handlers '(("[d1]" "SPC")))

    (sp-local-pair "\\[" "\\]" :post-handlers '(("| " "SPC")))
    (sp-local-pair "\\(" "\\)" :post-handlers '(("| " "SPC")))
    (sp-local-pair "$$" "$$"   :post-handlers '((:add " | ")) :unless '(sp-point-at-bol-p))
    (sp-local-pair "{" nil))

  (use-package org-bullets :commands (org-bullets-mode)))

(defun doom|org-keybinds ()
  (map! (:map org-mode-map
          "RET" nil
          "C-j" nil
          "C-k" nil
          :i [remap doom/inflate-space-maybe] 'org-self-insert-command
          :i "RET" 'org-return-indent)

        (:map evil-org-mode-map
          :ni "A-l" 'org-metaright
          :ni "A-h" 'org-metaleft
          :ni "A-k" 'org-metaup
          :ni "A-j" 'org-metadown
          ;; Expand tables (or shiftmeta move)
          :ni "A-L" 'doom/org-table-append-field-or-shift-right
          :ni "A-H" 'doom/org-table-prepend-field-or-shift-left
          :ni "A-K" 'doom/org-table-prepend-row-or-shift-up
          :ni "A-J" 'doom/org-table-append-row-or-shift-down

          :i  "C-L" 'doom/org-table-next-field
          :i  "C-H" 'doom/org-table-previous-field
          :i  "C-K" 'doom/org-table-previous-row
          :i  "C-J" 'doom/org-table-next-row

          :i  "C-e" 'org-end-of-line
          :i  "C-a" 'org-beginning-of-line
          :i  "<tab>"   'doom/org-indent
          :i  "<S-tab>" 'doom/org-dedent

          :nv "j"   'evil-next-visual-line
          :nv "k"   'evil-previous-visual-line
          :v  "<S-tab>" 'doom/yas-insert-snippet

          :i  "M-a" (λ! (evil-visual-state) (org-mark-element))
          :n  "M-a" 'org-mark-element
          :v  "M-a" 'mark-whole-buffer

          :ni "<M-return>"   (λ! (doom/org-insert-item 'below))
          :ni "<S-M-return>" (λ! (doom/org-insert-item 'above))

          :i  "M-b" (λ! (doom/org-surround "*")) ; bold
          :i  "M-u" (λ! (doom/org-surround "_")) ; underline
          :i  "M-i" (λ! (doom/org-surround "/")) ; italics
          :i  "M-`" (λ! (doom/org-surround "+")) ; strikethrough

          :v  "M-b" "S*"
          :v  "M-u" "S_"
          :v  "M-i" "S/"
          :v  "M-`" "S+"

          (:leader
            :n ";"  'helm-org-in-buffer-headings
            :n "oa" 'doom/org-attachment-reveal)

          (:localleader
            :n  "/"  'org-sparse-tree
            :n  "?"  'org-tags-view

            :n  "n"  (λ! (if (buffer-narrowed-p) (widen) (org-narrow-to-subtree)))
            :n  "e"  'org-edit-special
            :n  "="  'org-align-all-tags
            :nv "l"  'org-insert-link
            :n  "L"  'org-store-link
            :n  "x"  'doom/org-remove-link
            ;; :n  "w"  'writing-mode
            :n  "v"  'variable-pitch-mode
            :n  "SPC" 'doom/org-toggle-checkbox
            :n  "RET" 'org-archive-subtree

            :n  "a"  'org-agenda
            :n  "A"  'doom:org-attachment-list

            :n  "d"  'org-time-stamp
            :n  "D"  'org-deadline
            :n  "i"  'doom/org-toggle-inline-images-at-point
            :n  "t"  (λ! (org-todo (if (org-entry-is-todo-p) 'none 'todo)))
            :v  "t"  (λ! (evil-ex-normal evil-visual-beginning evil-visual-end "\\t"))
            :n  "T"  'org-todo
            :n  "s"  'org-schedule
            :n  "r"  'org-refile
            :n  "R"  (λ! (org-metaleft) (org-archive-to-archive-sibling)) ; archive to parent sibling
            )

          ;; TODO Improve folding bindings
          :n  "za"  'org-cycle
          :n  "zA"  'org-shifttab
          :n  "zm"  (λ! (outline-hide-sublevels 1))
          :n  "zr"  'outline-show-all
          :n  "zo"  'outline-show-subtree
          :n  "zO"  'outline-show-all
          :n  "zc"  'outline-hide-subtree
          :n  "zC"  (λ! (outline-hide-sublevels 1))
          :n  "zd"  (lambda (&optional arg) (interactive "p") (outline-hide-sublevels (or arg 3)))

          :m  "]]"  (λ! (call-interactively 'org-forward-heading-same-level) (org-beginning-of-line))
          :m  "[["  (λ! (call-interactively 'org-backward-heading-same-level) (org-beginning-of-line))
          :m  "]l"  'org-next-link
          :m  "[l"  'org-previous-link

          :n  "RET" 'doom/org-dwim-at-point

          :m  "gh"  'outline-up-heading
          :m  "gj"  'org-forward-heading-same-level
          :m  "gk"  'org-backward-heading-same-level
          :m  "gl"  (λ! (call-interactively 'outline-next-visible-heading) (show-children))

          :n  "go"  'org-open-at-point
          :n  "gO"  (λ! (let ((org-link-frame-setup (append '((file . find-file-other-window)) org-link-frame-setup))
                              (org-file-apps '(("\\.org$" . emacs)
                                               (t . "open \"%s\""))))
                          (call-interactively 'org-open-at-point)))

          :n  "gQ"  'org-fill-paragraph
          :m  "$"   'org-end-of-line
          :m  "^"   'org-beginning-of-line
          :n  "<"   'org-metaleft
          :n  ">"   'org-metaright
          :v  "<"   (λ! (org-metaleft)  (evil-visual-restore))
          :v  ">"   (λ! (org-metaright) (evil-visual-restore))
          :n  "-"   'org-cycle-list-bullet
          :n  [tab] 'org-cycle)

        (:map org-src-mode-map
          :n  "<escape>" (λ! (message "Exited") (org-edit-src-exit)))

        (:after org-agenda
          (:map org-agenda-mode-map
            :e "<escape>" 'org-agenda-Quit
            :e "m"   'org-agenda-month-view
            :e "C-j" 'org-agenda-next-item
            :e "C-k" 'org-agenda-previous-item
            :e "C-n" 'org-agenda-next-item
            :e "C-p" 'org-agenda-previous-item))))

(provide 'module-org)
;;; module-org.el ends here
