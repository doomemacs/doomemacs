;;; module-org.el --- -*- no-byte-compile: t; -*-

(defconst org-directory (expand-file-name "~/Notes/"))

(define-minor-mode evil-org-mode
  "Evil-mode bindings for org-mode."
  :init-value nil
  :lighter " !"
  :keymap (make-sparse-keymap)
  :group 'evil-org)

(add-hook 'org-load-hook 'doom|org-init t)
(add-hook 'org-load-hook 'doom|org-keybinds t)
(add-hook 'org-load-hook 'doom|org-hacks t)
(add-hook 'org-mode-hook 'doom|org-hook)

;;
(defun doom|org-hook ()
  (evil-org-mode +1)
  (visual-line-mode +1)
  (setq line-spacing 1)

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
   org-hide-leading-stars t
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
   org-completion-use-ido nil ; Use ivy/counsel for refiling
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
   org-archive-location (concat org-directory "/archived/%s::")
   org-refile-targets '((nil . (:maxlevel . 2))) ; display full path in refile completion

   ;; Agenda
   org-agenda-restore-windows-after-quit nil
   org-agenda-skip-unavailable-files nil
   org-agenda-dim-blocked-tasks nil
   org-agenda-window-setup 'other-frame ; to get org-agenda to behave with shackle...
   org-agenda-inhibit-startup t
   org-agenda-files (append (f-entries org-directory (lambda (path) (f-ext? path "org")))
                            (f-entries (f-expand "projects" org-directory) (lambda (path) (f-ext? path "org")))
                            (f-entries (f-expand "contacts" org-directory) (lambda (path) (f-ext? path "org"))))
   org-todo-keywords '((sequence "[ ](t)" "[-](p)" "[?](m)" "|" "[X](d)")
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
   )

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

  ;; smartparens config
  (sp-with-modes '(org-mode)
    (sp-local-pair "\\[" "\\]" :post-handlers '(("| " "SPC")))
    (sp-local-pair "\\(" "\\)" :post-handlers '(("| " "SPC")))
    (sp-local-pair "$$" "$$"   :post-handlers '((:add " | ")) :unless '(sp-point-at-bol-p))
    (sp-local-pair "{" nil))

  ;; bullets
  (use-package org-bullets :commands org-bullets-mode))

(defun doom|org-hacks ()
  ;; Don't open separate windows
  (push '(file . find-file) org-link-frame-setup)

  ;; Reveal files in finder
  (setq org-file-apps '(("\\.org$" . emacs) (t . "open -R \"%s\"")))

  ;; Don't clobber recentf with agenda files
  (defun org-is-agenda-file (filename)
    (find (file-truename filename) org-agenda-files :key 'file-truename
          :test 'equal))
  (pushnew 'org-is-agenda-file recentf-exclude)

  ;; Don't track attachments
  (push (format "/%s.+$" (regexp-quote org-attach-directory)) recentf-exclude)
  (push ".attach" projectile-globally-ignored-file-suffixes)

  ;; Remove highlights on ESC
  (defun doom*org-remove-occur-highlights (&rest args)
    (when (eq major-mode 'org-mode) (org-remove-occur-highlights)))
  (advice-add 'evil-force-normal-state :before 'doom*org-remove-occur-highlights)

  ;; Don't reset org-hide!
  (advice-add 'org-find-invisible-foreground :override 'ignore)

  ;; Tame org-mode popups
  ;; Ensures org-src-edit yields control of its buffer to shackle.
  (defun org-src-switch-to-buffer (buffer context)
    (pop-to-buffer buffer))

  ;; And these for org-todo, org-link and org-agenda
  (defun org-pop-to-buffer-same-window (&optional buffer-or-name norecord label)
    "Pop to buffer specified by BUFFER-OR-NAME in the selected window."
    (display-buffer buffer-or-name))

  (defun org-switch-to-buffer-other-window (&rest args)
    (mapc (lambda (b)
            (let ((buf (if (stringp b) (get-buffer-create b) b)))
              (pop-to-buffer buf t t)))
          args))

  ;; Taming Org-agenda!
  (defun doom/org-agenda-quit ()
    "Necessary to finagle org-agenda into shackle popups and behave properly on quit."
    (interactive)
    (if org-agenda-columns-active
        (org-columns-quit)
      (let ((buf (current-buffer)))
        (and (not (eq org-agenda-window-setup 'current-window))
             (not (one-window-p))
             (delete-window))
        (kill-buffer buf)
        (setq org-agenda-archives-mode nil
              org-agenda-buffer nil))))

  (after! org-agenda
    (map! :map org-agenda-mode-map
          :e "<escape>" 'doom/org-agenda-quit
          :e "ESC" 'doom/org-agenda-quit
          :e [escape] 'doom/org-agenda-quit
          "q" 'doom/org-agenda-quit
          "Q" 'doom/org-agenda-quit)))

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
          :m  "<tab>" 'org-cycle)

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
