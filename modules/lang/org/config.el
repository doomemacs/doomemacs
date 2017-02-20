;;; lang/org/config.el

;; A few things you can expect
;;   + `org-capture' in a popup frame (can be invoked from outside emacs too)
;;   + A simpler attachment system (with auto-deleting support) and
;;     drag-and-drop for images and documents into org files
;;   + Exported files are put in a centralized location (see
;;     `org-export-directory')
;;   + TODO Custom links for class notes
;;   + TODO An org-mode based CRM (including invoicing and pdf exporting) (see custom-crm)
;;   + TODO A tag-based file browser reminiscient of Evernote and Quiver (there's neotree too!)

(defvar +org-init-hook nil
  "TODO")

(add-hook 'org-load-hook '+org|init)
(add-hook 'org-mode-hook '+org|hook)

;; Custom variables
(defvar +org-dir "~/work/org/"
  "The directory where org files are kept.")

(defvaralias 'org-directory '+org-dir)

(defvar +org-notes-dir (concat +org-dir "notes")
  "The directory where the notes are kept")

(defvar +org-quicknote-dir (concat +org-dir "inbox")
  "The directory to store quick notes produced by `doom:org-capture' (individual org files)")

(defvar +org-attachment-dir ".attach/"
  "Where to store attachments (relative to current org file).")

;; (defvar-local +org-attachments-list '()
;;   "A list of attachments for the current buffer. This is so my custom attachment
;; system can keep track of each buffer's attachments.")

(@load +attach)
(@load +capture)
(@load +export)


;;
;; Config
;;

(defun +org|hook ()
  "Run everytime `org-mode' is enabled."
  (evil-org-mode +1)
  (visual-line-mode +1)
  (setq line-spacing 1)

  ;; If saveplace places the point in a folded position, unfold it on load
  (when (outline-invisible-p)
    (ignore-errors
      (save-excursion
        (outline-previous-visible-heading 1)
        (org-show-subtree))))

  (defun +org|update-cookies ()
    "Update counts on headlines (\"cookies\")."
    (when (file-exists-p buffer-file-name)
      (org-update-statistics-cookies t)))

  (add-hook 'before-save-hook '+org|update-cookies nil t)
  (add-hook 'evil-insert-state-exit-hook '+org|update-cookies nil t))


(defun +org|init ()
  "Initializes org core."
  (define-minor-mode evil-org-mode
    "Evil-mode bindings for org-mode."
    :init-value nil
    :lighter " !"
    :keymap (make-sparse-keymap)
    :group 'evil-org)

  (@set :popup
    '(" *Agenda Commands*"  :size 30  :noselect t)
    '(" *Org todo*"         :size 5   :noselect t)
    '("*Calendar*"          :size 0.4 :noselect t)
    '("*Org Links*"         :size 5   :noselect t)
    '("^\\*Org Agenda.+"    :size 0.4 :regexp t)
    '("^\\*Org Src .+\\*$"  :size 0.4 :regexp t)
    '("^\\*Org-Babel.*\\*$" :size 0.4 :regexp t))

  (setq-default
   org-export-coding-system 'utf-8

   ;; Appearance
   outline-blank-line t
   org-indent-mode-turns-on-hiding-stars t
   org-adapt-indentation nil
   org-blank-before-new-entry '((heading . nil) (plain-list-item . auto))
   org-cycle-separator-lines 1
   org-cycle-include-plain-lists t
   org-ellipsis '+doom-folded-face
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
   org-archive-location (concat +org-dir "/archived/%s::")
   org-refile-targets '((nil . (:maxlevel . 2))) ; display full path in refile completion

   ;; Agenda
   org-agenda-restore-windows-after-quit nil
   org-agenda-skip-unavailable-files nil
   org-agenda-dim-blocked-tasks nil
   org-agenda-window-setup 'other-frame ; to get org-agenda to behave with shackle...
   org-agenda-inhibit-startup t
   org-agenda-files (directory-files +org-dir t "\\.org$" t)
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
   org-format-latex-options
   (plist-put org-format-latex-options :scale 1.5)
   org-format-latex-options
   (plist-put org-format-latex-options
              :background (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                              'default)
                                          :background nil t))

   org-highlight-latex-and-related '(latex)
   org-latex-create-formula-image-program 'dvipng
   org-latex-image-default-width ".9\\linewidth"
   org-latex-preview-ltxpng-directory (concat doom-cache-dir "/ltxpng/")
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
  (defface org-list-bullet
    '((t (:inherit font-lock-keyword-face)))
    "Face for list bullets")

  (font-lock-add-keywords
   'org-mode '(("^ *\\([-+]\\|[0-9]+[).]\\) "
                (1 'org-list-bullet))
               ("^ *\\(-----+\\)$"
                (1 'org-meta-line))))

  ;; Enable gpg support
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
  (@def-package org-bullets :commands org-bullets-mode)

  ;; Keybinds
  (@map (:map org-mode-map
          "RET" nil
          "C-j" nil
          "C-k" nil
          :i [remap doom/inflate-space-maybe] 'org-self-insert-command
          :i "RET" 'org-return-indent)

        (:map evil-org-mode-map
          :n  "RET" '+org/dwim-at-point
          ;;
          :ni "A-l" 'org-metaright
          :ni "A-h" 'org-metaleft
          :ni "A-k" 'org-metaup
          :ni "A-j" 'org-metadown
          :ni "A-L" 'org-shiftmetaright
          :ni "A-H" 'org-shiftmetaleft
          :ni "A-K" 'org-shiftmetaup
          :ni "A-J" 'org-shiftmetadown
          ;; Expand tables (or shiftmeta move)
          :ni "C-S-l" '+org/table-append-field-or-shift-right
          :ni "C-S-h" '+org/table-prepend-field-or-shift-left
          :ni "C-S-k" '+org/table-prepend-row-or-shift-up
          :ni "C-S-j" '+org/table-append-row-or-shift-down
          ;; Navigate table cells
          :i  "C-L" '+org/table-next-field
          :i  "C-H" '+org/table-previous-field
          :i  "C-K" '+org/table-previous-row
          :i  "C-J" '+org/table-next-row

          :i  "C-e" 'org-end-of-line
          :i  "C-a" 'org-beginning-of-line

          :i  "<tab>" '+org/indent-or-next-field
          :i  [S-iso-lefttab] '+org/dedent-or-prev-field ; for GNU Emacs
          :i  [(shift tab)] '+org/dedent-or-prev-field
          :i  [backtab] '+org/dedent-or-prev-field

          :n  "<tab>" '+org/toggle-fold

          :nv "j"   'evil-next-visual-line
          :nv "k"   'evil-previous-visual-line
          :v  "<S-tab>" '+snippets/expand-on-region

          :i  "M-a" (@λ (evil-visual-state) (org-mark-element))
          :n  "M-a" 'org-mark-element
          :v  "M-a" 'mark-whole-buffer

          :ni "<M-return>"   (@λ (+org/insert-item 'below))
          :ni "<S-M-return>" (@λ (+org/insert-item 'above))

          ;; Formatting shortcuts
          :i  "M-b" (@λ (+org-surround "*")) ; bold
          :i  "M-u" (@λ (+org-surround "_")) ; underline
          :i  "M-i" (@λ (+org-surround "/")) ; italics
          :i  "M-`" (@λ (+org-surround "+")) ; strikethrough

          :v  "M-b" "S*"
          :v  "M-u" "S_"
          :v  "M-i" "S/"
          :v  "M-`" "S+"

          (:localleader
           :n  "RET" 'org-archive-subtree
           :n  "SPC" '+org/toggle-checkbox
           :n  "/"  'org-sparse-tree
           :n  "="  'org-align-all-tags
           :n  "?"  'org-tags-view
           :n  "D"  'org-deadline
           :nv "L"  'org-store-link
           :n  "R"  (@λ (org-metaleft) (org-archive-to-archive-sibling)) ; archive to parent sibling
           :n  "T"  'org-todo
           :n  "a"  'org-agenda
           :n  "d"  'org-time-stamp
           :n  "e"  'org-edit-special
           :n  "l"  'org-insert-link
           :n  "n"  (@λ (if (buffer-narrowed-p) (widen) (org-narrow-to-subtree)))
           :n  "r"  'org-refile
           :n  "s"  'org-schedule
           :n  "t"  (@λ (org-todo (if (org-entry-is-todo-p) 'none 'todo)))
           :v  "t"  (@λ (evil-ex-normal evil-visual-beginning evil-visual-end "\\t"))
           :n  "v"  'variable-pitch-mode
           ;; :n  "w"  'writing-mode
           ;; :n  "x"  '+org/remove-link
           )

          ;; TODO Improve folding bindings
          :n  "za"  '+org/toggle-fold
          :n  "zA"  'org-shifttab
          :n  "zc"  'outline-hide-subtree
          :n  "zC"  (@λ (outline-hide-sublevels 1))
          :n  "zd"  (lambda (&optional arg) (interactive "p") (outline-hide-sublevels (or arg 3)))
          :n  "zm"  (@λ (outline-hide-sublevels 1))
          :n  "zo"  'outline-show-subtree
          :n  "zO"  'outline-show-all
          :n  "zr"  'outline-show-all

          :m  "]]"  (@λ (call-interactively 'org-forward-heading-same-level) (org-beginning-of-line))
          :m  "[["  (@λ (call-interactively 'org-backward-heading-same-level) (org-beginning-of-line))
          :m  "]l"  'org-next-link
          :m  "[l"  'org-previous-link

          :m  "gh"  'outline-up-heading
          :m  "gj"  'org-forward-heading-same-level
          :m  "gk"  'org-backward-heading-same-level
          :m  "gl"  (@λ (call-interactively 'outline-next-visible-heading) (show-children))

          :n  "go"  'org-open-at-point
          :n  "gO"  (@λ (let ((org-link-frame-setup (append '((file . find-file-other-window)) org-link-frame-setup))
                              (org-file-apps '(("\\.org$" . emacs)
                                               (t . "open \"%s\""))))
                          (call-interactively 'org-open-at-point)))

          :n  "gQ"  'org-fill-paragraph
          :m  "$"   'org-end-of-line
          :m  "^"   'org-beginning-of-line
          :n  "<"   'org-metaleft
          :n  ">"   'org-metaright
          :v  "<"   (@λ (org-metaleft)  (evil-visual-restore))
          :v  ">"   (@λ (org-metaright) (evil-visual-restore))
          :n  "-"   'org-cycle-list-bullet
          :m  "<tab>" 'org-cycle)

        (:map org-src-mode-map
          :n  "<escape>" (@λ (message "Exited") (org-edit-src-exit)))

        (:after org-agenda
          (:map org-agenda-mode-map
            :e "<escape>" 'org-agenda-Quit
            :e "m"   'org-agenda-month-view
            :e "C-j" 'org-agenda-next-item
            :e "C-k" 'org-agenda-previous-item
            :e "C-n" 'org-agenda-next-item
            :e "C-p" 'org-agenda-previous-item)))

  ;; Initialize everything else
  (run-hooks '+org-init-hook)
  (+org|hacks))


(defun +org|hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (push '(file . find-file) org-link-frame-setup)

  ;; Let OS decide what to do with files when opened
  (setq org-file-apps
        `(("\\.org$" . emacs)
          (t . ,(cond (IS-MAC "open -R \"%s\"")
                      (IS-LINUX "xdg-open \"%s\"")))))

  ;; Don't clobber recentf with agenda files
  (defun +org-is-agenda-file (filename)
    (find (file-truename filename) org-agenda-files :key 'file-truename
          :test 'equal))
  (add-to-list 'recentf-exclude '+org-is-agenda-file)

  ;; Remove highlights on ESC
  (defun +org*remove-occur-highlights (&rest args)
    (when (eq major-mode 'org-mode)
      (org-remove-occur-highlights)))
  (advice-add 'evil-force-normal-state :before '+org*remove-occur-highlights)

  ;; Don't reset org-hide!
  (advice-add 'org-find-invisible-foreground :override 'ignore))

