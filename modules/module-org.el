;;; module-org.el

(define-minor-mode evil-org-mode
  "Evil-mode bindings for org-mode."
  :init-value nil
  :lighter    "!"
  :keymap     (make-sparse-keymap) ; defines evil-org-mode-map
  :group      'evil-org)

(defvar org-directory (expand-file-name "org/" narf-dropbox-dir))
(defvar org-directory-contacts (expand-file-name "my/contacts/" org-directory))
(defvar org-directory-projects (expand-file-name "my/projects/" org-directory))
(defvar org-directory-invoices (expand-file-name "my/invoices/" org-directory))

(defvar org-default-notes-file (concat org-directory "notes.org"))

(add-hook! org-load 'narf|org-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun narf@org-vars ()
  (setq org-agenda-files
        (f-entries org-directory (lambda (path) (string-suffix-p ".org" path)) t)

        org-archive-location (concat org-directory "/archive/%s::")
        org-attach-directory ".attach/"

        ;; org-mobile-inbox-for-pull (concat org-directory "notes.org")
        ;; org-mobile-directory "~/Dropbox/Apps/MobileOrg"

        ;; Use helm for refiling
        org-completion-use-ido nil
        org-refile-targets '((nil . (:maxlevel . 2)))
        ;; display full path in refile completion
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil

        org-default-priority ?C
        org-catch-invisible-edits nil
        org-confirm-elisp-link-function nil
        org-hidden-keywords '(title)
        org-special-ctrl-a/e t
        org-hierarchical-todo-statistics t
        org-checkbox-hierarchical-statistics nil
        org-tags-column 0
        org-loop-over-headlines-in-active-region t
        org-footnote-auto-label 'plain
        org-log-done t
        org-agenda-window-setup 'other-window
        org-agenda-skip-unavailable-files t
        org-src-window-setup 'current-window
        org-startup-folded 'content
        org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                            (sequence "IDEA(i)" "NEXT(n)" "ACTIVE(a)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)")
                            (sequence "UNSENT(u)" "UNPAID(U)" "|" "PAID(p)"))
        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . auto))

        org-tag-alist '(("@home" . ?h)
                        ("@daily" . ?d)
                        ("@projects" . ?r))

        org-capture-templates
        '(("t" "TODO" entry
           (file+headline (concat org-directory "notes.org") "Unsorted")
           "*** TODO %? %u")

          ;; TODO Select file from org files
          ;; ("T" "Specific TODO" entry
          ;;  (function narf/-org-capture-choose)
          ;;  "** TODO %?\n%i" :prepend t)

          ;; ("c" "Changelog" entry
          ;;  (function narf/-org-capture-changelog)
          ;;  "** %<%H:%M>: %? :unsorted:\n%i" :prepend t)

          ("j" "Journal" entry
           (file+datetree (concat org-directory "journal.org"))
           "** %<%H:%M>: %?\n%i" :prepend t)

          ;; TODO Select file from notes folder
          ("n" "Notes" entry
           (file+headline (concat org-directory "notes.org") "Unsorted")
           "* %u %?\n%i" :prepend t)

          ("s" "Writing Scraps" entry
           (file+headline (concat org-directory "writing/scraps.org") "Unsorted")
           "* %t %?\n%i" :prepend t)

          ;; TODO Sort word under correct header
          ("v" "Vocab" entry
           (file+headline (concat org-directory "notes/vocab.org") "Unsorted")
           "** %i%?\n")

          ("e" "Excerpt" entry
           (file+headline (concat org-directory "notes/excerpts.org") "Excerpts")
           "** %u %?\n%i" :prepend t)

          ("q" "Quote" item
           (file+headline (concat org-directory "notes/excerpts.org") "Quotes")
           "+ %i\n  *Source: ...*\n  : @tags" :prepend t)
          ))

  (defvar org-agenda-restore-windows-after-quit t)
  (defvar org-agenda-custom-commands
    '(("x" agenda)
      ("y" agenda*)
      ("l" todo "LEAD")
      ("t" todo)
      ("c" tags "+class")))

  (let ((ext-regexp (regexp-opt '("GIF" "JPG" "JPEG" "SVG" "TIF" "TIFF" "BMP" "XPM"
                                  "gif" "jpg" "jpeg" "svg" "tif" "tiff" "bmp" "xpm"))))
    (setq iimage-mode-image-regex-alist
          `((,(concat "\\(`?file://\\|\\[\\[\\|<\\|`\\)?\\([-+./_0-9a-zA-Z]+\\."
                      ext-regexp "\\)\\(\\]\\]\\|>\\|'\\)?") . 2)
            (,(concat "<\\(http://.+\\." ext-regexp "\\)>") . 1))))

  (add-to-list 'org-link-frame-setup '(file . find-file)))

(defun narf@org-export ()
  (defvar narf-org-export-directory (concat org-directory ".export"))
  (require 'ox-pandoc)
  (setq org-export-backends '(ascii html latex md opml)
        org-export-with-toc nil)

  ;; Export to a central directory (why isn't this easier?)
  (defun narf*org-export-output-file-name (args)
    (unless (nth 2 args)
      (setq args (append args (list narf-org-export-directory))))
    args)
  (advice-add 'org-export-output-file-name :filter-args 'narf*org-export-output-file-name))

(defun narf@org-babel ()
  (setq org-confirm-babel-evaluate nil   ; you don't need my permission
        org-src-fontify-natively t       ; make code pretty
        org-src-preserve-indentation t
        org-src-tab-acts-natively t)

  (defun narf-refresh-babel-lob ()
    (let ((files (f-entries org-directory (lambda (path) (f-ext? path "org")) t)))
      (async-start
       `(lambda ()
          ,(async-inject-variables "\\`\\(org-directory\\|load-path$\\)")
          (require 'org)
          (setq org-babel-library-of-babel nil)
          (mapc (lambda (f) (org-babel-lob-ingest f)) (list ,@files))
          org-babel-library-of-babel)
       (lambda (lib)
         ;; (persistent-soft-store 'org-babel-library lib "org")
         (message "Library of babel updated!")
         (setq org-babel-library-of-babel lib)))))
  (setq org-babel-library-of-babel (narf-refresh-babel-lob))
  (add-hook! org-mode
    (add-hook 'after-save-hook
              (lambda ()
                (when (file-exists-p buffer-file-name)
                  (shut-up! (org-babel-lob-ingest buffer-file-name))))
              t t))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (ruby . t) (sh . t) (js . t) (css . t)
     (plantuml . t) (emacs-lisp . t) (matlab . t)
     (latex . t) (calc . t) (lisp . t) (lilypond . t)
     (http . t) (rust . t) (go . t)))

  (setq org-babel-lilypond-gen-png t)
  ;; Ensure lilypond doesn't print out entire pages for previews
  (defvar org-babel-lilypond-prologue
    "\\paper { indent = 0\\mm line-width = 180\\mm oddHeaderMarkup = \"\" evenHeaderMarkup = \"\" oddFooterMarkup = \"\" evenFooterMarkup = \"\" }")
  (defun org-babel-lilypond-get-header-args (mode)
    (cond (mode
           `((:tangle . "yes")
             (:noweb . "yes")
             (:results . "silent")
             (:cache . "yes")
             (:comments . "yes")
             (:prologue . ,org-babel-lilypond-prologue)))
          (t
           `((:results . "file")
             (:exports . "results")
             (:prologue . ,org-babel-lilypond-prologue)))))
  (setq org-babel-default-header-args:lilypond
        '((:results . "file")
          (:exports . "results")
          (:prologue . ,org-babel-lilypond-prologue)))

  (setq org-plantuml-jar-path puml-plantuml-jar-path)
  (when (file-exists-p "~/.plantuml")
    (add-to-list 'org-babel-default-header-args:plantuml
                 '(:cmdline . "-config ~/.plantuml")))

  (defadvice org-edit-src-code (around set-buffer-file-name activate compile)
    "Assign a proper filename to the edit-src buffer, so plugins like quickrun
will function properly."
    (let ((file-name (buffer-file-name)))
      ad-do-it
      (setq buffer-file-name file-name)))

  ;; Add plantuml syntax highlighting support
  (add-to-list 'org-src-lang-modes '("puml" . puml))
  (add-to-list 'org-src-lang-modes '("plantuml" . puml)))

(defun narf@org-latex ()
  (setq-default
   org-latex-preview-ltxpng-directory (concat narf-temp-dir "ltxpng/")
   org-latex-remove-logfiles t
   org-latex-create-formula-image-program 'dvipng
   org-startup-with-latex-preview nil
   org-highlight-latex-and-related '(latex)
   org-format-latex-options (plist-put org-format-latex-options :scale 1.4)
   org-latex-image-default-width nil
   org-latex-packages-alist
   '(("" "gauss" t)
     ;; ("" "physics" t) TODO Install this
     )))

(defun narf@org-looks ()
  (setq org-image-actual-width nil
        org-startup-with-inline-images nil
        org-startup-indented t
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts t
        org-use-sub-superscripts '{}
        org-fontify-whole-heading-line nil
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-ellipsis 'hs-face
        org-indent-indentation-per-level 2
        org-cycle-separator-lines 2
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-bullets-bullet-list '("•" "◦" "•" "◦" "•" "◦")
        org-entities-user
        '(("flat" "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))

        org-priority-faces
        '((?A . org-todo-vhigh)
          (?B . org-todo-high)))

  (add-hook! org-mode
    (highlight-regexp org-any-link-re 'org-link))

  ;; Restore org-block-background face (removed in official org)
  (defface org-block-background '((t ()))
    "Face used for the source block background.")
  (defun narf--adjoin-to-list-or-symbol (element list-or-symbol)
    (let ((list (if (not (listp list-or-symbol))
                    (list list-or-symbol)
                  list-or-symbol)))
      (require 'cl-lib)
      (cl-adjoin element list)))
  (set-face-attribute 'org-block-background nil :inherit
                      (narf--adjoin-to-list-or-symbol
                       'fixed-pitch
                       (face-attribute 'org-block-background :inherit)))

  ;; Prettify symbols, blocks and TODOs
  (defface org-headline-todo '((t ())) "Face for todo headlines")
  (defface org-todo-high '((t ())) "Face for high-priority todo")
  (defface org-todo-vhigh '((t ())) "Face for very high-priority todo")
  ;; (defface org-whitespace '((t ())) "Face for spaces")
  (defface org-list-bullet '((t ())) "Face for list bullets")
  (defface org-todo-checkbox '((t ())) "Face for list bullets")
  (font-lock-add-keywords
   'org-mode `(("^ *\\(#\\+begin_src\\>\\)"
                (1 (narf/show-as ?#)))
               ("^ *\\(#\\+end_src\\>\\)"
                (1 (narf/show-as ?#)))
               ("^ *\\(#\\+begin_quote\\>\\)"
                (1 (narf/show-as ?>)))
               ("^ *\\(#\\+end_quote\\>\\)"
                (1 (narf/show-as ?>)))

               ;; Hide TODO tags
               ("^\\**\\(\\* DONE\\) \\([^$\n\r]+\\)"
                (1 (narf/show-as ?☑))
                (2 'org-headline-done))
               ("^\\**\\(\\* \\(TODO\\|PAID\\)\\) "
                (1 (narf/show-as ?☐)))

               ;; ("[-+*] \\(\\[X\\]\\) \\([^$\n\r]+\\)"
               ;;  (2 'org-headline-done))

               ("[-+*] \\[X\\] \\([^$\n\r]+\\)"
                (1 'org-headline-done))

               ;; Show checkbox for other todo states (but don't hide the label)
               (,(concat
                  "\\(\\*\\) "
                  (regexp-opt '("IDEA" "NEXT" "ACTIVE" "WAITING" "LATER" "CANCELLED" "UNPAID" "UNSENT") t)
                  " ")
                (1 (narf/show-as ?☐)))

               ("^ *\\([-+]\\|[0-9]+[).]\\)\\( \\)+[^$\n\r]"
                (1 'org-list-bullet))
               )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun narf|org-hook ()
  (evil-org-mode +1)
  (org-bullets-mode +1)
  (org-indent-mode +1)
  ;; (text-scale-set 1)

  ;; Org-specific font. See `narf-writing-font'
  (setq buffer-face-mode-face `(:family ,(symbol-name (font-get narf-writing-font :family))))
  (buffer-face-mode +1)

  (narf|enable-tab-width-2)
  (setq truncate-lines nil)
  (setq line-spacing '2)

  (defun narf|org-update-statistics-cookies ()
    (when (file-exists-p buffer-file-name)
      (org-update-statistics-cookies t)))

  (add-hook 'before-save-hook 'narf|org-update-statistics-cookies nil t)
  (add-hook 'evil-insert-state-exit-hook 'narf|org-update-statistics-cookies nil t)

  ;; If saveplace places the point in a folded position, unfold it on load
  (when (outline-invisible-p)
    (ignore-errors
      (save-excursion
        (outline-previous-visible-heading 1)
        (org-show-subtree))))

  ;; Custom Ex Commands
  (exmap! "src"      'org-edit-special)
  (exmap! "refile"   'org-refile)
  (exmap! "archive"  'org-archive-subtree)
  (exmap! "agenda"   'org-agenda)
  (exmap! "todo"     'org-show-todo-tree)
  (exmap! "link"     'org-link)
  (exmap! "wc"       'narf/org-word-count)
  (exmap! "at[tach]" 'narf:org-attach)
  (exmap! "export"   'narf:org-export)
  )

(defun narf|org-init ()
  (narf@org-vars)
  (narf@org-babel)
  (narf@org-latex)
  (narf@org-looks)
  (narf@org-export)

  (add-hook 'org-mode-hook 'narf|org-hook)

  ;; Don't track attachments
  (push (format "/%s.+$" (regexp-quote org-attach-directory)) recentf-exclude)

  ;; Enable encryption
  (require 'epa-file)
  (epa-file-enable)
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key user-mail-address)
  (setq epa-file-encrypt-to user-mail-address)

  ;; Custom links
  (org-add-link-type "contact" 'narf/org-crm-link-contact)
  (org-add-link-type "project" 'narf/org-crm-link-project)
  (org-add-link-type "invoice" 'narf/org-crm-link-invoice)

  (after! helm
    (mapc (lambda (r) (add-to-list 'helm-boring-file-regexp-list r))
          (list "\\.attach$" "\\.Rhistory$")))

  ;; fix some org-mode + yasnippet conflicts:
  (defun yas/org-very-safe-expand ()
    (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

  (add-hook! org-mode
    (make-variable-buffer-local 'yas-trigger-key)
    (setq yas/trigger-key [tab])
    (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
    (define-key yas-keymap [tab] 'yas-next-field))

  ;;; Evil integration
  (progn
    (advice-add 'evil-force-normal-state :before 'org-remove-occur-highlights)
    ;; Add element delimiter text-objects so we can use evil-surround to
    ;; manipulate them.
    (define-text-object! "$" "\\$" "\\$")
    (define-text-object! "*" "\\*" "\\*")
    (define-text-object! "/" "/" "/")
    (define-text-object! "_" "_" "_")
    (define-text-object! "=" "=" "=")
    (define-text-object! "~" "~" "~"))

  ;;; Plugins
  (require 'org-download)
  (setq-default
   org-download-image-dir ".attach"
   org-download-heading-lvl nil
   org-download-timestamp "_%Y%m%d_%H%M%S")

  (when IS-MAC
    (setq org-download-screenshot-method "screencapture -i %s"))

  (defun org-download--dir-2 ()
    (f-base (buffer-file-name)))

  ;;; Auto-completion
  (after! company
    (require 'company-math)
    (define-company-backend! org-mode
      (math-symbols-latex
       math-symbols-unicode
       latex-commands
       capf
       yasnippet
       dabbrev-code
       keywords)))

  (define-key org-mode-map (kbd "RET") nil)
  (define-key org-mode-map (kbd "C-j") nil)
  (define-key org-mode-map (kbd "C-k") nil)
  ;; Keybinds
  (map! (:map org-mode-map
          :i [remap narf/inflate-space-maybe] 'org-self-insert-command
          :i "RET" 'org-return-indent)

        (:map evil-org-mode-map
          :ni "A-l" 'org-metaright
          :ni "A-h" 'org-metaleft
          :ni "A-k" 'org-metaup
          :ni "A-j" 'org-metadown
          ;; Expand tables (or shiftmeta move)
          :ni "A-L" 'narf/org-table-append-field-or-shift-right
          :ni "A-H" 'narf/org-table-prepend-field-or-shift-left
          :ni "A-K" 'narf/org-table-prepend-row-or-shift-up
          :ni "A-J" 'narf/org-table-append-row-or-shift-down

          :i  "C-L" 'narf/org-table-next-field
          :i  "C-H" 'narf/org-table-previous-field
          :i  "C-K" 'narf/org-table-previous-row
          :i  "C-J" 'narf/org-table-next-row

          :i  "C-e" 'org-end-of-line
          :i  "C-a" 'org-beginning-of-line

          :nv "j"   'evil-next-visual-line
          :nv "k"   'evil-previous-visual-line
          :v  "<S-tab>" 'narf/yas-insert-snippet

          :i  "M-a" (λ (evil-visual-state) (org-mark-element))
          :n  "M-a" 'org-mark-element
          :v  "M-a" 'mark-whole-buffer

          :ni "<M-return>"   (λ (narf/org-insert-item 'below))
          :ni "<S-M-return>" (λ (narf/org-insert-item 'above))

          :i  "M-b" (λ (narf/org-surround "*")) ; bold
          :i  "M-u" (λ (narf/org-surround "_")) ; underline
          :i  "M-i" (λ (narf/org-surround "/")) ; italics
          :i  "M-`" (λ (narf/org-surround "+")) ; strikethrough

          :v  "M-b" "S*"
          :v  "M-u" "S_"
          :v  "M-i" "S/"
          :v  "M-`" "S+"

          (:leader
           :n ";" 'helm-org-in-buffer-headings
           :n "oa" 'narf/org-attachment-reveal
           )

          (:localleader
           :n  "/"  'org-sparse-tree
           :n  "?"  'org-tags-view

           :n  "n"  (λ (if (buffer-narrowed-p) (widen) (org-narrow-to-subtree)))
           :n  "e"  'org-edit-special
           :n  "="  'org-align-all-tags
           :nv "l"  'org-insert-link
           :n  "L"  'org-store-link
           :n  "x"  'narf/org-remove-link
           :n  "w"  'writing-mode
           :n  "v"  'variable-pitch-mode
           :n  "SPC" 'narf/org-toggle-checkbox
           :n  "RET" 'org-archive-subtree

           :n  "a"  'org-agenda
           :n  "A"  'narf:org-attachment-list

           :n  "d"  'org-time-stamp
           :n  "D"  'org-time-stamp-inactive
           :n  "i"  'narf/org-toggle-inline-images-at-point
           :n  "t"  (λ (org-todo (if (org-entry-is-todo-p) 'none 'todo)))
           :n  "T"  'org-todo
           :n  "s"  'org-schedule
           :n  "r"  'org-refile
           :n  "R"  (λ (org-metaleft) (org-archive-to-archive-sibling)) ; archive to parent sibling

           :n  "op" 'narf/org-open-project-at-pt
           :n  "oc" 'narf/org-open-contact-at-pt
           :n  "oi" 'narf/org-open-invoice-at-pt
           )

          ;; TODO Improve folding bindings
          :n  "za"  'org-cycle
          :n  "zA"  'org-shifttab
          :n  "zm"  (λ (outline-hide-sublevels 1))
          :n  "zr"  'outline-show-all
          :n  "zo"  'outline-show-subtree
          :n  "zO"  'outline-show-all
          :n  "zc"  'outline-hide-subtree
          :n  "zC"  (λ (outline-hide-sublevels 1))
          :n  "zd"  (lambda (&optional arg) (interactive "p") (outline-hide-sublevels (or arg 3)))

          :m  "]]"  (λ (call-interactively 'org-forward-heading-same-level) (org-beginning-of-line))
          :m  "[["  (λ (call-interactively 'org-backward-heading-same-level) (org-beginning-of-line))
          :m  "]l"  'org-next-link
          :m  "[l"  'org-previous-link

          :n  "RET" 'narf/org-dwim-at-point

          :m  "gh"  'outline-up-heading
          :m  "gj"  'org-forward-heading-same-level
          :m  "gk"  'org-backward-heading-same-level
          :m  "gl"  (λ (call-interactively 'outline-next-visible-heading) (show-children))

          :n  "go"  'org-open-at-point
          :n  "gO"  (λ (let ((org-link-frame-setup (append '((file . find-file-other-window)) org-link-frame-setup))
                             (org-file-apps '(("\\.org$" . emacs)
                                              (t . "open \"%s\""))))
                         (call-interactively 'org-open-at-point)))

          :n  "gQ"  'org-fill-paragraph
          :m  "$"   'org-end-of-line
          :m  "^"   'org-beginning-of-line
          :n  "<"   'org-metaleft
          :n  ">"   'org-metaright
          :v  "<"   (λ (org-metaleft)  (evil-visual-restore))
          :v  ">"   (λ (org-metaright) (evil-visual-restore))
          :n  "-"   'org-cycle-list-bullet
          :n  [tab] 'org-cycle)

        (:map org-src-mode-map
          :n  "<escape>" (λ (message "Exited") (org-edit-src-exit)))

        (:after org-agenda
          (:map org-agenda-mode-map
            :e "<escape>" 'org-agenda-Quit
            :e "C-j" 'org-agenda-next-item
            :e "C-k" 'org-agenda-previous-item
            :e "C-n" 'org-agenda-next-item
            :e "C-p" 'org-agenda-previous-item)))

    ;;; OS-Specific
  (cond (IS-MAC (narf-org-init-for-osx))
        (IS-LINUX nil)
        (IS-WINDOWS nil))

  (progn ;; Org hacks
    (defun org-fontify-meta-lines-and-blocks-1 (limit)
      "Fontify #+ lines and blocks."
      (let ((case-fold-search t))
        (if (re-search-forward
             "^\\([ \t]*#\\(\\(\\+[a-zA-Z]+:?\\| \\|$\\)\\(_\\([a-zA-Z]+\\)\\)?\\)[ \t]*\\(\\([^ \t\n]*\\)[ \t]*\\(.*\\)\\)\\)"
             limit t)
            (let ((beg (match-beginning 0))
                  (block-start (match-end 0))
                  (block-end nil)
                  (lang (match-string 7))
                  (beg1 (line-beginning-position 2))
                  (dc1 (downcase (match-string 2)))
                  (dc3 (downcase (match-string 3)))
                  end end1 quoting block-type ovl)
              (cond
               ((and (match-end 4) (equal dc3 "+begin"))
                ;; Truly a block
                (setq block-type (downcase (match-string 5))
                      quoting (member block-type org-protecting-blocks))
                (when (re-search-forward
                       (concat "^[ \t]*#\\+end" (match-string 4) "\\>.*")
                       nil t)  ;; on purpose, we look further than LIMIT
                  (setq end (min (point-max) (match-end 0))
                        end1 (min (point-max) (1- (match-beginning 0))))
                  (setq block-end (match-beginning 0))
                  (when quoting
                    (org-remove-flyspell-overlays-in beg1 end1)
                    (remove-text-properties beg end
                                            '(display t invisible t intangible t)))
                  (add-text-properties
                   beg end '(font-lock-fontified t font-lock-multiline t))
                  (add-text-properties beg beg1 '(face org-meta-line))
                  (org-remove-flyspell-overlays-in beg beg1)
                  (add-text-properties	; For end_src
                   end1 (min (point-max) (1+ end)) '(face org-meta-line))
                  (org-remove-flyspell-overlays-in end1 end)
                  (cond
                   ((and lang (not (string= lang "")) org-src-fontify-natively)
                    (org-src-font-lock-fontify-block lang block-start block-end)
                   ;;;;;;; EDIT
                    ;; remove old background overlays
                    (mapc (lambda (ov)
                            (if (eq (overlay-get ov 'face) 'org-block-background)
                                (delete-overlay ov)))
                          (overlays-at (/ (+ beg1 block-end) 2)))
                    ;; add a background overlay
                    (setq ovl (make-overlay beg1 block-end))
                    (overlay-put ovl 'face 'org-block-background)
                    (overlay-put ovl 'evaporate t)) ; make it go away when empty
                   ;; (add-text-properties beg1 block-end '(src-block t)))
                   ;;;;;;; /EDIT
                   (quoting
                    (add-text-properties beg1 (min (point-max) (1+ end1))
                                         '(face org-block))) ; end of source block
                   ((not org-fontify-quote-and-verse-blocks))
                   ((string= block-type "quote")
                    (add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-quote)))
                   ((string= block-type "verse")
                    (add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-verse))))
                  (add-text-properties beg beg1 '(face org-block-begin-line))
                  (add-text-properties (min (point-max) (1+ end)) (min (point-max) (1+ end1))
                                       '(face org-block-end-line))
                  t))
               ((string-match-p
                 (format "^\\+%s+:$"
                         (regexp-opt '("title" "author" "email" "date" "address" "location" "contact"
                                       "project" "country" "city" "created" "issued" "paid" "currency")))
                                        dc1)
                ;; (member dc1 '("+title:" "+author:" "+email:" "+date:" "+address:" "+location:" "+contact:" "+project:"))
                (org-remove-flyspell-overlays-in
                 (match-beginning 0)
                 (if (equal "+title:" dc1) (match-end 2) (match-end 0)))
                (add-text-properties
                 beg (match-end 3)
                 (if (member (intern (substring dc1 1 -1)) org-hidden-keywords)
                     '(font-lock-fontified t invisible t)
                   '(font-lock-fontified t face org-document-info-keyword)))
                (add-text-properties
                 (match-beginning 6) (min (point-max) (1+ (match-end 6)))
                 (if (string-equal dc1 "+title:")
                     '(font-lock-fontified t face org-document-title)
                   '(font-lock-fontified t face org-document-info))))
               ((equal dc1 "+caption:")
                (org-remove-flyspell-overlays-in (match-end 2) (match-end 0))
                (remove-text-properties (match-beginning 0) (match-end 0)
                                        '(display t invisible t intangible t))
                (add-text-properties (match-beginning 1) (match-end 3)
                                     '(font-lock-fontified t face org-meta-line))
                (add-text-properties (match-beginning 6) (+ (match-end 6) 1)
                                     '(font-lock-fontified t face org-block))
                t)
               ((member dc3 '(" " ""))
                (org-remove-flyspell-overlays-in beg (match-end 0))
                (add-text-properties
                 beg (match-end 0)
                 '(font-lock-fontified t face font-lock-comment-face)))
               (t ;; just any other in-buffer setting, but not indented
                (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
                (remove-text-properties (match-beginning 0) (match-end 0)
                                        '(display t invisible t intangible t))
                (add-text-properties beg (match-end 0)
                                     '(font-lock-fontified t face org-meta-line))
                t))))))
    ))

(provide 'module-org)
;;; module-org.el ends here
