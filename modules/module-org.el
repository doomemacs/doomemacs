;;; module-org.el

(define-minor-mode evil-org-mode
  "Evil-mode bindings for org-mode."
  :init-value nil
  :lighter    "!"
  :keymap     (make-sparse-keymap) ; defines evil-org-mode-map
  :group      'evil-org)

(defvar org-directory "~/Dropbox/org/")
(defvar org-directory-contacts (concat org-directory "work/contacts/"))
(defvar org-directory-projects (concat org-directory "work/projects/"))
(defvar org-directory-invoices (concat org-directory "work/invoices/"))

(add-hook! org-load 'narf|org-init)
(add-hook! org-mode 'evil-org-mode)
(add-hook! org-mode '(narf|enable-tab-width-2 narf|enable-hard-wrap))

;; Fixes when saveplace places the point in a folded position
(defun narf|org-restore-point ()
  (when (outline-invisible-p)
    (ignore-errors
      (save-excursion
        (outline-previous-visible-heading 1)
        (org-show-subtree)))))
(add-hook! org-mode 'narf|org-restore-point)

;; Realign tables and tags on save
(defun narf|org-realign ()
  (org-table-map-tables 'org-table-align 'quietly)
  (org-align-all-tags))
(defun narf|org-update ()
  (org-update-statistics-cookies t))
(add-hook! org-mode
  (add-hook 'before-save-hook 'narf|org-realign nil t)
  (add-hook 'before-save-hook 'narf|org-update nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun narf@org-vars ()
  (setq org-default-notes-file (concat org-directory "notes.org")
        org-agenda-files
        (eval-when-compile
          (f-entries org-directory
                     (lambda (path)
                       (and (f-ext? path "org")
                            (not (f-same? path (f-expand "inbox.org" org-directory)))))
                     t))

        org-archive-location (concat org-directory "/archive/%s::")
        org-attach-directory ".attach/"

        ;; org-mobile-inbox-for-pull (concat org-directory "inbox.org")
        ;; org-mobile-directory "~/Dropbox/Apps/MobileOrg"

        org-catch-invisible-edits nil
        org-confirm-elisp-link-function nil
        org-completion-use-ido t
        org-hidden-keywords '(title)
        org-special-ctrl-a/e t
        org-hierarchical-todo-statistics t
        org-checkbox-hierarchical-statistics nil
        org-tags-column -87
        org-loop-over-headlines-in-active-region t
        org-footnote-auto-label 'plain
        org-log-done t
        org-agenda-window-setup 'current-window
        org-src-window-setup 'current-window
        org-startup-folded 'content
        org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                            (sequence "LEAD(l)" "NEXT(n)" "ACTIVE(a)" "PENDING(p)" "|" "CANCELLED(c)"))
        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . auto))
        org-export-backends '(ascii html latex md opml)

        org-tag-alist '(("@home" . ?h)
                        ("@daily" . ?d)
                        ("@projects" . ?r))

        org-capture-templates
        '(("t" "TODO" entry (file+headline "~/Dropbox/notes/gtd.org" "Inbox") "** TODO %? %u\n%i")
          ("T" "Project TODO" entry (file+headline (narf/project-org-filename) "Tasks") "** TODO %?\n%i" :prepend t)
          ("N" "Project Note" entry (file+headline (narf/project-org-filename) "Notes") "** %u %?\n%i")
          ("c" "Changelog" entry (file+datetree (narf/project-org-filename)) "** %<%H:%M>: %? :unsorted:\n%i" :prepend t)
          ("n" "Note" entry (file+datetree org-default-notes-file) "** %<%H:%M>: %?\n%i" :prepend t)
          ("j" "Journal" entry (file+datetree "~/Dropbox/notes/journal.org") "** %?%^g\nAdded: %U\n%i")
          ("a" "Trivia" entry (file "~/Dropbox/notes/trivia.org") "* %u %?\n%i" :prepend t)
          ("s" "Writing Scraps" entry (file "~/Dropbox/notes/writing.org") "* %u %?\n%i" :prepend t)
          ("v" "Vocab" entry (file (concat org-directory "notes/vocab.org")) "* %?\n%i" :prepend t)
          ("e" "Excerpt" entry (file (concat org-directory "notes/excerpts.org")) "* %u %?\n%i" :prepend t)))

  (add-to-list 'org-link-frame-setup '(file . find-file)))

(defun narf@org-ex ()
  (exmap! "oe[dit]"  'org-edit-special)
  (exmap! "refile"   'org-refile)
  (exmap! "archive"  'org-archive-subtree)
  (exmap! "agenda"   'org-agenda)
  (exmap! "todo"     'org-show-todo-tree)
  (exmap! "link"     'org-link)
  (exmap! "wc"       'narf/org-word-count)
  (exmap! "at[tach]" 'narf:org-attach))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun narf@org-babel ()
  (setq org-confirm-babel-evaluate nil   ; you don't need my permission
        org-src-fontify-natively t       ; make code pretty
        org-src-tab-acts-natively t)

  (require 'ob-http)
  (require 'ob-rust)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (ruby . t) (sh . t) (js . t) (css . t)
     (plantuml . t) (emacs-lisp . t) (matlab . t)
     (latex . t) (calc . t)
     (http . t) (rust . t)))

  (add-to-list 'org-src-lang-modes '("rust" . rust))
  (add-to-list 'org-src-lang-modes '("puml" . puml))
  (add-to-list 'org-src-lang-modes '("plantuml" . puml)))

(defun narf@org-latex ()
  (setq-default
   org-latex-preview-ltxpng-directory (concat narf-temp-dir "ltxpng/")
   org-latex-create-formula-image-program 'dvipng
   org-startup-with-latex-preview nil
   org-highlight-latex-and-related nil
   org-highlight-latex-and-related '(latex))

  (require 'company-math)
  (define-company-backend! org-mode
    (math-symbols-latex
     math-symbols-unicode
     latex-commands
     company-capf
     company-yasnippet
     company-dabbrev-code
     company-keywords))

  (plist-put org-format-latex-options :scale 1.65))

(defun narf@org-looks ()
  (setq org-image-actual-width nil
        org-startup-with-inline-images t
        org-startup-indented t
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts t
        org-use-sub-superscripts '{}
        org-fontify-whole-heading-line nil
        org-hide-emphasis-markers t
        org-hide-leading-stars t)

  (add-hook! org-mode
    (setq truncate-lines nil
          word-wrap t))

  (after! org-indent (diminish 'org-indent-mode))
  (after! iimage     (diminish 'iimage-mode))
  (setq iimage-mode-image-regex-alist
        '(("\\(`?file://\\|\\[\\[\\|<\\|`\\)?\\([-+./_0-9a-zA-Z]+\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|SVG\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|svg\\|tiff?\\|x\\(?:[bp]m\\)\\)\\)\\(\\]\\]\\|>\\|'\\)?" . 2)
          ("<\\(http://.+\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|SVG\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|svg\\|tiff?\\|x\\(?:[bp]m\\)\\)\\)>" . 1)))

  (when IS-MAC
    ;; Display images with quicklook on OSX
    (add-to-list 'org-file-apps '("\\.\\(jpe?g\\|png\\|gif\\|pdf\\)\\'" . "qlmanage -p %s")))

  (add-hook! org-mode (setq line-spacing '0.1))
  ;; (add-hook! org-mode (text-scale-set 1))

  (require 'org-bullets)
  (setq org-bullets-bullet-list '("▪" "•" "◦" "*" ))
  (add-hook! org-mode 'org-bullets-mode)

  (defun narf--not-in-org-src-block (beg end)
    (notany (lambda (overlay)
              (eq (overlay-get overlay 'face) 'org-block-background))
            (overlays-in beg end)))

  (font-lock-add-keywords
   'org-mode `(("\\(#\\+begin_src\\>\\)"
                (0 (narf/show-as ?#)))
               ("\\(#\\+end_src\\>\\)"
                (0 (narf/show-as ?#)))
               ("\\(#\\+begin_quote\\>\\)"
                (0 (narf/show-as ?➤)))
               ("\\(#\\+end_quote\\>\\)"
                (0 (narf/show-as ?➤)))
               ("\\([-+] \\[X\\]\\)"
                (0 (narf/show-as ?☑)))
               ("\\([-+] \\[ \\]\\)"
                (0 (narf/show-as ?☐))))))

(defun narf@org-plantuml ()
  (setq org-plantuml-jar-path puml-plantuml-jar-path)
  (when (file-exists-p "~/.plantuml")
    (add-to-list 'org-babel-default-header-args:plantuml
                 '(:cmdline . "-config ~/.plantuml"))))

(defun narf@org-links ()
  (defun narf--org-file2title (file)
    (s-replace " " "_" (downcase file)))
  (defun narf-org-link-trello (url)
    (browse-url (format "https://trello.com/%s" (url-encode-url url))))
  (defun narf-org-link-contact (id)
    (org-open-file (format "%swork/contacts/%s.org" org-directory (narf--org-file2title id)) t))
  (defun narf-org-link-invoice (id)
    (org-open-file (format "%swork/invoices/%s.yml" org-directory (narf--org-file2title id)) t))
  (defun narf-org-link-project (id)
    (org-open-file (format "%swork/projects/%s.org" org-directory (narf--org-file2title id)) t))

  (org-add-link-type "trello"  'narf-org-link-trello)
  (org-add-link-type "contact" 'narf-org-link-contact)
  (org-add-link-type "project" 'narf-org-link-project)
  (org-add-link-type "invoice" 'narf-org-link-invoice)

  (defun narf-org-complete (type)
    (let ((default-directory (symbol-value (intern (format "org-directory-%ss" type)))))
      (let ((file (org-iread-file-name ">>> ")))
        (format "%s:%s" type (capitalize (s-replace "_" " " (f-base file)))))))
  (defun org-contact-complete-link ()
    (narf-org-complete "contact"))
  (defun org-project-complete-link ()
    (narf-org-complete "project"))
  (defun org-invoice-complete-link ()
    (narf-org-complete "invoice")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun narf|org-init ()
  (after! autoinsert
    (add-template! (format "%s.\\.org$" org-directory-contacts) "__contact.org"  'org-mode)
    (add-template! (format "%s.\\.org$" org-directory-projects) "__projects.org" 'org-mode)
    (add-template! (format "%s.\\.yml$" org-directory-invoices) "__invoices.org" 'org-mode))

  (advice-add 'evil-force-normal-state :before 'org-remove-occur-highlights)

  (narf@org-vars)
  (narf@org-babel)
  (narf@org-latex)
  (narf@org-looks)
  (narf@org-links)
  (narf@org-plantuml)
  (add-hook! org-mode 'narf@org-ex)

  ;; Align table, if in table when exiting insert mode
  (defun narf|org-realign-table ()
    (when (org-table-p)
      (org-table-align)))
  (add-hook! org-mode
    (add-hook 'evil-insert-state-exit-hook 'narf|org-realign-table t t))

  (require 'org-agenda)
  (setq org-agenda-restore-windows-after-quit t
        org-agenda-custom-commands
        '(("x" agenda)
          ("y" agenda*)
          ("w" todo "WAITING")
          ("W" todo-tree "WAITING")
          ("to" todo)
          ("tp" tags "+Projects")
          ("tg" tags-todo "+gamedev")
          ("tw" tags-tree "+webdev")))

  (defun narf-project-org-filename (cat)
    (interactive (list (completing-read "Choose category:"
                                        (mapcar 'f-filename (f-directories org-project-directory)))))
    (expand-file-name (concat (file-name-nondirectory (directory-file-name (narf/project-root))) ".org")
                      (expand-file-name cat org-project-directory)))

  ;; Add element delimiter text-objects so we can use evil-surround to
  ;; manipulate them.
  (define-text-object! "$" "\\$" "\\$")
  (define-text-object! "*" "\\*" "\\*")
  (define-text-object! "/" "/" "/")
  (define-text-object! "_" "_" "_")
  (define-text-object! "=" "=" "=")
  (define-text-object! "~" "~" "~")

  ;; Keybinds
  (bind! (:map org-mode-map
           "RET" nil
           "C-j" nil
           "C-k" nil

           :i [remap narf/inflate-space-maybe] 'org-self-insert-command
           :i "RET" 'org-return-indent)

         (:map evil-org-mode-map
           :ni "A-l" 'org-metaright       ; M-j
           :ni "A-h" 'org-metaleft        ; M-h
           :ni "A-k" 'org-metaup          ; M-k
           :ni "A-j" 'org-metadown        ; M-j

           ;; Expand tables (or shiftmeta move)
           :ni "A-L" 'narf/org-table-append-row-or-shift-right
           :ni "A-H" 'narf/org-table-prepend-row-or-shift-left
           :ni "A-K" 'narf/org-table-prepend-field-or-shift-up
           :ni "A-J" 'narf/org-table-append-field-or-shift-down

           :i  "C-L"  'narf/org-table-next-field
           :i  "C-H"  'narf/org-table-previous-field
           :i  "C-K"  'narf/org-table-previous-row
           :i  "C-J"  'narf/org-table-next-row

           :nv "j" 'evil-next-visual-line
           :nv "k" 'evil-previous-visual-line

           :ni "M-a" 'mark-whole-buffer

           :n  "gr" 'org-babel-execute-src-block-maybe

           :i  "C-e"          'org-end-of-line
           :i  "C-a"          'org-beginning-of-line
           ;;  Add new header line before this line
           :i  "<S-M-return>" 'narf/org-insert-item-before
           ;;  Add new header line after this line
           :i  "<M-return>"   'narf/org-insert-item-after

           :i  "M-b" (λ (narf/org-surround "*"))     ; bold
           :i  "M-u" (λ (narf/org-surround "_"))     ; underline
           :i  "M-i" (λ (narf/org-surround "/"))     ; italics
           :i  "M-`" (λ (narf/org-surround "+"))     ; strikethrough

           :v  "M-b" "S*"
           :v  "M-u" "S_"
           :v  "M-i" "S/"
           :v  "M-`" "S+"

           :i  "M-b" (λ (if (org-element-bold-parser) (evil-surround-delete ?\*) (insert "**") (backward-char)))
           :i  "M-u" (λ (if (org-element-bold-parser) (evil-surround-delete ?\_) (insert "__") (backward-char)))
           :i  "M-i" (λ (if (org-element-bold-parser) (evil-surround-delete ?\/) (insert "//") (backward-char)))
           :i  "M-`" (λ (if (org-element-bold-parser) (evil-surround-delete ?\+) (insert "++") (backward-char)))

           :n  ",;" 'helm-org-in-buffer-headings
           :nv ",l" 'org-insert-link
           :n  ",=" 'org-align-all-tags
           :n  ",f" 'org-sparse-tree
           :n  ",?" 'org-tags-view
           :n  ",a" 'org-attach
           :n  ",D" 'org-time-stamp-inactive
           :n  ",T" 'org-show-todo-tree
           :n  ",d" 'org-time-stamp
           :n  ",r" 'org-refile
           :n  ",s" 'org-schedule
           :n  ",t" 'org-todo
           :n  ",SPC" 'narf/org-toggle-checkbox
           :n  ",<return>" 'org-archive-subtree

           :n "za" 'org-cycle
           :n "zA" 'org-shifttab
           :n "zm" 'hide-body
           :n "zr" 'show-all
           :n "zo" 'show-subtree
           :n "zO" 'show-all
           :n "zc" 'hide-subtree
           :n "zC" 'hide-all

           :n  "gr" (λ (cond ((org-table-p)
                              (org-table-align))
                             ((org-in-src-block-p)
                              (org-babel-execute-src-block))
                             ((org-inside-LaTeX-fragment-p)
                              (org-toggle-latex-fragment))
                             (t (org-toggle-inline-images))))
           :m  "gh" 'outline-up-heading
           :m  "gj" (λ (hide-subtree) (call-interactively 'org-forward-heading-same-level) (show-children))
           :m  "gk" (λ (hide-subtree) (call-interactively 'org-backward-heading-same-level) (show-children))
           ;;  :m "gk" 'org-backward-heading-same-level
           :m  "gl" (λ (call-interactively 'outline-next-visible-heading) (show-children))

           :n  "go" 'org-open-at-point
           :n  "gO" (λ (let ((org-link-frame-setup (append '((file . find-file-other-window)) org-link-frame-setup)))
                         (call-interactively 'org-open-at-point)))

           :n  "ga" 'org-attach
           :n  "gC-o" 'org-attach-reveal
           :n  "gI" (λ (if (> (length org-inline-image-overlays) 0)
                           (org-remove-inline-images)
                         (org-display-inline-images nil t (line-beginning-position) (line-end-position))))
           :n  "gQ" 'org-fill-paragraph
           :n  "gA" 'org-agenda
           :n  "gt" 'org-show-todo-tree
           :m  "]l" 'org-next-link
           :m  "[l" 'org-previous-link
           :m  "$" 'org-end-of-line
           :m  "^" 'org-beginning-of-line
           :n  "<" 'org-metaleft
           :n  ">" 'org-metaright
           :v  "<" (λ (org-metaleft)  (evil-visual-restore))
           :v  ">" (λ (org-metaright) (evil-visual-restore))
           :n  "-" 'org-cycle-list-bullet
           :n  "<S-M-return>" 'narf/org-insert-item-before
           :n  "<M-return>"   'narf/org-insert-item-after
           :n  "RET" (λ (cond ((org-at-item-checkbox-p)
                               (org-toggle-checkbox))
                              ((org-entry-is-todo-p)
                               (org-todo 'done))))
           :n  [tab] 'org-cycle)

         (:after org-agenda
           (:map org-agenda-mode-map
             :e "<escape>" 'org-agenda-Quit
             :e "C-j" 'org-agenda-next-item
             :e "C-k" 'org-agenda-previous-item
             :e "C-n" 'org-agenda-next-item
             :e "C-p" 'org-agenda-previous-item)))

  (progn ;; Org hacks
    (defun org-insert-link (&optional complete-file link-location default-description)
      (interactive "P")
      (let* ((wcf (current-window-configuration))
             (origbuf (current-buffer))
             (region (if (org-region-active-p) (buffer-substring (region-beginning) (region-end))))
             (remove (and region (list (region-beginning) (region-end))))
             (desc region)
             tmphist ; byte-compile incorrectly complains about this
             (link link-location)
             (abbrevs org-link-abbrev-alist-local)
             entry file all-prefixes auto-desc)
        (cond
         (link-location) ; specified by arg, just use it.
         ((org-in-regexp org-bracket-link-regexp 1)
          ;; We do have a link at point, and we are going to edit it.
          (setq remove (list (match-beginning 0) (match-end 0)))
          (setq desc (if (match-end 3) (org-match-string-no-properties 3)))
          (setq link (read-string "Link: "
                                  (org-link-unescape
                                   (org-match-string-no-properties 1)))))
         ((or (org-in-regexp org-angle-link-re)
              (org-in-regexp org-plain-link-re))
          ;; Convert to bracket link
          (setq remove (list (match-beginning 0) (match-end 0))
                link (read-string "Link: " (org-remove-angle-brackets (match-string 0)))))
         ((member complete-file '((4) (16)))
          ;; Completing read for file names.
          (setq link (org-file-complete-link complete-file)))
         (t (org-link-fontify-links-to-this-file)
            (setq tmphist (append (mapcar 'car org-stored-links) org-insert-link-history))
            (setq all-prefixes (append (mapcar 'car abbrevs)
                                       (mapcar 'car org-link-abbrev-alist)
                                       org-link-types))
            (unwind-protect
                (progn (setq link (org-completing-read
                                   "Link: " (append (mapcar (lambda (x) (concat x ":")) all-prefixes)
                                                    (mapcar 'car org-stored-links))
                                   nil nil nil 'tmphist (caar org-stored-links)))
                       (if (not (string-match "\\S-" link))
                           (user-error "No link selected"))
                       (mapc (lambda(l)
                               (when (equal link (cadr l)) (setq link (car l) auto-desc t)))
                             org-stored-links)
                       (if (or (member link all-prefixes)
                               (and (equal ":" (substring link -1))
                                    (member (substring link 0 -1) all-prefixes)
                                    (setq link (substring link 0 -1))))
                           (setq link (with-current-buffer origbuf
                                        (org-link-try-special-completion link)))))
              (set-window-configuration wcf))
            (setq entry (assoc link org-stored-links))
            (or entry (push link org-insert-link-history))
            (setq desc (or desc (nth 1 entry)))))
        (if (funcall (if (equal complete-file '(64)) 'not 'identity)
                     (not org-keep-stored-link-after-insertion))
            (setq org-stored-links (delq (assoc link org-stored-links)
                                         org-stored-links)))
        (if (and (string-match org-plain-link-re link)
                 (not (string-match org-ts-regexp link)))
            (setq link (org-remove-angle-brackets link)))
        (when (and buffer-file-name
                   (string-match "^file:\\(.+?\\)::\\(.+\\)" link))
          (let* ((path (match-string 1 link))
                 (case-fold-search nil)
                 (search (match-string 2 link)))
            (save-match-data
              (if (equal (file-truename buffer-file-name) (file-truename path))
                  (setq link search)))))
        (when (string-match "^\\(file:\\|docview:\\)\\(.*\\)" link)
          (let* ((type (match-string 1 link))
                 (path (match-string 2 link))
                 (origpath path)
                 (case-fold-search nil))
            (cond ((or (eq org-link-file-path-type 'absolute)
                       (equal complete-file '(16)))
                   (setq path (abbreviate-file-name (expand-file-name path))))
                  ((eq org-link-file-path-type 'noabbrev)
                   (setq path (expand-file-name path)))
                  ((eq org-link-file-path-type 'relative)
                   (setq path (file-relative-name path)))
                  (t (save-match-data
                       (if (string-match (concat "^" (regexp-quote (expand-file-name
                                                                    (file-name-as-directory default-directory))))
                                         (expand-file-name path))
                           (setq path (substring (expand-file-name path) (match-end 0)))
                         (setq path (abbreviate-file-name (expand-file-name path)))))))
            (setq link (concat type path))
            (if (equal desc origpath)
                (setq desc path))))
        (if org-make-link-description-function
            (setq desc
                  (or (condition-case nil
                          (funcall org-make-link-description-function link desc)
                        (error (progn (message "Can't get link description from `%s'"
                                               (symbol-name org-make-link-description-function))
                                      (sit-for 2) nil)))
                      (read-string "Description: " default-description)))
          (if default-description (setq desc default-description)
            (setq desc (or (and auto-desc desc)
                           (read-string "Description: " desc)))))
        (unless (string-match "\\S-" desc) (setq desc nil))
        (if remove (apply 'delete-region remove))
        (insert (org-make-link-string link desc))))))

(provide 'module-org)
;;; module-org.el ends here
