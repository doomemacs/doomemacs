;;; module-org.el

(define-minor-mode evil-org-mode
  "Evil-mode bindings for org-mode."
  :init-value nil
  :lighter    "!"
  :keymap     (make-sparse-keymap) ; defines evil-org-mode-map
  :group      'evil-org)

(defvar org-directory "~/Dropbox/notes/")
(associate! org-mode :match "/Dropbox/notes/.+$")

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
  (org-update-checkbox-count-maybe t))
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

        org-confirm-elisp-link-function nil
        org-completion-use-ido t
        org-hidden-keywords '(title)
        org-special-ctrl-a/e t
        org-hierarchical-todo-statistics t
        org-checkbox-hierarchical-statistics t
        org-tags-column -87
        org-log-done t
        org-startup-folded t
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "DOING(s)" "PENDING(p)")
                            (sequence "|" "CANCELLED(c)"))
        org-blank-before-new-entry '((heading . auto) (plain-list-item . auto))
        org-export-backends '(ascii html latex md opml)

        org-tag-alist '(("@home" . ?h)
                        ("@daily" . ?d)
                        ("@projects" . ?r))

        org-capture-templates
        '(("t" "TODO" entry (file+headline "~/Dropbox/notes/todo.org" "Inbox") "* TODO %? %u\n%i")
          ("T" "Project TODO" entry (file+headline (narf/project-org-filename) "Tasks") "** TODO %?\n%i" :prepend t)
          ("N" "Project Note" entry (file+headline (narf/project-org-filename) "Notes") "** %u %?\n%i")
          ("c" "Changelog" entry (file+datetree (narf/project-org-filename)) "** %<%H:%M>: %? :unsorted:\n%i" :prepend t)
          ("n" "Note" entry (file+datetree org-default-notes-file) "** %<%H:%M>: %?\n%i" :prepend t)
          ("j" "Journal" entry (file+datetree "~/Dropbox/notes/journal.org") "** %?%^g\nAdded: %U\n%i")
          ("a" "Trivia" entry (file "~/Dropbox/notes/trivia.org") "* %u %?\n%i" :prepend t)
          ("s" "Writing Scraps" entry (file "~/Dropbox/notes/writing.org") "* %u %?\n%i" :prepend t)
          ("v" "Vocab" entry (file "~/Dropbox/notes/vocab.org") "* %?\n%i" :prepend t)
          ("e" "Excerpt" entry (file "~/Dropbox/notes/excerpts.org") "* %u %?\n%i" :prepend t)))

  (add-to-list 'org-link-frame-setup '(file . find-file)))

(defun narf@org-ex ()
  (exmap! "edit"     'org-edit-special)
  (exmap! "refile"   'org-refile)
  (exmap! "archive"  'org-archive-subtree)
  (exmap! "agenda"   'org-agenda)
  (exmap! "todo"     'org-show-todo-tree)
  (exmap! "link"     'org-link)
  (exmap! "wc"       'narf/org-word-count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun narf@org-babel ()
  (setq org-confirm-babel-evaluate nil    ; you don't need my permission
        org-src-fontify-natively t       ; make code pretty
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (ruby . t) (sh . t) (js . t) (css . t)
     (plantuml . t) (emacs-lisp . t) (matlab . t)
     (latex . t) (calc . t)))

  (add-to-list 'org-src-lang-modes '("puml" . puml))
  (add-to-list 'org-src-lang-modes '("plantuml" . puml)))

(defun narf@org-latex ()
  (setq-default
   org-latex-preview-ltxpng-directory (concat narf-temp-dir "ltxpng/")
   org-latex-create-formula-image-program 'dvipng
   org-startup-with-latex-preview t
   org-highlight-latex-and-related '(latex))
  (plist-put org-format-latex-options :scale 1.5))

(defun narf@org-looks ()
  (setq org-image-actual-width nil
        org-startup-with-inline-images t
        org-pretty-entities t
        org-fontify-whole-heading-line t
        org-hide-emphasis-markers t)
  (after! iimage (diminish 'iimage-mode))
  ;; (setq iimage-mode-image-regex-alist
  ;;       '(("\\(`?file://\\|\\[\\[\\|<\\|`\\)?\\([-+./_0-9a-zA-Z]+\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|SVG\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|svg\\|tiff?\\|x\\(?:[bp]m\\)\\)\\)\\(\\]\\]\\|>\\|'\\)?" . 2)
  ;;         ("<\\(http://.+\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|SVG\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|svg\\|tiff?\\|x\\(?:[bp]m\\)\\)\\)>" . 1)))
  (when IS-MAC
    ;; Display images with quicklook on OSX
    (add-to-list 'org-file-apps '("\\.\\(jpe?g\\|png\\|gif\\|pdf\\)\\'" . "qlmanage -p %s")))

  (setq org-hide-leading-stars t)
  (after! org-indent (diminish 'org-indent-mode))
  ;; (add-hook! org-mode 'org-indent-mode)
  (add-hook! org-mode (setq line-spacing '0.2))

  (require 'org-bullets)
  (setq org-bullets-bullet-list '("▪" "◇" "•" "◦" ))
  (add-hook! org-mode 'org-bullets-mode))

(defun narf@org-plantuml ()
  (setq org-plantuml-jar-path puml-plantuml-jar-path)
  (when (file-exists-p "~/.plantuml")
    (add-to-list 'org-babel-default-header-args:plantuml
                 '(:cmdline . "-config ~/.plantuml"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun narf|org-init ()
  (advice-add 'evil-force-normal-state :before 'org-remove-occur-highlights)

  (narf@org-vars)
  (narf@org-babel)
  (narf@org-latex)
  (narf@org-looks)
  (narf@org-plantuml)
  (add-hook! org-mode 'narf@org-ex)

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

  ;; Custom link types
  (org-add-link-type "trello" 'narf-org-link-trello)
  (org-add-link-type "contact" 'narf-org-link-contact)
  (org-add-link-type "invoice" 'narf-org-link-invoice)

  (defun narf-org-link-trello (url)
    "Open a trello url"
    (browse-url (format "https://trello.com/%s" (url-encode-url url))))
  (defun narf-org-link-contact (id)
    (org-open-file (format "%scontacts.org" org-directory) t nil (format "#%s" id)))
  (defun narf-org-link-invoice (id))

  (defun narf-project-org-filename (cat)
    (interactive (list (completing-read "Choose category:"
                                        (mapcar 'f-filename (f-directories org-project-directory)))))
    (expand-file-name (concat (file-name-nondirectory (directory-file-name (narf/project-root))) ".org")
                      (expand-file-name cat org-project-directory)))

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
           :ni "A-l" 'org-shiftmetaright  ; M-L
           :ni "A-h" 'org-shiftmetaleft   ; M-H
           :ni "A-k" 'org-shiftmetaup     ; M-K
           :ni "A-j" 'org-shiftmetadown   ; M-J

           :ni "<M-left>"      'org-beginning-of-line
           :ni "<M-right>"     'org-end-of-line
           :ni "<M-up>"        'org-up-element
           :ni "<M-down>"      'org-down-element

           :ni "M-a" 'mark-whole-buffer

           :n "gr" 'org-babel-execute-src-block-maybe

           :i "C-e"          'org-end-of-line
           :i "C-a"          'org-beginning-of-line
           ;; Add new header line before this line
           :i "<S-M-return>" 'narf/org-insert-item-before
           ;; Add new header line after this line
           :i "<M-return>"   'narf/org-insert-item-after

           :i "M-b" (λ (narf/org-surround "*"))     ; bold
           :i "M-u" (λ (narf/org-surround "_"))     ; underline
           :i "M-i" (λ (narf/org-surround "/"))     ; italics
           :i "M-`" (λ (narf/org-surround "+"))     ; strikethrough

           :v "M-b" "S*"
           :v "M-u" "S_"
           :v "M-i" "S/"
           :v "M-`" "S+"

           :n  ",;" 'helm-org-in-buffer-headings
           :nv ",l" 'org-insert-link
           :n ",=" 'org-align-all-tags
           :n ",/" 'org-sparse-tree
           :n ",?" 'org-tags-view
           :n ",a" 'org-attach
           :n ",D" 'org-time-stamp-inactive
           :n ",T" 'org-show-todo-tree
           :n ",d" 'org-time-stamp
           :n ",r" 'org-refile
           :n ",s" 'org-schedule
           :n ",t" 'org-todo
           :n ",SPC" 'narf/org-toggle-checkbox
           :n ",<return>" 'org-archive-subtree

           :n "gr" 'org-babel-execute-src-block-maybe
           :m "gh" 'outline-up-heading
           :m "gj" (λ (hide-subtree) (call-interactively 'org-forward-heading-same-level) (show-children))
           :m "gk" (λ (hide-subtree) (call-interactively 'org-backward-heading-same-level) (show-children))
           ;; :m "gk" 'org-backward-heading-same-level
           :m "gl" (λ (call-interactively 'outline-next-visible-heading) (show-children))
           :n "go"   'org-open-at-point
           :n "gO"   'org-attach-open
           :n "gC-o" 'org-attach-reveal
           :n "gI" (λ (if (> (length org-inline-image-overlays) 0)
                          (org-remove-inline-images)
                        (org-display-inline-images nil t (line-beginning-position) (line-end-position))))
           :n "gQ" 'org-fill-paragraph
           :n "ga" 'org-attach
           :n "gA" 'org-agenda
           :n "gt" 'org-show-todo-tree
           :m "]l" 'org-next-link
           :m "[l" 'org-previous-link
           :m "$" 'org-end-of-line
           :m "^" 'org-beginning-of-line
           :nv "<" 'org-metaleft
           :nv ">" 'org-metaright
           :n "-" 'org-cycle-list-bullet
           :n "<S-M-return>" 'narf/org-insert-item-before
           :n "<M-return>" 'narf/org-insert-item-after
           :n "RET" (λ (cond ((org-at-item-checkbox-p)
                              (org-toggle-checkbox))
                             ((org-entry-is-todo-p)
                              (org-todo 'done))))
           :n [tab] 'org-cycle)

         (:after org-agenda
           (:map org-agenda-mode-map
             :e "<escape>" 'org-agenda-Quit
             :e "C-j" 'org-agenda-next-item
             :e "C-k" 'org-agenda-previous-item
             :e "C-n" 'org-agenda-next-item
             :e "C-p" 'org-agenda-previous-item))))

(provide 'module-org)
;;; module-org.el ends here
