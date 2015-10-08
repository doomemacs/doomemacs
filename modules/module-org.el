;;; module-org.el

(defvar org-directory "~/Dropbox/notes/")

(define-minor-mode evil-org-mode
  "Evil-mode bindings for org-mode."
  :init-value nil
  :lighter    "!"
  :keymap     (make-sparse-keymap) ; defines evil-org-mode-map
  :group      'evil-org)

(use-package org
  :functions (org-bookmark-jump-unhide outline-next-heading org-end-of-subtree
              outline-flag-region org-remove-inline-images org-display-inline-images
              org-at-item-checkbox-p org-toggle-checkbox org-entry-is-todo-p org-todo
              org-format-outline-path org-get-outline-path)
  :commands (org-capture org-capture-string)
  :mode (("\\.org$"  . org-mode)
         ("\\.opml$" . org-mode))
  :init
  (add-hook! org-mode (hl-line-mode -1))
  (add-hook! org-mode '(narf|enable-tab-width-2
                        narf|enable-hard-wrap
                        org-indent-mode
                        evil-org-mode))
  (add-hook! org-load 'narf-init-org)

  (defun narf--org-all-files (path)
      (and (f-ext? path "org") (not (f-same? path (f-expand "inbox.org" org-directory)))))

  (defun narf-init-org ()
    (after! org-indent (diminish 'org-indent-mode))
    (after! iimage     (diminish 'iimage-mode))

    (setq org-project-directory  (! (concat org-directory "projects")) ; not an org var
          org-default-notes-file (! (concat org-directory "notes.org"))
          org-agenda-files (f-entries org-directory 'narf--org-all-files t)
          org-archive-location (! (concat org-directory "/archive/%s::"))
          org-confirm-babel-evaluate nil
          org-src-tab-acts-natively t
          org-image-actual-width 250
          org-startup-with-inline-images t
          org-completion-use-ido t
          org-hidden-keywords '(title)
          org-special-ctrl-a/e t
          org-hide-leading-stars t
          org-hierarchical-todo-statistics t
          org-checkbox-hierarchical-statistics t
          org-tags-column -87
          org-log-done t
          org-confirm-elisp-link-function nil
          org-startup-folded 'content
          org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                              (sequence "DOING(s)" "PENDING(p)")
                              (sequence "|" "CANCELLED(c)"))
          org-mobile-directory "~/Dropbox/Apps/MobileOrg"
          org-mobile-inbox-for-pull (! (expand-file-name "mobile.org" org-directory))
          org-src-fontify-natively t
          org-blank-before-new-entry '((heading . auto) (plain-list-item . auto))
          org-export-backends '(ascii html latex md opml))

    (add-to-list 'org-link-frame-setup '(file . find-file))

    (setq org-tag-alist '(("@home" . ?h)
                          ("@daily" . ?d)
                          ("@invoices" . ?i)
                          ("@personal" . ?p)
                          ("@learning" . ?l)
                          ("@dev" . ?v)
                          ("@writing" . ?w)
                          ("@projects" . ?r)))

    (setq org-capture-templates
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

    (setq iimage-mode-image-regex-alist
          '(("\\(`?file://\\|\\[\\[\\|<\\|`\\)?\\([-+./_0-9a-zA-Z]+\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|SVG\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|svg\\|tiff?\\|x\\(?:[bp]m\\)\\)\\)\\(\\]\\]\\|>\\|'\\)?" . 2)
            ("<\\(http://.+\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|SVG\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|svg\\|tiff?\\|x\\(?:[bp]m\\)\\)\\)>" . 1)))

    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((python . t)
                                   (ruby . t)
                                   (sh . t)
                                   (emacs-lisp . t)
                                   (matlab . t)
                                   (latex . t)))

    (advice-add 'evil-force-normal-state :before 'org-remove-occur-highlights)
    ;; (advice-add 'org-cycle-hide-drawers :override 'narf/org-cycle-hide-drawers)

    (use-package org-agenda
      :config
      (setq org-agenda-restore-windows-after-quit t
            org-agenda-custom-commands
            '(("x" agenda)
              ("y" agenda*)
              ("w" todo "WAITING")
              ("W" todo-tree "WAITING")
              ("to" todo)
              ("tp" tags "+Projects")
              ("tg" tags-todo "+gamedev")
              ("tw" tags-tree "+webdev"))))

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
             :ni "A-j" 'org-shiftmetadown  ; M-J

             :ni "<M-left>"      'org-beginning-of-line
             :ni "<M-right>"     'org-end-of-line
             :ni "<M-up>"        'org-up-element
             :ni "<M-down>"      'org-down-element

             :n ",;" 'helm-org-in-buffer-headings
             :n ",l" 'org-insert-link
             :ni "M-a" 'mark-whole-buffer

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
             :m "gj" 'org-forward-heading-same-level
             :m "gk" 'org-backward-heading-same-level
             :m "gl" 'outline-next-visible-heading
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
             :n "<" 'org-metaleft
             :n ">" 'org-metaright
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
               :e "C-p" 'org-agenda-previous-item)))))

(provide 'module-org)
;;; module-org.el ends here
