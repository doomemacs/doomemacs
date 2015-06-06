(define-minor-mode evil-org-mode
  "Evil-mode bindings for org-mode."
  :init-value nil
  :lighter    "!"
  :keymap     (make-sparse-keymap) ; defines evil-org-mode-map
  :group      'evil-org)

(use-package org
  :defines   (org-directory)
  :functions (org-bookmark-jump-unhide outline-next-heading org-end-of-subtree
              outline-flag-region org-remove-inline-images org-display-inline-images
              org-at-item-checkbox-p org-toggle-checkbox org-entry-is-todo-p org-todo
              org-format-outline-path org-get-outline-path)
  :commands (org-capture
             org-capture-string)
  :mode (("\\.org$"  . org-mode)
         ("\\.opml$" . org-mode))
  :init
  (progn
    (add-hook 'org-mode-hook 'narf|enable-tab-width-2)
    (add-hook 'org-mode-hook 'narf|enable-hard-wrap)
    (add-hook 'org-mode-hook 'iimage-mode)
    (add-hook 'org-mode-hook 'org-indent-mode)
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook! 'org-mode-hook (hl-line-mode -1)))
  :config
  (progn
    (after "org-indent" (diminish 'org-indent-mode))
    (after "iimage"     (diminish 'iimage-mode))

    (setq org-directory "~/Dropbox/notes")
    (setq org-project-directory (expand-file-name "projects" org-directory)    ; custom variable
          org-default-notes-file (expand-file-name "notes.org" org-directory)
          org-agenda-files (f-entries org-directory (lambda (path) (and (f-ext? path "org")
                                                                        (not (f-same? path (f-expand "inbox.org" org-directory))))) t)
          org-archive-location (concat org-directory "/archive/%s::")
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
          org-mobile-inbox-for-pull (expand-file-name "mobile.org" org-directory))

    (setq org-src-fontify-natively t
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

    ;; Remove occur highlights on ESC in normal mode
    (advice-add 'evil-force-normal-state :before 'org-remove-occur-highlights)

    ;; (progn ; opml support
    ;;   (defun set-buffer-file-format-to-opml ()
    ;;     (when (string-match "\.opml$" (buffer-file-name))
    ;;       (setq buffer-file-format '(opml))))

    ;;   (defun my--opml-encode (begin end buffer)
    ;;     "Export Org mode buffer to OPML."
    ;;     (let ((org-export-show-temporary-export-buffer nil)
    ;;           (name "*OPML Export Buffer*"))
    ;;       (org-export-to-buffer 'opml name)
    ;;       (erase-buffer)
    ;;       (insert-buffer-substring (get-buffer name))
    ;;       (point-max)))

    ;;   (add-hook 'find-file-hooks 'set-buffer-file-format-to-opml)
    ;;   (add-to-list 'format-alist '(opml "Outline Processor Markup Language"
    ;;                                     "<[?]xml version=\"1.0\"[^>]*[?]>[\n]?.*[\n]?.*[\n]?<opml version=\"[1|2].0\">"
    ;;                                     "~/.emacs.d/elisp/org-opml/opml2org.py" my--opml-encode t))
    ;;   (shut-up (load-library "ox-opml")))

    (progn ; key bindings
      ;; Hide properties PERMANENTLY
      (defun org-cycle-hide-drawers (state)
        "Re-hide all drawers after a visibility state change."
        (when (and (derived-mode-p 'org-mode)
                   (not (memq state '(overview folded contents))))
          (save-excursion
            (let* ((globalp (memq state '(contents all)))
                   (beg (if globalp (point-min) (point)))
                   (end (if globalp (point-max)
                          (if (eq state 'children)
                              (save-excursion (outline-next-heading) (point))
                            (org-end-of-subtree t)))))
              (goto-char beg)
              (while (re-search-forward org-drawer-regexp end t)
                (save-excursion
                  (beginning-of-line 1)
                  (backward-char 1)
                  (let ((b (point)))
                    (if (re-search-forward
                         "^[ \t]*:END:"
                         (save-excursion (outline-next-heading) (point)) t)
                        (outline-flag-region b (point-at-eol) t)
                      (user-error ":END: line missing at position %s" b)))))))))

      (use-package org-agenda
        :config
        (setq org-agenda-restore-windows-after-quit t
              org-agenda-custom-commands '(("x" agenda)
                                           ("y" agenda*)
                                           ("w" todo "WAITING")
                                           ("W" todo-tree "WAITING")
                                           ("to" todo)
                                           ("tp" tags "+Projects")
                                           ("tg" tags-todo "+gamedev")
                                           ("tw" tags-tree "+webdev"))))

      (bind :map org-mode-map
            "RET" nil
            "C-j" nil
            "C-k" nil

            insert [remap narf:inflate-space-maybe] 'self-insert-command

            normal insert
            :map evil-org-mode-map
            "A-l" 'org-metaright       ; M-j
            "A-h" 'org-metaleft        ; M-h
            "A-k" 'org-metaup          ; M-k
            "A-j" 'org-metadown        ; M-j
            "A-l" 'org-shiftmetaright  ; M-L
            "A-h" 'org-shiftmetaleft   ; M-H
            "A-k" 'org-shiftmetaup     ; M-K
            "A-j" 'org-shiftmetadown  ; M-J

            "<M-left>"      'org-beginning-of-line
            "<M-right>"     'org-end-of-line
            "<M-up>"        'org-up-element
            "<M-down>"      'org-down-element

            ",;"  'helm-org-in-buffer-headings
            "M-a" 'mark-whole-buffer
            ", l" 'org-insert-link

            insert
            "C-e"           'org-end-of-line
            "C-a"           'org-beginning-of-line
            ;; Add new header line before this line
            "<S-M-return>" 'narf/org-insert-item-before
            ;; Add new header line after this line
            "<M-return>"   'narf/org-insert-item-after

            "M-b" (λ (narf/org-surround "*"))     ; bold
            "M-u" (λ (narf/org-surround "_"))     ; underline
            "M-i" (λ (narf/org-surround "/"))     ; italics
            "M-`" (λ (narf/org-surround "+"))     ; strikethrough

            visual
            "M-b" "S*"
            "M-u" "S_"
            "M-i" "S/"
            "M-`" "S+"

            normal
            ",=" 'org-align-all-tags
            ",/" 'org-sparse-tree
            ",?" 'org-tags-view
            ",a" 'org-attach
            ",D" 'org-time-stamp-inactive
            ",T" 'org-show-todo-tree
            ",d" 'org-time-stamp
            ",r" 'org-refile
            ",s" 'org-schedule
            ",t" 'org-todo
            "gr" 'org-babel-execute-src-block-maybe
            "gh" 'outline-up-heading
            "gj" 'org-forward-heading-same-level
            "gk" 'org-backward-heading-same-level
            "gl" 'outline-next-visible-heading
            "go"   'org-open-at-point
            "gO"   'org-attach-open
            "gC-o" 'org-attach-reveal
            "gI" (λ (if (> (length org-inline-image-overlays) 0)
                         (org-remove-inline-images)
                       (org-display-inline-images nil t (line-beginning-position) (line-end-position))))
            "gQ" 'org-fill-paragraph
            "ga" 'org-attach
            "gA" 'org-agenda
            "gt" 'org-show-todo-tree
            "]l" 'org-next-link
            "[l" 'org-previous-link
            "$" 'org-end-of-line
            "^" 'org-beginning-of-line
            "<" 'org-metaleft
            ">" 'org-metaright
            "-" 'org-cycle-list-bullet
            ",SPC" 'narf/org-toggle-checkbox
            ",<return>" 'org-archive-subtree
            "<S-M-return>" 'narf/org-insert-item-before
            "<M-return>" 'narf/org-insert-item-after
            "RET" (λ (cond ((org-at-item-checkbox-p)
                            (org-toggle-checkbox))
                           ((org-entry-is-todo-p)
                            (org-todo 'done))))
            [tab] 'org-cycle)

      (after "org-agenda"
        (bind emacs :map org-agenda-mode-map
              "<escape>" 'org-agenda-Quit
              "C-j" 'org-agenda-next-item
              "C-k" 'org-agenda-previous-item
              "C-n" 'org-agenda-next-item
              "C-p" 'org-agenda-previous-item)))))


(provide 'init-org)
;;; init-org.el ends here
