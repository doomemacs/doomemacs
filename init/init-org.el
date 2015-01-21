(use-package org
  :mode (("\\.org$" . org-mode)
         ("\\.opml$" . org-mode))
  :init
  (progn
    (add-hook 'org-mode-hook 'enable-tab-width-2)
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (add-hook 'org-mode-hook 'iimage-mode)

    ;; Reset evil to ensure evil-org-mode's maps work
    (add-hook! 'org-mode-hook (evil-mode nil) (evil-mode 1))

    (setq org-directory "~/Dropbox/notes"
          org-default-notes-file "~/Dropbox/notes/notes.org"
          org-agenda-files '("~/Dropbox/notes"
                             "~/Dropbox/notes/projects"
                             "~/Dropbox/notes/projects/dev"
                             "~/Dropbox/notes/projects/gamedev"
                             "~/Dropbox/notes/projects/webdev")
          org-archive-location "~/Dropbox/notes/archive/%s.org::"
          org-confirm-babel-evaluate nil
          org-src-tab-acts-natively t
          org-image-actual-width 300
          org-startup-with-inline-images t)

    (setq org-completion-use-ido t
          org-hidden-keywords '(title)
          org-special-ctrl-a/e t
          org-hide-leading-stars t
          org-hierarchical-todo-statistics t
          org-checkbox-hierarchical-statistics t
          org-tags-column -87
          org-log-done t
          org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "DOING(s)" "PENDING(p)")
            (sequence "|" "CANCELLED(c)")))

    (setq org-src-fontify-natively t)
    (setq org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
    (setq org-export-backends '(ascii html latex md opml))

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
            ("T" "Project TODO" entry (file+headline (concat (projectile-project-root) "/TODO.org") "Unsorted") "** %u %?\n%i" :prepend t)
            ("c" "Changelog" entry (file+headline (concat (projectile-project-root) "/CHANGELOG.org") "Unsorted") "** %u %? :unsorted:\n%i" :prepend t)
            ("n" "Note" entry (file+headline org-default-notes-file "Unfiled") "** %T %?\n%i" :prepend t)
            ("j" "Journal" entry (file+datetree "~/Dropbox/notes/journal.org") "** %?%^g\nAdded: %U\n%i")
            ("a" "Trivia" entry (file "~/Dropbox/notes/trivia.org") "* %u %?\n%i" :prepend t)
            ("s" "Writing Scraps" entry (file "~/Dropbox/notes/writing.org") "* %u %?\n%i" :prepend t)
            ("v" "Vocab" entry (file "~/Dropbox/notes/vocab.org") "* %?\n%i" :prepend t)
            ("e" "Excerpt" entry (file "~/Dropbox/notes/excerpts.org") "* %u %?\n%i" :prepend t)))

    (setq org-agenda-custom-commands
          '(("x" agenda)
            ("y" agenda*)
            ("w" todo "WAITING")
            ("W" todo-tree "WAITING")
            ("to" todo)
            ("tp" tags "+Projects")
            ("tg" tags-todo "+gamedev")
            ("tw" tags-tree "+webdev"))))
  :config
  (progn
    (message "Org-mode loaded")

    (setq iimage-mode-image-regex-alist
                  '(("\\(`?file://\\|\\[\\[\\|<\\|`\\)?\\([-+./_0-9a-zA-Z]+\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|SVG\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|svg\\|tiff?\\|x\\(?:[bp]m\\)\\)\\)\\(\\]\\]\\|>\\|'\\)?" . 2)
                    ("<\\(http://.+\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|SVG\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|svg\\|tiff?\\|x\\(?:[bp]m\\)\\)\\)>" . 1)))

    (push '("\\*Org.+\\*" :regexp t :width 0.3 :position bottom) popwin:special-display-config)

    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((python . t)
                                   (ruby . t)
                                   (sh . t)
                                   (emacs-lisp . t)
                                   (matlab . t)
                                   (latex . t)))

    ;; Remove occur highlights on ESC in normal mode
    (defadvice evil-force-normal-state (before evil-esc-org-remove-highlights activate)
      (org-remove-occur-highlights))

    ;; Auto update cookies
    (defun my--org-mode-update-cookies ()
      (when (eq major-mode 'org-mode) (org-update-parent-todo-statistics) (org-update-statistics-cookies nil)))
    (add-hook 'evil-normal-state-entry-hook 'my--org-mode-update-cookies)

    (define-minor-mode evil-org-mode
      :init-value nil
      :lighter " EvilOrg"
      :keymap (make-sparse-keymap) ; defines evil-org-mode-map
      :group 'evil-org)

    ;; (use-package org-present
    ;;   :config
    ;;   (progn
    ;;     (defun my--org-present-mode-on ()
    ;;       (org-present-big)
    ;;       (org-display-inline-images)
    ;;       (org-present-hide-cursor)
    ;;       (org-present-read-only))

    ;;     (defun my--org-present-mode-off ()
    ;;       (org-present-small)
    ;;       (org-remove-inline-images)
    ;;       (org-present-show-cursor)
    ;;       (org-present-read-write))

    ;;     (add-hook 'org-present-mode-hook 'my--org-present-mode-on)
    ;;     (add-hook 'org-present-mode-quit-hook 'my--org-present-mode-off)))

    (progn ; opml support
      (defun set-buffer-file-format-to-opml ()
        (when (string-match "\.opml$" (buffer-file-name))
          (setq buffer-file-format '(opml))))

      (defun my--opml-encode (begin end buffer)
        "Export Org mode buffer to OPML."
        (let ((org-export-show-temporary-export-buffer nil)
              (name "*OPML Export Buffer*"))
          (org-export-to-buffer 'opml name)
          (erase-buffer)
          (insert-buffer-substring (get-buffer name))
          (point-max)))

      (add-hook 'find-file-hooks 'set-buffer-file-format-to-opml)
      (add-to-list 'format-alist '(opml "Outline Processor Markup Language"
                                        "<[?]xml version=\"1.0\"[^>]*[?]>[\n]?.*[\n]?.*[\n]?<opml version=\"[1|2].0\">"
                                        "~/.emacs.d/elisp/org-opml/opml2org.py" my--opml-encode t))
      (shut-up (load-library "ox-opml")))

    (progn ; key bindings
      (defun my--org-in-list-p ()
        (and (save-excursion (search-backward-regexp "^ *\\([0-9]+[\.)]\\|[-*+]\\) " (line-beginning-position) t))
             (org-in-item-p)))
      (defun my--org-insert-item-after ()
        "Inserts a new heading or item, depending on the context."
        (interactive)
        (org-end-of-line)
        (cond ((org-at-item-checkbox-p)
               (org-insert-heading)
               (insert "[ ] "))
              ((my--org-in-list-p)
               (org-insert-heading))
              ((org-on-heading-p)
               (org-insert-heading-after-current))
              (t
               (org-insert-heading-after-current)
               (delete-char 1)))
        (evil-insert-state))

      ;; TODO Check if this and -forward can be combined
      (defun my--org-insert-item-before ()
        "Inserts a new heading or item, depending on the context."
        (interactive)
        (evil-first-non-blank)
        (cond ((org-at-item-checkbox-p)
               (org-insert-heading)
               (insert "[ ] "))
              ((my--org-in-list-p)
               (org-insert-heading))
              (t (org-insert-heading)))
        (evil-insert-state))

      (defun my--toggle-checkbox ()
        (interactive)
        (save-excursion
          (org-end-of-line)
          (cond ((org-in-item-p)
                 (if (search-backward-regexp "\\[[ +-]\\]" (line-beginning-position) t)
                     (delete-char 4)
                   (org-beginning-of-line)))
                (t (org-insert-heading)))
          (insert "[ ] ")))

      (bind 'insert org-mode-map [remap my.inflate-space-maybe] 'self-insert-command)

      (bind org-mode-map
            "RET" nil
            "C-j" nil
            "C-k" nil)

      (bind '(normal insert) evil-org-mode-map
            "M-l" 'org-metaright
            "M-h" 'org-metaleft
            "M-k" 'org-metaup
            "M-j" 'org-metadown
            "M-L" 'org-shiftmetaright
            "M-H" 'org-shiftmetaleft
            "M-K" 'org-shiftmetaup
            "M-J" 'org-shiftmetadown)

      (bind 'insert evil-org-mode-map
            "C-e"           'org-end-of-line
            "C-a"           'org-beginning-of-line)
      (bind '(insert normal) evil-org-mode-map
            "<s-left>"      'org-beginning-of-line
            "<s-right>"     'org-end-of-line
            "<s-up>"        'org-up-element
            "<s-down>"      'org-down-element)

      ;; Formatting shortcuts
      (defun my/org-surround (delim)
        (insert delim) (save-excursion (insert delim)))

      (bind 'insert evil-org-mode-map
            ;; Add new header line before this line
            (kbd "<S-s-return>") 'my--org-insert-item-before
            ;; Add new header line after this line
            (kbd "<s-return>") 'my--org-insert-item-after

            (kbd "s-b") (λ (my/org-surround "*"))     ; bold
            (kbd "s-u") (λ (my/org-surround "_"))     ; underline
            (kbd "s-i") (λ (my/org-surround "/"))     ; italics
            (kbd "s-`") (λ (my/org-surround "+")))    ; strikethrough
      (bind 'visual evil-org-mode-map
            (kbd "s-b") "s*"
            (kbd "s-u") "s_"
            (kbd "s-i") "s/"
            (kbd "s-`") "s+")

      (bind '(normal visual) evil-org-mode-map
            ", l" 'org-insert-link)

      (bind 'normal evil-org-mode-map
            ", +" 'org-align-all-tags
            ", /" 'org-sparse-tree
            ", ?" 'org-tags-view
            ", a" 'org-attach
            ", D" 'org-time-stamp-inactive
            ", T" 'org-show-todo-tree
            ", d" 'org-time-stamp
            ", r" 'org-refile
            ", s" 'org-schedule
            ", t" 'org-todo
            "g r" 'org-babel-execute-src-block-maybe
            "g h" 'outline-up-heading
            "g j" 'org-forward-heading-same-level
            "g k" 'org-backward-heading-same-level
            "g l" 'outline-next-visible-heading
            "g o"   'org-open-at-point
            "g O"   'org-attach-open
            "g C-o" 'org-attach-reveal
            "g i" (λ (if (> (length org-inline-image-overlays) 0)
                         (org-remove-inline-images)
                       (org-display-inline-images nil t (line-beginning-position) (line-end-position))))
            "g a" 'org-attach
            "g t" 'org-show-todo-tree
            "$" 'org-end-of-line
            "^" 'org-beginning-of-line
            "<" 'org-metaleft
            ">" 'org-metaright
            "-" 'org-cycle-list-bullet
            ", SPC" 'my--toggle-checkbox
            ", <return>" 'org-archive-subtree
            "<S-s-return>" 'my--org-insert-item-before
            "<s-return>" 'my--org-insert-item-after
            "RET" (λ (cond ((org-at-item-checkbox-p)
                            (org-toggle-checkbox))
                           ((org-entry-is-todo-p)
                            (org-todo 'done))))
            [tab] 'org-cycle))

    (evil-ex-define-cmd "o[rg]edit" 'org-edit-special)
    (evil-ex-define-cmd "o[rg]refile" 'org-refile)
    (evil-ex-define-cmd "o[rg]archive" 'org-archive-subtree)
    (evil-ex-define-cmd "o[rg]agenda" 'org-agenda)
    (evil-ex-define-cmd "o[rg]todo" 'org-show-todo-tree)
    (evil-ex-define-cmd "o[rg]link" 'org-link)
    (evil-ex-define-cmd "o[rg]align" 'org-align-all-tags)

    (evil-ex-define-cmd "o[rg]image" 'my:org-insert-image)

    (evil-define-command my:org-insert-image-url (&optional image-url)
      :repeat nil
      (interactive "<f><!>")
      (unless image-url
        (user-error "You must specify an image URL to insert"))
      (let ((dest (f-join org-directory "images/" (concat (format-time-string "%Y%m%d-") (f-filename filename)))))
        (shell-command (format "wget '%s' -O '%s'" image-url dest))
        (insert (format "<%s>" (f-relative dest (f-dirname (buffer-file-name)))))
        (indent-according-to-mode)))

    (evil-define-command my:org-insert-image (&optional filename bang)
      :repeat nil
      (interactive "<f><!>")
      (if bang
          (my:org-insert-image-url filename)
        (unless filename
          (user-error "You must specify a file to attach"))
        (unless (file-exists-p filename)
          (user-error "File %s does not exist" filename))
        (let ((dest (f-join org-directory "images/" (concat (format-time-string "%Y%m%d-") (f-filename filename)))))
          (when (f-exists? dest)
            (user-error "File %s already exists at destination!"))
          (copy-file filename dest)
          (insert (format "<file:%s>" (f-relative dest (f-dirname (buffer-file-name)))))
          (indent-according-to-mode))))))


(provide 'init-org)
;;; init-org.el ends here
