;; Defuns
(defun my/org-insert-list-item ()
  "Force insertion of org item"
  (interactive)
  (if (not (org-in-item-p))
      (insert "\n- ")
    (org-insert-item)))

(defun my/org-eol-call (fun)
  "Go to end of line and call provided function"
  (end-of-line)
  (funcall fun)
  (evil-append nil))

(defun my/org-surround (delim)
  (insert delim) (save-excursion (insert delim)))

(defun set-buffer-file-format-to-opml ()
  (when (string-match "\.opml$" (buffer-file-name))
    (setq buffer-file-format '(opml))))

(defun opml-encode (begin end buffer)
  "Export Org mode buffer to OPML."
  (let ((org-export-show-temporary-export-buffer nil)
        (name "*OPML Export Buffer*"))
    (org-export-to-buffer 'opml name)
    (erase-buffer)
    (insert-buffer-substring (get-buffer name))
    (point-max)))

;;
(use-package org :ensure t
  :mode (("\\.org\\'" . org-mode)
         ("\\.opml\\'" . org-mode))
  :config
  (load-library "ox-opml")
  :init
  (progn
    (setq org-export-backends '(ascii html latex md opml))
    (add-hook 'find-file-hooks 'set-buffer-file-format-to-opml)
    (add-to-list 'auto-mode-alist '("\\.opml$" . org-mode))
    (add-to-list 'format-alist '(opml "Outline Processor Markup Language"
                                      "<[?]xml version=\"1.0\"[^>]*[?]>[\n]?.*[\n]?.*[\n]?<opml version=\"[1|2].0\">"
                                      "~/.emacs.d/elisp/org-opml/opml2org.py" opml-encode t))

    (setq org-directory "~/Dropbox/notes"
          org-default-notes-file "~/Dropbox/notes/notes.org"
          org-mobile-inbox-for-pull "~/Dropbox/notes/notes.org"
          org-mobile-directory "~/Dropbox/Apps/MobileOrg"
          org-agenda-files '("~/Dropbox/notes")
          org-src-tab-acts-natively t)

    (setq org-completion-use-ido t
          org-hide-leading-stars t
          org-todo-keywords
          '((sequence "TODO(t)" "|" "DONE(d)")
            (sequence "STARTED(s)" "VERIFY(v)" "WAITING(w)")
            (sequence "|" "CANCELLED(c)")))

    (org-babel-do-load-languages 'org-babel-load-languages
          '((python . t)
            (ruby . t)
            (sh . t)
            (matlab . t)
            (latex . t)))

    (setq org-tag-alist '(("@work" . ?b)
                          ("@home" . ?h)
                          ("@writing" . ?w)
                          ("@errands" . ?e)
                          ("@drawing" . ?d)
                          ("@coding" . ?c)
                          ("@phone" . ?p)
                          ("@reading" . ?r)
                          ("projects" . ?q)
                          ("easy" . ?0)
                          ("hard" . ?1)))

    (setq org-capture-templates
          '(("t" "TODO" entry (file+headline "~/Dropbox/notes/gtd.org" "Inbox") "* TODO %? %u")
            ("T" "TODO Someday" entry (file+headline "~/Dropbox/notes/gtd.org" "Someday") "* TODO %? %u :someday:")
            ("c" "Changelog" entry (file+headline (concat (projectile-project-root) "/CHANGELOG.org") "Unsorted") "** %u %? :unsorted:" :prepend t)
            ("i" "Invoice" entry (file+headline "~/Dropbox/notes/invoices.org" "Invoices") "** TODO %?" :prepend t)
            ("n" "Note" entry (file+datetree org-default-notes-file) "** %?")
            ("b" "Blog" entry (file+datetree "~/Dropbox/notes/blog.org") "** %?")
            ("j" "Journal" entry (file+datetree "~/Dropbox/notes/journal.org") "** %?%^g\n%?\nAdded: %U")
            ("a" "Trivia" entry (file "~/Dropbox/notes/trivia.org") "* %u %?" :prepend t)
            ("s" "Writing Scraps" entry (file "~/Dropbox/notes/writing.org") "* %u %?" :prepend t)
            ("v" "Vocab" entry (file "~/Dropbox/notes/vocab.org") "* %u %?" :prepend t)
            ("e" "Excerpt" entry (file "~/Dropbox/notes/excerpts.org") "* %u %?" :prepend t)))

    (setq org-agenda-custom-commands
          '(("x" agenda)
            ("y" agenda*)
            ("w" todo "WAITING")
            ("W" todo-tree "WAITING")
            ("to" todo)
            ("tp" tags "+Projects")
            ("tg" tags-todo "+gamedev")
            ("tw" tags-tree "+webdev")))

    (define-minor-mode evil-org-mode
      "Buffer local minor mode for evil-org"
      :init-value nil
      :lighter " EvilOrg"
      :keymap (make-sparse-keymap) ; defines evil-org-mode-map
      :group 'evil-org)

    ;; Keymaps
    ;; (emap org-agenda-mode-map
    ;;       ...)

    ;; Formatting shortcuts
    ;; Bold
    (vmap evil-org-mode-map (kbd "s-b") "s*")
    (imap evil-org-mode-map (kbd "s-b") (λ (my/org-surround "*")))
    ;; Italics
    (vmap evil-org-mode-map (kbd "s-i") "s/")
    (imap evil-org-mode-map (kbd "s-i") (λ (my/org-surround "/")))
    ;; Underline
    (imap evil-org-mode-map (kbd "s-u") (λ (my/org-surround "_")))
    ;; Strikethrough
    (imap evil-org-mode-map (kbd "s-`") (λ (my/org-surround "+")))

    (imap evil-org-mode-map
          (kbd "<s-return>") 'org-insert-heading-after-current)

    (nvmap evil-org-mode-map
          ",l" 'org-insert-link)

    ;; (vmap evil-org-mode-map
    ;;       ",l" 'org-insert-link)

    (nmap evil-org-mode-map
          ",d" 'org-time-stamp
          ",D" 'org-time-stamp-inactive
          ",s" 'org-schedule
          ",a" 'org-attach
          ",A" 'org-attach-open
          ",t" 'org-todo
          ",T" 'org-show-todo-tree
          ",/" 'org-match-sparse-tree
          ",?" 'org-tags-view
          ",+" 'org-align-all-tags
          ",r" 'org-refile
          "gh" 'outline-up-heading
          "gj" 'org-forward-heading-same-level
          "gk" 'org-backward-heading-same-level
          "gl" 'outline-next-visible-heading
          "go" 'org-open-at-point
          "ga" 'org-agenda
          "H" 'org-beginning-of-line
          "L" 'org-end-of-line
          "$" 'org-end-of-line
          "^" 'org-beginning-of-line
          "<" 'org-metaleft
          ">" 'org-metaright
          "-" 'org-cycle-list-bullet
          (kbd ", SPC") 'org-archive-subtree
          (kbd "<s-return>") (λ (org-insert-heading-after-current) (evil-insert-state))
          (kbd "RET") (λ (org-todo 'done))
          (kbd "TAB") 'org-cycle)

    ;; normal & insert state shortcuts.
    (mapc (lambda (state)
            (evil-define-key state evil-org-mode-map
              (kbd "M--") 'my/org-insert-list-item
              (kbd "M-l") 'org-metaright
              (kbd "M-h") 'org-metaleft
              (kbd "M-k") 'org-metaup
              (kbd "M-j") 'org-metadown
              (kbd "M-L") 'org-shiftmetaright
              (kbd "M-H") 'org-shiftmetaleft
              (kbd "M-K") 'org-shiftmetaup
              (kbd "M-J") 'org-shiftmetadown
              (kbd "<M-return>") '(lambda () (interactive)
                             (my/org-eol-call
                              '(lambda()
                                 (org-insert-heading)
                                 (org-metaright))))
              (kbd "M-t") '(lambda () (interactive)
                             (my/org-eol-call
                              '(lambda()
                                 (org-insert-todo-heading nil)
                                 (org-metaright))))
              ))
          '(normal insert))

    ;; Reset evil to ensure certain evil keybindings are prioritized
    (add-hook 'org-mode-hook (lambda() (evil-mode nil) (evil-mode 1)))

    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'my/enable-hard-wrap))
)

;;
(provide 'mod-org)
