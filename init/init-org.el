(provide 'init-org)

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
(use-package org)

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

;; Reset evil to ensure certain evil keybindings are prioritized
(add-hook 'org-mode-hook 'evil-org-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'enable-hard-wrap)
(add-hook! 'org-mode-hook (evil-mode nil) (evil-mode 1))

(shut-up (load-library "ox-opml"))

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
      '(("t" "TODO" entry (file+headline "~/Dropbox/notes/gtd.org" "Inbox") "* TODO %? %u\n%i")
        ("T" "TODO Someday" entry (file+headline "~/Dropbox/notes/gtd.org" "Someday") "* TODO %? %u :someday:\n%i")
        ("c" "Changelog" entry (file+headline (concat (projectile-project-root) "/CHANGELOG.org") "Unsorted") "** %u %? :unsorted:\n%i" :prepend t)
        ("i" "Invoice" entry (file+headline "~/Dropbox/notes/invoices.org" "Invoices") "** TODO %?\n%i" :prepend t)
        ("n" "Note" entry (file+datetree org-default-notes-file) "** %?\n%i")
        ("b" "Blog" entry (file+datetree "~/Dropbox/notes/blog.org") "** %i%?")
        ("j" "Journal" entry (file+datetree "~/Dropbox/notes/journal.org") "** %?%^g\nAdded: %U\n%i")
        ("a" "Trivia" entry (file "~/Dropbox/notes/trivia.org") "* %u %?\n%i" :prepend t)
        ("s" "Writing Scraps" entry (file "~/Dropbox/notes/writing.org") "* %u %?\n%i" :prepend t)
        ("v" "Vocab" entry (file "~/Dropbox/notes/vocab.org") "* %u %?\n%i" :prepend t)
        ("e" "Excerpt" entry (file "~/Dropbox/notes/excerpts.org") "* %u %?\n%i" :prepend t)))

(setq org-agenda-custom-commands
      '(("x" agenda)
        ("y" agenda*)
        ("w" todo "WAITING")
        ("W" todo-tree "WAITING")
        ("to" todo)
        ("tp" tags "+Projects")
        ("tg" tags-todo "+gamedev")
        ("tw" tags-tree "+webdev")))
