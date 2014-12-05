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
(require 'org)

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Reset evil to ensure evil-org-mode's maps work
(add-hook! 'org-mode-hook (evil-mode nil) (evil-mode 1))

(shut-up (load-library "ox-opml"))

;; Remove occur highlights on ESC in normal mode
(defadvice evil-force-normal-state (before evil-esc-org-remove-highlights activate)
  (org-remove-occur-highlights))

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
        (sequence "DOING(s)" "PENDING(p)")
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


;; Keymaps
(bind 'insert org-mode-map [remap my.inflate-space-maybe] 'self-insert-command)
(bind org-mode-map (kbd "RET") nil
                     (kbd "C-j") nil
                     (kbd "C-k") nil)

(mapc (lambda (state)
        (evil-define-key state evil-org-mode-map
          (kbd "M-l") 'org-metaright
          (kbd "M-h") 'org-metaleft
          (kbd "M-k") 'org-metaup
          (kbd "M-j") 'org-metadown
          (kbd "M-L") 'org-shiftmetaright
          (kbd "M-H") 'org-shiftmetaleft
          (kbd "M-K") 'org-shiftmetaup
          (kbd "M-J") 'org-shiftmetadown
          (kbd "M-o") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-heading)
                             (org-metaright))))
          (kbd "M-t") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-todo-heading nil)
                             (org-metaright))))
          ))
      '(normal insert))

;; Formatting shortcuts
(bind 'insert evil-org-mode-map
      (kbd "s-b") (λ (my/org-surround "*"))     ; bold
      (kbd "s-u") (λ (my/org-surround "_"))     ; underline
      (kbd "s-i") (λ (my/org-surround "/"))     ; italics
      (kbd "s-`") (λ (my/org-surround "+"))     ; strikethrough

      (kbd "<S-s-return>") (λ (evil-move-beginning-of-line) (org-insert-heading))
      (kbd "<s-return>") (λ (org-insert-heading-after-current)))

(bind '(normal visual) evil-org-mode-map
       ",l" 'org-insert-link)

(bind 'visual evil-org-mode-map
      (kbd "s-b") "s*"          ; bold
      (kbd "s-i") "s/")         ; italics

(bind 'normal evil-org-mode-map
      ",d" 'org-time-stamp
      ",D" 'org-time-stamp-inactive
      ",s" 'org-schedule
      ",a" 'org-attach
      ",A" 'org-attach-open
      ",t" 'org-todo
      ",T" 'org-show-todo-tree
      ",/" 'org-sparse-tree
      ",?" 'org-tags-view
      ",+" 'org-align-all-tags
      ",r" 'org-refile
      "gh" 'outline-up-heading
      "gj" 'org-forward-heading-same-level
      "gk" 'org-backward-heading-same-level
      "gl" 'outline-next-visible-heading
      "go" 'org-open-at-point
      "ga" 'org-agenda
      "gt" 'org-show-todo-tree
      "H" 'org-beginning-of-line
      "L" 'org-end-of-line
      "$" 'org-end-of-line
      "^" 'org-beginning-of-line
      "<" 'org-metaleft
      ">" 'org-metaright
      "-" 'org-cycle-list-bullet
      (kbd ", SPC") 'org-archive-subtree
      (kbd "<S-s-return>") (λ (evil-move-beginning-of-line) (org-insert-heading) (evil-insert-state))
      (kbd "<s-return>") (λ (org-insert-heading-after-current) (evil-insert-state))
      (kbd "RET") (λ (if (org-entry-is-todo-p) (org-todo 'done)))
      (kbd "TAB") 'org-cycle)
