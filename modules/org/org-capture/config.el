;;; org/org-capture/config.el -*- lexical-binding: t; -*-

;; Sets up two `org-capture' workflows that I like:
;;
;; 1. The traditional way: invoking `org-capture' directly (or through a
;;    command, like :org).
;;
;; 2. Through a org-capture popup frame that is invoked from outside Emacs (the
;;    script is in ~/.emacs.d/bin). This lets me open an org-capture box
;;    anywhere I can call org-capture (whether or not Emacs is open/running),
;;    like, say, from qutebrowser, vimperator, dmenu or a global keybinding.

(setq org-default-notes-file (concat +org-dir "notes.org")
      org-capture-templates
      '(("t" "Todo" entry
         (file+headline (expand-file-name "todo.org" +org-dir) "Inbox")
         "* [ ] %?\n%i" :prepend t :kill-buffer t)

        ("n" "Notes" entry
         (file+headline org-default-notes-file "Inbox")
         "* %u %?\n%i" :prepend t :kill-buffer t)))

(add-hook 'org-capture-after-finalize-hook #'+org-capture|cleanup-frame)
(when (featurep! :feature evil)
  (add-hook 'org-capture-mode-hook #'evil-insert-state))

(when (featurep! :ui doom-dashboard)
  (add-hook '+doom-dashboard-inhibit-functions #'+org-capture-frame-p))
