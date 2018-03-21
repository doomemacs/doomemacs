;;; lang/org/+capture.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org|init-capture)

;; Sets up two `org-capture' workflows that I like:
;;
;; 1. The traditional way: invoking `org-capture' directly (or through a
;;    command, like :org).
;;
;; 2. Through a org-capture popup frame that is invoked from outside Emacs (the
;;    script is in ~/.emacs.d/bin). This lets me open an org-capture box
;;    anywhere I can call org-capture (whether or not Emacs is open/running),
;;    like, say, from qutebrowser, vimperator, dmenu or a global keybinding.

(defvar +org-default-todo-file "todo.org"
  "TODO")

(defvar +org-default-notes-file "notes.org"
  "TODO")

(defvar org-capture-templates
  '(("t" "Todo" entry
     (file+headline +org-default-todo-file "Inbox")
     "* [ ] %?\n%i" :prepend t :kill-buffer t)

    ("n" "Notes" entry
     (file+headline +org-default-notes-file "Inbox")
     "* %u %?\n%i" :prepend t :kill-buffer t)))


(defun +org|init-capture ()
  (defvaralias 'org-default-notes-file '+org-default-notes-file)

  (setq org-default-notes-file (expand-file-name +org-default-notes-file +org-dir)
        +org-default-todo-file (expand-file-name +org-default-todo-file  +org-dir))

  (add-hook 'org-capture-after-finalize-hook #'+org-capture|cleanup-frame)

  ;; fix #462: when refiling from org-capture, Emacs prompts to kill the
  ;; underlying, modified buffer. This fixes that.
  (defun +org-capture*refile (&rest _)
    (when org-capture-is-refiling
      (org-save-all-org-buffers)))
  (advice-add 'org-refile :after #'+org-capture*refile)

  (when (featurep! :feature evil)
    (add-hook 'org-capture-mode-hook #'evil-insert-state))

  (when (featurep! :ui doom-dashboard)
    (add-hook '+doom-dashboard-inhibit-functions #'+org-capture-frame-p)))
