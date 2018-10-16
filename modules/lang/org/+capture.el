;;; lang/org/+capture.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org|init-capture)

;; Sets up some reasonable defaults, as well as two `org-capture' workflows that
;; I like:
;;
;; 1. The traditional way: invoking `org-capture' directly, via SPC X, or
;;    through the :cap ex command.
;; 2. Through a org-capture popup frame that is invoked from outside Emacs (the
;;    ~/.emacs.d/bin/org-capture script). This can be invoked from qutebrowser,
;;    vimperator, dmenu or a global keybinding.

(defvar +org-capture-todo-file "todo.org"
  "The path to your personal todo file.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-notes-file "notes.org"
  "The path to your personal notes file.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-changelog-file "changelog.org"
  "The filename to use for project changelog files.

It is used in Doom's default `org-capture-templates'.")

(defvar org-capture-templates
  '(("t" "Personal todo" entry
     (file+headline +org-capture-todo-file "Inbox")
     "* [ ] %?\n%i\n%a" :prepend t :kill-buffer t)
    ("n" "Personal notes" entry
     (file+headline +org-capture-notes-file "Inbox")
     "* %u %?\n%i\n%a" :prepend t :kill-buffer t)

    ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
    ;; {todo,notes,changelog}.org file is found in a parent directory.
    ("p" "Templates for projects")
    ("pt" "Project todo" entry  ; {project-root}/todo.org
     (file+headline +org-capture-project-todo-file "Inbox")
     "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
    ("pn" "Project notes" entry  ; {project-root}/notes.org
     (file+headline +org-capture-project-notes-file "Inbox")
     "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
    ("pc" "Project changelog" entry  ; {project-root}/changelog.org
     (file+headline +org-capture-project-notes-file "Unreleased")
     "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)))


(defvar org-default-notes-file nil)  ; defined in org.el


;;
(defun +org|init-capture ()
  (dolist (var '(+org-capture-todo-file
                 +org-capture-notes-file))
    (set var (expand-file-name (symbol-value var) org-directory)))
  (unless org-default-notes-file
    (setq org-default-notes-file +org-capture-notes-file))

  (add-hook 'org-capture-after-finalize-hook #'+org-capture|cleanup-frame)

  (defun +org*expand-variable-paths (file)
    "If a variable is used for a file path in `org-capture-template', it is used
as is, and expanded relative to `default-directory'. This changes it to be
relative to `org-directory', unless it is an absolute path."
    (if (and (symbolp file) (boundp file))
        (expand-file-name (symbol-value file) org-directory)
      file))
  (advice-add #'org-capture-expand-file :filter-args #'+org*expand-variable-paths)

  (defun +org*prevent-save-prompts-when-refiling (&rest _)
    "Fix #462: when refiling from org-capture, Emacs prompts to kill the
underlying, modified buffer. This fixes that."
    (when (bound-and-true-p org-capture-is-refiling)
      (org-save-all-org-buffers)))
  (advice-add 'org-refile :after #'+org*prevent-save-prompts-when-refiling)

  (defun +org|show-target-in-capture-header ()
    (setq header-line-format
          (format "%s%s%s"
                  (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
                              'face 'font-lock-string-face)
                  org-eldoc-breadcrumb-separator
                  header-line-format)))
  (add-hook 'org-capture-mode-hook #'+org|show-target-in-capture-header)

  (when (featurep! :feature evil)
    (add-hook 'org-capture-mode-hook #'evil-insert-state))

  (when (featurep! :ui doom-dashboard)
    (add-hook '+doom-dashboard-inhibit-functions #'+org-capture-frame-p)))
