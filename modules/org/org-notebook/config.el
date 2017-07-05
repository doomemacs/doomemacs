;;; org/org-notebook/config.el -*- lexical-binding: t; -*-

;; (add-hook 'org-load-hook '+org|init-notebook t)

;; While I program, write or plan, I want easy access to notes of various kinds,
;; such as major-mode/language specific notes, or project-specific notes. They
;; can be accessed via `+org-notebook/find-major-mode-notes' and
;; `+org-notebook/find-project-notes'.

(defvar +org-notebook-dir (concat +org-dir "notes/")
  "The directory where the notes are kept.")

(defvar +org-notebook-code-dir (concat +org-notebook-dir "code/")
  "The directory where programming notes and snippets are kept.")

(defvar +org-notebook-project-dir (concat +org-notebook-dir "projects/")
  "The directory where project notes are kept.")


(defvar +org-notebook-code-alist
  '((js2-mode . "javascript"))
  "An alist mapping certain modes (symbols) to their org notes directory name.
If a mode isn't here, it's guessed by stripping out the -mode suffix and
replacing '+' characters with 'p's.")


;; (defun +org|init-notebook ())
