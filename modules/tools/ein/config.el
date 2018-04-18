;;; tools/ein/config.el -*- lexical-binding: t; -*-

(defvar +ein-notebook-dir "~/"
  "Default directory from where Jupyter notebooks are to be opened.")

(def-setting! :ein-notebook-dir (dir)
  "Set the default directory from where to open Jupyter notebooks."
  `(setq +ein-notebook-dir ,dir))


(def-package! ein
  :commands (ein:notebooklist-open ein:notebooklist-login ein:jupyter-server-start)
  :init
  (set! :popup "\\*ein:*" :ignore)
  (set! :popup "\\*ein:notebooklist *" '((side . left)) '((size . 40) (select)))
  ;; Ace-link on notebook list buffers
  (add-hook! 'ein:notebooklist-mode-hook
    (map! :map ein:notebooklist-mode-map
          "o" #'+ein/ace-link-ein))
  ;; Ein uses request to store http cookies. Store them in the cache dir.
  (setq request-storage-directory (concat doom-cache-dir "/request"))
  ;; Auto complete with company
  (set! :company-backend '(ein:notebook-multilang-mode ein:notebook-python-mode ein:notebook-plain-mode)
    'ein:company-backend)
  :config
  ;; Manually load the autoloads of EIN. This takes time...
  (load "ein-loaddefs.el" nil t t)
  (setq
   ;; Slide images into rows so that we can navigate buffers with images more easily
   ein:slice-image t
   ein:jupyter-default-notebook-directory +ein-notebook-dir
   ein:jupyter-default-server-command "/usr/bin/jupyter"
   ein:jupyter-server-args '("--no-browser")
   ein:notebook-modes
   '(ein:notebook-multilang-mode ein:notebook-python-mode ein:notebook-plain-mode))
  ;; Avy is required for showing links in the notebook list with ace-link.
  (require 'avy))

