;;; tools/ein/config.el -*- lexical-binding: t; -*-

(defvar +ein-notebook-dir "~/"
  "Default directory from where Jupyter notebooks are to be opened.")


;;
;; Packages

(after! ein
  (setq ein:notebook-modes
        '(ein:notebook-multilang-mode
          ein:notebook-python-mode
          ein:notebook-plain-mode)
        ;; Slice images into rows; easier to navigate around images
        ein:slice-image t)

  (set-popup-rules!
   '(("\\*ein: .*" :ignore t)
     ("\\*ein:tb .*" :side 'bottom :size 0.3 :quit t :ttl nil :select nil)
     ("\\*ein:notebooklist *" :side 'left :size 50 :select nil)))

  (when (featurep! :completion company)
    ;; Code completion with company
    (setq ein:completion-backend 'ein:use-company-backend)
    (set-company-backend! '(ein:notebook-multilang-mode
                            ein:notebook-python-mode
                            ein:notebook-plain-mode)
      'ein:company-backend))

  (after! ein-jupyter
    (setq ein:jupyter-server-args '("--no-browser"))
    (unless ein:jupyter-default-notebook-directory
      (setq ein:jupyter-default-notebook-directory "~/")))

  (defun +ein-buffer-p (buf)
    (string-match-p "^\\*ein: .*" (buffer-name buf)))
  (add-to-list 'doom-real-buffer-functions #'+ein-buffer-p nil #'eq)

  (map! :map ein:notebook-mode-map
        "M-s" #'ein:notebook-save-notebook-command
        :map ein:notebooklist-mode-map
        "o" #'+ein/ace-link-ein))
