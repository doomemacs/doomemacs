;; custom-write.el

;; This library offers the following:
;;   + Write-mode: a mode that turns Emacs into an app for writing notes,
;;     papers, or fiction: it adds eye-candy to org-mode, switches to a light
;;     color theme and to a more readable font.
;;   + Bibtex integration

;; Write-mode settings
(defconst write-mode-theme 'doom-one)
(defconst write-mode-font (font-spec :family "Source Sans Pro" :size 14))

(defconst write-mode--last-mode-line mode-line-format)
(defconst write-mode--last-theme doom-current-theme)
(defconst write-mode--last-line-spacing line-spacing)

(after! spaceline
  (spaceline-compile
   'write
   '(((*macro-recording *anzu *iedit *evil-substitute *flycheck)
      :skip-alternate t
      :tight t)
     *buffer-path
     *buffer-modified)
   '((*selection-info :when active)
     *buffer-encoding-abbrev
     (global :when active)
     *buffer-position
     *pad)))

(defvar write-mode nil)
(defun doom/write-mode ()
  "A mode that turns Emacs into an app for writing notes, papers, or fiction: it
adds eye-candy to org-mode, switches to a light color theme and to a more
readable font."
  (interactive)
  (setq write-mode (not write-mode))
  (when write-mode-theme
    (doom/load-theme (if write-mode write-mode-theme write-mode--last-theme) t))
  (when write-mode-font
    (doom/load-font (if write-mode write-mode-font doom-default-font)))
  (mapc (lambda (b)
          (with-current-buffer b
            (setq line-spacing (if write-mode write-mode--last-line-spacing '2))
            (when (featurep 'spaceline)
              (let ((doom-hide-mode-line-format '("%e" (:eval (spaceline-ml-write)))))
                (doom-hide-mode-line-mode (if write-mode +1 -1))))))
        (doom/get-buffers-in-modes '(org-mode markdown-mode))))

(provide 'custom-write)
;;; custom-write.el ends here
