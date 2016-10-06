;; custom-write.el --- FIXME

;; This library offers the following:
;;   + Write-mode: a mode that turns Emacs into an app for writing notes,
;;     papers, or fiction: it adds eye-candy to org-mode, switches to a light
;;     color theme and to a more readable font.
;;   + Bibtex integration

;; Write-mode settings
(defconst write-mode-theme 'doom-one)
(defconst write-mode-font (font-spec :family "Source Sans Pro" :size 14))

(defconst write-mode--last-mode-line mode-line-format)
(defconst write-mode--last-theme doom-ui-theme)
(defconst write-mode--last-line-spacing line-spacing)

(defun doom-write-mode-line (&optional id)
  `(:eval
    (let* ((active (eq (selected-window) mode-line-selected-window))
           (lhs (list (propertize " "
                                  'display
                                  (pl/percent-xpm doom-modeline-height 100 0 100 0 3
                                                  (face-attribute (if active 'doom-modeline-bar 'doom-modeline-inactive-bar) :background nil t)
                                                  nil))
                      (*flycheck)
                      (*macro-recording)
                      (*selection-info)
                      (*anzu)
                      (*evil-substitute)
                      (*iedit)
                      " "
                      ,(if (eq id 'scratch)
                           '(*buffer-pwd)
                         '(list (*buffer-path) (*buffer-name) " "))
                      (*buffer-state)))
           (rhs (list (*buffer-encoding-abbrev) "  "
                      (*vc) "  "
                      (*major-mode) "  "
                      (*buffer-position)))
           (middle (propertize
                    " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                                       ,(1+ (string-width (format-mode-line rhs)))))))))
      (list lhs middle rhs))))

(defvar write-mode nil)
(defun doom/write-mode ()
  "A mode that turns Emacs into an app for writing notes, papers, or fiction: it
adds eye-candy to org-mode, switches to a light color theme and to a more
readable font."
  (interactive)
  (setq write-mode (not write-mode))
  (when write-mode-theme
    (doom/load-theme (if write-mode write-mode-theme write-mode--last-theme)))
  (when write-mode-font
    (doom/load-font (if write-mode write-mode-font doom-ui-font)))
  (mapc (lambda (b)
          (with-current-buffer b
            (setq line-spacing (if write-mode write-mode--last-line-spacing '2))
            (when (featurep 'spaceline)
              (let ((doom-hide-mode-line-format '("%e" (:eval (spaceline-ml-write)))))
                (doom-hide-mode-line-mode (if write-mode +1 -1))))))
        (doom/get-buffers-in-modes '(org-mode markdown-mode))))

(provide 'custom-write)
;;; custom-write.el ends here
