;; module-write.el

;; This library offers the following:
;;   + Writing-mode: visual-fill-column, larger fonts, fewer disctractions
;;   + Bibtex integration
;;   + TODO Pandoc integration
;;   + TODO A separate emacs configuration dedicated to org-mode

(defvar write-mode nil)
(defvar write-mode-theme narf-theme)
(defvar write-mode-font narf-default-font)
(defvar write-mode-dir nil)
(defvar write-mode-biblio-dir nil)

(defvar write-mode--last-mode-line mode-line-format)
(defvar write-mode--last-line-spacing line-spacing)

(setq-default visual-fill-column-center-text t)

(defun write-mode-toggle ()
  (interactive)
  (let* ((mode-p write-mode)
         (on-off (if mode-p -1 +1)))
    (disable-theme (if mode-p write-mode-theme narf-theme))
    (scroll-bar-mode on-off)
    (narf/load-theme (if mode-p narf-theme write-mode-theme))
    (narf/load-font (if mode-p narf-default-font write-mode-font))
    (when (featurep 'volatile-highlights)
      (volatile-highlights-mode (not on-off)))
    (when IS-MAC
      ;; sane trackpad/mouse scroll settings
      (setq mac-mouse-wheel-smooth-scroll (not mode-p)
            mouse-wheel-progressive-speed (not mode-p)))
    (mapc (lambda (b)
            (with-current-buffer b
              (setq line-spacing (if mode-p write-mode--last-line-spacing '2))
              (when (eq major-mode 'org-mode)
                (org-bullets-mode on-off))))
          (narf/get-visible-buffers (narf/get-buffers-in-modes '(org-mode markdown-mode))))
    (setq write-mode (not write-mode))))

(evil-define-command narf:set-columns (&optional bang columns)
  "Adjusts visual-fill-column-width on the fly."
  (interactive "<!><a>")
  (if (or (= (length columns) 0) bang)
      (progn
        (setq visual-fill-column-width nil)
        (when visual-fill-column-mode
          (visual-fill-column-mode -1)))
    (setq columns (string-to-number columns))
    (when (> columns 30)
      (setq visual-fill-column-width columns)))
    (if visual-fill-column-mode
        (visual-fill-column--adjust-window)
      (visual-fill-column-mode 1)))

(when (>= emacs-major-version 25)
  ;; From <https://github.com/joostkremers/visual-fill-column/pull/6>
  ;; Splitting windows while visual-fill-column makes Emacs go crazy. This prevents
  ;; that by simply disabled VFC before splitting.
  (after! visual-fill-column
    (advice-add 'split-window :around #'visual-fill-column--disable-on-split-window))
  (defun visual-fill-column--disable-on-split-window (fn window &rest args)
    "Undo the effects of `visual-fill-column-mode' for splitting window."
    (if (and (or (not window) (window-live-p window))
             (buffer-local-value 'visual-fill-column-mode
                                 (window-buffer (or window (selected-window)))))
        (let ((inhibit-redisplay t))
          (set-window-fringes (or window (selected-window)) nil)
          (set-window-margins (or window (selected-window)) 0 0)
          (unwind-protect (apply fn window args)
            (save-selected-window
              (when window (select-window window 'norecord))
              (visual-fill-column--adjust-window))))
      (apply fn window args))))

;;; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq bibtex-dialect 'biblatex)
(setq bibtex-align-at-equal-sign t)
(setq bibtex-text-indentation 20)
(add-hook! bibtex-mode
  (local-set-key (kbd "C-c \\") 'bibtex-fill-entry)
  (setq fill-column 140))
(add-hook! latex-mode 'turn-on-auto-fill)
(add-hook! LaTeX-mode 'turn-on-auto-fill)

(use-package reftex
  :config
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t
        reftex-ref-style-default-list '("Cleveref" "Hyperref" "Fancyref")
        reftex-default-bibliography
        `(,(expand-file-name "phys.bib" write-mode-biblio-dir))))

;;; Bibtex
;; NOTE: http://bibdesk.sourceforge.net/
(use-package helm-bibtex
  :defer t
  :config
  (setq helm-bibtex-bibliography
        `(,(expand-file-name "phys.bib" write-mode-biblio-dir))

        helm-bibtex-library-path
        `(,(expand-file-name "phys-pdf" write-mode-biblio-dir))

        helm-bibtex-notes-path (expand-file-name "notes" write-mode-biblio-dir)
        helm-bibtex-notes-extension ".org"

        helm-bibtex-pdf-open-function
        (lambda (fpath) (async-start-process "open-pdf" "/usr/bin/open" nil fpath))))

(provide 'module-write)
;;; module-write.el ends here
