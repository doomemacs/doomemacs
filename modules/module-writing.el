;;; module-writing.el

;; From <https://github.com/joostkremers/visual-fill-column/pull/6>
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
    (apply fn window args)))

;;;
(setq-default visual-fill-column-center-text nil)
(defvar writing-mode--last-mode-line mode-line-format)
(defvar writing-mode--last-line-spacing line-spacing)
(define-minor-mode writing-mode "Mode for writing research papers or fiction."
  :lighter "swrite"
  :keymap (make-sparse-keymap)
  (let* ((mode-p writing-mode)
         (on-off (if mode-p +1 -1)))
    (visual-fill-column-mode on-off)
    (visual-line-mode on-off)

    (if mode-p (setq writing-mode--last-line-spacing line-spacing))
    (setq line-spacing (if mode-p '4 writing-mode--last-line-spacing))

    (setq mode-line-format
          (if mode-p
              '("%e" (:eval (spaceline--prepare
                             '("[W]" narf-anzu narf-iedit narf-evil-substitute
                               (narf-buffer-path remote-host)
                               narf-buffer-modified)
                             '((selection-info :face highlight-face :skip-alternate t)
                               narf-hud
                               ))))
            writing-mode--last-mode-line))

    (when IS-MAC
      (setq ;; sane trackpad/mouse scroll settings
       mac-mouse-wheel-smooth-scroll mode-p
       mouse-wheel-progressive-speed mode-p))))

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

(defvar biblio-directory (concat narf-dropbox-dir "docs/biblio/") "docstring")
(use-package reftex
  :config
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t
        reftex-ref-style-default-list '("Cleveref" "Hyperref" "Fancyref")
        reftex-default-bibliography
        `(,(expand-file-name "phys.bib" biblio-directory))))


;;; Bibtex

;; NOTE: http://bibdesk.sourceforge.net/

(use-package helm-bibtex
  :defer t
  :config
  (setq helm-bibtex-bibliography
        `(,(expand-file-name "phys.bib" biblio-directory))

        helm-bibtex-library-path
        `(,(expand-file-name "phys-pdf" biblio-directory))

        helm-bibtex-notes-path (expand-file-name "notes" biblio-directory)
        helm-bibtex-notes-extension ".org"

        helm-bibtex-pdf-open-function
        (lambda (fpath) (async-start-process "open-pdf" "/usr/bin/open" nil fpath))))


(provide 'module-writing)
;;; module-writing.el ends here
