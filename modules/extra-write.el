;; extra-write.el

;; This library offers the following:
;;   + TODO Write-mode: a mode that turns Emacs into an app for writing notes, papers, or
;;     fiction: it adds eye-candy to org-mode, switches to a light color theme and to a
;;     more readable font.
;;   + Bibtex integration

;; Write-mode settings
(defconst write-mode nil)
(defconst write-mode-theme 'doom-light)
(defconst write-mode-font (font-spec :family "Hack" :size 12))

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

;;
(defun write-mode|org-hook ()
  "A hook that runs in org-mode buffers when `write-mode' is active."
  (when write-mode
    (setq header-line-format '("%e" (:eval (spaceline-ml-write)))
          mode-line-format nil)))

(defun write-mode-toggle ()
  "Enable write-mode, this is not a [global] minor mode because it mixes some frame-local
functionality with buffer-local ones, which can be buggy in a minor-mode."
  (interactive)
  (let* ((mode-p write-mode)
         (on-off (if mode-p -1 +1)))
    (doom/load-theme (if mode-p write-mode--last-theme write-mode-theme) t)
    (doom/load-font (if mode-p doom-default-font write-mode-font))
    (if mode-p
        (remove-hook 'org-mode-hook 'write-mode|org-hook)
      (add-hook 'org-mode-hook 'write-mode|org-hook))
    (mapc (lambda (b)
            (with-current-buffer b
              (setq line-spacing (if mode-p write-mode--last-line-spacing '2))
              (when (and (eq major-mode 'org-mode)
                         (not mode-p))
                (write-mode|org-hook))
              (unless mode-p
                (setq mode-line-format write-mode--last-mode-line
                      header-line-format nil))))
          (doom/get-buffers-in-modes '(org-mode markdown-mode)))
    (setq write-mode (not mode-p))))

(when (> emacs-major-version 24)
  ;; From <https://github.com/joostkremers/visual-fill-column/pull/6>
  ;; Splitting windows while visual-fill-column makes Emacs go crazy. This prevents that
  ;; by simply disabled VFC before splitting.
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

(provide 'extra-write)
;;; extra-write.el ends here
