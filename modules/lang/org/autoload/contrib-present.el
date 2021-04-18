;;; lang/org/autoload/contrib-present.el -*- lexical-binding: t; -*-
;;;###if (featurep! +present)

(defvar +org-present--overlays nil)


;;
;;; Helpers

(defun +org-present--cleanup-org-tree-slides-mode ()
  (unless (cl-loop for buf in (doom-buffers-in-mode 'org-mode)
                   if (buffer-local-value 'org-tree-slide-mode buf)
                   return t)
    (org-tree-slide-mode -1)
    (remove-hook 'kill-buffer-hook #'+org-present--cleanup-org-tree-slides-mode
                 'local)))

(defun +org-present--make-invisible (beg end)
  (unless (assq '+org-present buffer-invisibility-spec)
    (add-to-invisibility-spec '(+org-present)))
  (let ((overlay (make-overlay beg (1+ end))))
    (push overlay +org-present--overlays)
    (overlay-put overlay 'invisible '+org-present)))


;;
;;; Hooks

;;;###autoload
(defun +org-present-hide-blocks-h ()
  "Hide org #+ constructs."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*\\(#\\+\\)\\(\\(?:BEGIN\\|END\\|ATTR\\)[^[:space:]]+\\).*" nil t)
      (+org-present--make-invisible
       (match-beginning 1)
       (match-end 0)))))

;;;###autoload
(defun +org-present-hide-leading-stars-h ()
  "Hide leading stars in headings."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\)" nil t)
      (+org-present--make-invisible (match-beginning 1) (match-end 1)))))

;;;###autoload
(defun +org-present-remove-overlays-h ()
  "TODO"
  (mapc #'delete-overlay +org-present--overlays)
  (remove-from-invisibility-spec '(+org-present)))

;;;###autoload
(defun +org-present-detect-slide-h ()
  "TODO"
  (outline-show-all)
  (if (member "title" (org-get-tags))
      (text-scale-set 10)
    (text-scale-set +org-present-text-scale)))

(defvar cwm-use-vertical-padding)
(defvar cwm-frame-internal-border)
(defvar cwm-left-fringe-ratio)
(defvar cwm-centered-window-width)
;;;###autoload
(defun +org-present-prettify-slide-h ()
  "TODO"
  "Set up the org window for presentation."
  (doom/window-maximize-buffer)
  (let ((arg (if org-tree-slide-mode +1 -1)))
    (when (fboundp 'centered-window-mode)
      (setq-local cwm-use-vertical-padding t)
      (setq-local cwm-frame-internal-border 100)
      (setq-local cwm-left-fringe-ratio -10)
      (setq-local cwm-centered-window-width 300)
      (centered-window-mode arg))
    (hide-mode-line-mode arg)
    (+org-pretty-mode arg)
    (cond (org-tree-slide-mode
           (set-window-fringes nil 0 0)
           (when (bound-and-true-p solaire-mode)
             (solaire-mode -1)
             (fringe-mode 0))
           (when (bound-and-true-p flyspell-mode)
             (flyspell-mode -1))
           (add-hook 'kill-buffer-hook #'+org-present--cleanup-org-tree-slides-mode
                     nil 'local)
           (text-scale-set +org-present-text-scale)
           (ignore-errors (org-latex-preview '(4))))
          (t
           (text-scale-set 0)
           (set-window-fringes nil fringe-mode fringe-mode)
           (org-clear-latex-preview)
           (+org-present-remove-overlays-h)
           (org-remove-inline-images)
           (org-mode)))
    (redraw-display)))
