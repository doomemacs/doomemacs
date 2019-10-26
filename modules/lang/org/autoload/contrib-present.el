;;; lang/org/autoload/contrib-present.el -*- lexical-binding: t; -*-
;;;###if (featurep! +present)

;;
;;; Helpers

(defun +org-present--cleanup-org-tree-slides-mode ()
  (unless (cl-loop for buf in (doom-buffers-in-mode 'org-mode)
                   if (buffer-local-value 'org-tree-slide-mode buf)
                   return t)
    (org-tree-slide-mode -1)
    (remove-hook 'kill-buffer-hook #'+org-present--cleanup-org-tree-slides-mode)))

(defun +org-present--make-invisible (beg end)
  (let ((overlay (make-overlay beg end)))
    (push overlay +org-present--overlays)
    (overlay-put overlay 'invisible '+org-present)))


;;
;;; Hooks

;;;###autoload
(defun +org-present-add-overlays-h ()
  "TODO"
  (add-to-invisibility-spec '(+org-present))
  (save-excursion
    ;; hide org-mode options starting with #+
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*\\(#\\+\\)\\(\\(?:BEGIN\\|END\\|ATTR\\)[^[:space:]]+\\).*" nil t)
      (+org-present--make-invisible
       (match-beginning 1)
       (match-end 0)))
    ;; hide stars in headings
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\s-\\)" nil t)
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
(defun +org-present-init-org-tree-window-h ()
  "TODO"
  "Set up the org window for presentation."
  (doom/window-maximize-buffer)
  (let ((arg (if org-tree-slide-mode +1 -1)))
    (when (fboundp 'centered-window-mode)
      (let ((cwm-use-vertical-padding t)
            (cwm-frame-internal-border 110)
            (cwm-left-fringe-ratio -10)
            (cwm-centered-window-width 240))
        (centered-window-mode arg)))
    (window-divider-mode (* arg -1))
    (hide-mode-line-mode arg)
    (+org-pretty-mode arg)
    (cond (org-tree-slide-mode
           (org-indent-mode -1)
           (text-scale-set +org-present-text-scale)
           (ignore-errors (org-latex-preview '(4)))
           (set-face-attribute 'org-level-2 nil :height 1.4))
          (t
           (org-indent-mode +1)
           (text-scale-set 0)
           (org-clear-latex-preview)
           (set-face-attribute 'org-level-2 nil :height 1.0)
           (+org-present-remove-overlays-h)
           (org-remove-inline-images)))))


;;
;;; Commands

(defvar +org-present--overlays nil)
;;;###autoload
(defun +org-present/start ()
  "TODO"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "Not in an org buffer"))
  (call-interactively #'org-tree-slide-mode)
  (add-hook 'kill-buffer-hook #'+org-present--cleanup-org-tree-slides-mode))
