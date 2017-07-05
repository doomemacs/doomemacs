;;; org/org-present/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +doom-present*org-tree-slide-narrow-exclude-header (orig-fn &rest args)
  "TODO"
  (cl-letf (((symbol-function 'org-narrow-to-subtree)
             (lambda () (save-excursion
                     (save-match-data
                       (org-with-limited-levels
                        (narrow-to-region
                         (progn (org-back-to-heading t)
                                (forward-line 1)
                                (point))
                         (progn (org-end-of-subtree t t)
                                (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
                                (point)))))))))
    (apply orig-fn args)))

;;;###autoload
(defun +org-present|org-tree-prepare-window ()
  "TODO"
  (doom/window-zoom)
  (let ((arg (if org-tree-slide-mode +1 -1)))
    (when (fboundp 'centered-window-mode)
      (centered-window-mode arg))
    (window-divider-mode (* arg -1))
    (doom-hide-modeline-mode arg)
    (+org-pretty-mode arg)
    (cond (org-tree-slide-mode
           (org-indent-mode -1)
           (text-scale-set +org-present-text-scale)
           (ignore-errors (org-toggle-latex-fragment '(4)))
           (set-face-attribute 'org-level-2 nil :height 1.4))
          (t
           (org-indent-mode +1)
           (text-scale-set 0)
           (org-remove-latex-fragment-image-overlays)
           (set-face-attribute 'org-level-2 nil :height 1.0)
           (+org-present|remove-overlays)
           (org-remove-inline-images)))))

(defvar +org-present--overlays nil)
;;;###autoload
(defun +org-present/org-tree-slides ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "Not in an org buffer"))
  (call-interactively 'org-tree-slide-mode)
  (add-hook 'kill-buffer-hook '+org-present--cleanup-org-tree-slides-mode))

;;;###autoload
(defun +org-present|add-overlays ()
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
(defun +org-present|remove-overlays ()
  (mapc #'delete-overlay +org-present--overlays)
  (remove-from-invisibility-spec '(+org-present)))

;;;###autoload
(defun +org-present|detect-slide ()
  (outline-show-all)
  (if (member "title" (org-get-tags-at))
      (text-scale-set 10)
    (text-scale-set +org-present-text-scale)))

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
