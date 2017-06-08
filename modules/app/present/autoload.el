;;; app/present/autoload.el -*- lexical-binding: t; -*-

;; --- impatient-mode -------------------------------------------------------------

;;;###autoload
(defun +present/impatient-mode ()
  (interactive)
  (require 'simple-httpd)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (if impatient-mode
      (add-hook 'kill-buffer-hook '+present--cleanup-impatient-mode)
    (+present--cleanup-impatient-mode)))

(defun +present--cleanup-impatient-mode ()
  (unless (cl-loop for buf in (doom-buffer-list)
                   if (buffer-local-value 'impatient-mode buf)
                   return t)
    (httpd-stop)
    (remove-hook 'kill-buffer-hook '+present--cleanup-impatient-mode)))


;; --- org tree slides ------------------------------------------------------------

(defvar +present--overlays-list nil)

;;;###autoload
(defun +present/org-tree-slides ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "Not in an org buffer"))
  (call-interactively 'org-tree-slide-mode)
  (add-hook 'kill-buffer-hook '+present--cleanup-org-tree-slides-mode))

;;;###autoload
(defun +present|add-overlays ()
  (add-to-invisibility-spec '(+present))
  (save-excursion
    ;; hide org-mode options starting with #+
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*\\(#\\+\\)\\(\\(?:BEGIN\\|END\\|ATTR\\)[^[:space:]]+\\).*" nil t)
      (+present--make-invisible
       (match-beginning 1)
       (match-end 0)))
    ;; hide stars in headings
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\s-\\)" nil t)
      (+present--make-invisible (match-beginning 1) (match-end 1)))))

;;;###autoload
(defun +present|remove-overlays ()
  (mapc #'delete-overlay +present--overlays-list)
  (remove-from-invisibility-spec '(+present)))

;;;###autoload
(defun +present|detect-slide ()
  (outline-show-all)
  (if (member "title" (org-get-tags-at))
      (text-scale-set 10)
    (text-scale-set +present-scale)))

(defun +present--cleanup-org-tree-slides-mode ()
  (unless (cl-loop for buf in (doom-buffers-in-mode 'org-mode)
                   if (buffer-local-value 'org-tree-slide-mode buf)
                   return t)
    (org-tree-slide-mode -1)
    (remove-hook 'kill-buffer-hook #'+present--cleanup-org-tree-slides-mode)))

(defun +present--make-invisible (beg end)
  (let ((overlay (make-overlay beg end)))
    (push overlay +present--overlays-list)
    (overlay-put overlay 'invisible '+present)))




;; --- misc -----------------------------------------------------------------------

(defvar +present--original-font +doom-font)

;;;###autoload
(define-minor-mode +present/big-mode
  "A global mode that resizes the font, for streams, screen-sharing and
presentations."
  :init-value nil
  :lighter " BIG"
  :global t
  (if +present/big-mode
      (set-frame-font +present-big-font t t)
    (set-frame-font +present--original-font t t)))

;;;###autoload
(defun +present/resize-frame-for-stream ()
  "Resize the frame pixelwise, so that it fits directly into my livecoding.tv
streaming layout."
  (interactive)
  (set-frame-width (selected-frame) 1325 nil t)
  (set-frame-height (selected-frame) 1080 nil t))
