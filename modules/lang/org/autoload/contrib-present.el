;;; lang/org/autoload/contrib-present.el -*- lexical-binding: t; -*-
;;;###if (modulep! +present)

;;
;;; Helpers

(defun +org-present--cleanup-org-tree-slides-mode ()
  (unless (cl-loop for buf in (doom-buffers-in-mode 'org-mode)
                   if (buffer-local-value 'org-tree-slide-mode buf)
                   return t)
    (org-tree-slide-mode -1)
    (remove-hook 'kill-buffer-hook #'+org-present--cleanup-org-tree-slides-mode
                 'local)))


;;
;;; Hooks

;;;###autoload
(defun +org-present-hide-blocks-h ()
  "Hide org #+ constructs."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*\\(#\\+\\)\\(\\(?:BEGIN\\|END\\|ATTR\\)[^[:space:]]+\\).*" nil t)
      (org-fold-region (match-beginning 1)
                       (match-end 0)
                       org-tree-slide-mode
                       'block))))

;;;###autoload
(defun +org-present-hide-leading-stars-h ()
  "Hide leading stars in headings."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\)" nil t)
      (org-fold-region (match-beginning 1)
                       (match-end 1)
                       org-tree-slide-mode
                       'headline))))

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
(defvar +org-present--last-wconf nil)
;;;###autoload
(defun +org-present-prettify-slide-h ()
  "Set up the org window for presentation."
  (let ((arg (if org-tree-slide-mode +1 -1)))
    (if (not org-tree-slide-mode)
        (when +org-present--last-wconf
          (set-window-configuration +org-present--last-wconf))
      (setq +org-present--last-wconf (current-window-configuration))
      (doom/window-maximize-buffer))
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
           (when (bound-and-true-p flyspell-mode)
             (flyspell-mode -1))
           (add-hook 'kill-buffer-hook #'+org-present--cleanup-org-tree-slides-mode
                     nil 'local)
           (text-scale-set +org-present-text-scale)
           (ignore-errors (org-latex-preview '(4))))
          (t
           (text-scale-set 0)
           (pcase (type-of fringe-mode)
             ((or 'integer 'symbol) (set-window-fringes nil fringe-mode fringe-mode))
             ('cons (set-window-fringes nil (car fringe-mode) (cdr fringe-mode))))
           (org-clear-latex-preview)
           (org-remove-inline-images)
           (org-mode)))
    (redraw-display)))
