;;; feature/evil/autoload/folds.el -*- lexical-binding: t; -*-

(require 'hideshow)

;; `hideshow' is a decent code folding implementation, but it won't let you
;; create custom folds. `vimish-fold' offers custom folds, but essentially
;; ignores any other type of folding (indent or custom markers, which
;; hs-minor-mode and `outline-mode' give you).
;;
;; So this is my effort to combine them.

(defun +evil--vimish-fold-p ()
  (and (featurep 'vimish-fold)
       (cl-some #'vimish-fold--vimish-overlay-p
                (overlays-at (point)))))

(defun +evil--outline-fold-p ()
  (and (or (bound-and-true-p outline-minor-mode)
           (derived-mode-p 'outline-mode))
       (outline-on-heading-p)))

(defun +evil--hideshow-fold-p ()
  (hs-minor-mode +1)
  (save-excursion
    (ignore-errors
      (or (hs-looking-at-block-start-p)
          (hs-find-block-beginning)))))


;;
;; Code folding

(defmacro +evil-from-eol (&rest body)
  "Perform action after moving to the end of the line."
  `(save-excursion
     (end-of-line)
     ,@body))

;;;###autoload
(defun +evil/fold-toggle ()
  (interactive)
  (save-excursion
    (cond ((+evil--vimish-fold-p) (vimish-fold-toggle))
          ((+evil--hideshow-fold-p) (+evil-from-eol (hs-toggle-hiding)))
          ((+evil--outline-fold-p) (outline-toggle-children)))))

;;;###autoload
(defun +evil/fold-open ()
  (interactive)
  (save-excursion
    (cond ((+evil--vimish-fold-p) (vimish-fold-unfold))
          ((+evil--hideshow-fold-p) (+evil-from-eol (hs-show-block)))
          ((+evil--outline-fold-p)
           (outline-show-children)
           (outline-show-entry)))))

;;;###autoload
(defun +evil/fold-close ()
  (interactive)
  (save-excursion
    (cond ((+evil--vimish-fold-p) (vimish-fold-refold))
          ((+evil--hideshow-fold-p) (+evil-from-eol (hs-hide-block)))
          ((+evil--outline-fold-p) (outline-hide-subtree)))))

;;;###autoload
(defun +evil/fold-open-all (&optional level)
  "Open folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (when (featurep 'vimish-fold)
    (vimish-fold-unfold-all))
  (save-excursion
    (if (integerp level)
        (progn
          (outline-hide-sublevels (max 1 (1- level)))
          (hs-life-goes-on
           (hs-hide-level-recursive (1- level) (point-min) (point-max))))
      (hs-show-all)
      (when (fboundp 'outline-show-all)
        (outline-show-all)))))

;;;###autoload
(defun +evil/fold-close-all (&optional level)
  "Close folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (save-excursion
    (when (featurep 'vimish-fold)
      (vimish-fold-refold-all))
    (if (integerp level)
        (progn
          (when (fboundp 'outline-hide-sublevels)
            (outline-hide-sublevels (max 1 (1- level))))
          (hs-life-goes-on
           (hs-hide-level-recursive (1- level) (point-min) (point-max))))
      (when (fboundp 'outline-hide-sublevels)
        (outline-hide-sublevels 1))
      (hs-hide-all))))

(defun +evil--invisible-points (count)
  (let (points)
    (save-excursion
      (catch 'abort
        (if (< count 0) (beginning-of-line))
        (while (re-search-forward hs-block-start-regexp nil t
                                  (if (> count 0) 1 -1))
          (unless (invisible-p (point))
            (end-of-line)
            (when (hs-already-hidden-p)
              (push (point) points)
              (when (>= (length points) count)
                (throw 'abort nil))))
          (forward-line (if (> count 0) 1 -1)))))
    points))

;;;###autoload
(defun +evil/fold-next (count)
  "Jump to the next vimish fold, outline heading or folded region."
  (interactive "p")
  (cl-loop with orig-pt = (point)
           for fn
           in (list (lambda ()
                      (when hs-block-start-regexp
                        (car (+evil--invisible-points count))))
                    (lambda ()
                      (if (> count 0)
                          (evil-vimish-fold/next-fold count)
                        (evil-vimish-fold/previous-fold (- count)))
                      (if (/= (point) orig-pt) (point))))
           if (save-excursion (funcall fn))
           collect it into points
           finally do
           (if-let* ((pt (car (sort points (if (> count 0) #'< #'>)))))
               (goto-char pt)
             (message "No more folds %s point" (if (> count 0) "after" "before"))
             (goto-char orig-pt))))

;;;###autoload
(defun +evil/fold-previous (count)
  "Jump to the previous vimish fold, outline heading or folded region."
  (interactive "p")
  (+evil/fold-next (- count)))


;;
;; Misc

;;;###autoload
(defun +evil/matchit-or-toggle-fold ()
  "Do what I mean.

If in a magit-status buffer, use `magit-section-toggle'.
If on a folded element, unfold it.
Otherwise, jump to the matching delimiter with `evilmi-jump-items'."
  (interactive)
  (ignore-errors
    (call-interactively
     (cond ((derived-mode-p 'magit-mode)
            #'magit-section-toggle)
           ((+evil-from-eol (invisible-p (point)))
            #'+evil/fold-toggle)
           (#'evilmi-jump-items)))))
