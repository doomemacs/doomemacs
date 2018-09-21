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



;;
;; Misc

;;;###autoload
(defun +evil/matchit-or-toggle-fold ()
  "Do what I mean. If on a fold-able element, toggle the fold with
`hs-toggle-hiding'. Otherwise, if on a delimiter, jump to the matching one with
`evilmi-jump-items'. If in a magit-status buffer, use `magit-section-toggle'."
  (interactive)
  (ignore-errors
    (call-interactively
     (cond ((derived-mode-p 'magit-mode)
            #'magit-section-toggle)
           ((+evil-fold-p)
            #'+evil:fold-toggle)
           (t
            #'evilmi-jump-items)))))
