;;; feature/evil/autoload/folds.el -*- lexical-binding: t; -*-

;; `hideshow' is a decent code folding implementation, but it won't let you
;; create custom folds. `evil-vimish-fold' offers custom folds, but essentially
;; ignores any other type of folding (indent or custom markers, which
;; hs-minor-mode gives you).
;;
;; So this is my effort to combine them.

(defun +evil--vimish-fold-p ()
  (cl-some #'vimish-fold--vimish-overlay-p (overlays-at (point))))

(defun +evil--ensure-modes (&rest _)
  "Ensure hs-minor-mode is enabled."
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode +1)))

(require 'hideshow)
(advice-add #'hs-toggle-hiding :before #'+evil--ensure-modes)
(advice-add #'hs-hide-block    :before #'+evil--ensure-modes)
(advice-add #'hs-hide-level    :before #'+evil--ensure-modes)
(advice-add #'hs-show-all      :before #'+evil--ensure-modes)
(advice-add #'hs-hide-all      :before #'+evil--ensure-modes)


;; --- fold commands ----------------------

;;;###autoload
(defun +evil-fold-p ()
  (or (+evil--vimish-fold-p)
      (ignore-errors
        (+evil--ensure-modes)
        (hs-already-hidden-p))))

(defmacro +evil-from-eol (&rest body)
  "Perform action after moving to the end of the line."
  `(save-excursion
     (end-of-line)
     ,@body))

;;;###autoload (autoload '+evil:fold-toggle "feature/evil/autoload/folds" nil t)
(evil-define-command +evil:fold-toggle ()
  (interactive)
  (if (+evil--vimish-fold-p)
      (vimish-fold-toggle)
    (+evil-from-eol (hs-toggle-hiding))))

;;;###autoload (autoload '+evil:fold-open "feature/evil/autoload/folds" nil t)
(evil-define-command +evil:fold-open ()
  (interactive)
  (if (+evil--vimish-fold-p)
      (vimish-fold-unfold)
    (+evil-from-eol (hs-hide-block))))

;;;###autoload (autoload '+evil:fold-close "feature/evil/autoload/folds" nil t)
(evil-define-command +evil:fold-close ()
  (interactive)
  (if (+evil--vimish-fold-p)
      (vimish-fold-refold)
    (+evil-from-eol (hs-hide-block))))

;;;###autoload (autoload '+evil:fold-open-all "feature/evil/autoload/folds" nil t)
(evil-define-command +evil:fold-open-all (&optional level)
  "Open folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive "<c>")
  (vimish-fold-unfold-all)
  (if (integerp level)
      (hs-hide-level (1- level))
    (hs-show-all)))

;;;###autoload (autoload '+evil:fold-close-all "feature/evil/autoload/folds" nil t)
(evil-define-command +evil:fold-close-all (&optional level)
  "Close folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive "<c>")
  (vimish-fold-refold-all)
  (if (integerp level)
      (hs-hide-level (1- level))
    (hs-hide-all)))


;; --- misc -------------------------------

;;;###autoload
(defun +evil/matchit-or-toggle-fold ()
  "Do what I mean. If on a fold-able element, toggle the fold with
`hs-toggle-hiding'. Otherwise, if on a delimiter, jump to the matching one with
`evilmi-jump-items'. If in a magit-status buffer, use `magit-section-toggle'."
  (interactive)
  (ignore-errors
    (call-interactively
     (cond ((eq major-mode 'magit-status-mode)
            #'magit-section-toggle)
           ((+evil-fold-p)
            #'+evil:fold-toggle)
           (t
            #'evilmi-jump-items)))))
