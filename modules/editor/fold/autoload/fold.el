;;; editor/fold/autoload/fold.el -*- lexical-binding: t; -*-

;; `hideshow' is a decent code folding implementation, but it won't let you
;; create custom folds. `vimish-fold' offers custom folds, but essentially
;; ignores any other type of folding (indent or custom markers, which hideshow
;; and `outline-mode' give you). This is my effort to combine them.

;;
;;; Helpers

(defun +fold--ensure-hideshow-mode ()
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode +1)))

(defun +fold--vimish-fold-p ()
  (and (featurep 'vimish-fold)
       (cl-some #'vimish-fold--vimish-overlay-p
                (overlays-at (point)))))

(defun +fold--outline-fold-p ()
  (and (or (bound-and-true-p outline-minor-mode)
           (derived-mode-p 'outline-mode))
       (outline-on-heading-p)))

(defun +fold--hideshow-fold-p ()
  (+fold--ensure-hideshow-mode)
  (save-excursion
    (ignore-errors
      (or (hs-looking-at-block-start-p)
          (hs-find-block-beginning)
          (unless (eolp)
            (end-of-line)
            (+fold--hideshow-fold-p))))))

;; NOTE: does this need more?
(defun +fold--ts-fold-p ()
  (and (bound-and-true-p tree-sitter-mode)
       (featurep 'ts-fold)))

(defun +fold--invisible-points (count)
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

(defmacro +fold-from-eol (&rest body)
  "Perform action after moving to the end of the line."
  `(save-excursion
     (end-of-line)
     ,@body))


;;
;;; Commands

;;;###autoload
(defun +fold/toggle ()
  "Toggle the fold at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-toggle))
          ((+fold--outline-fold-p)
           (cl-letf (((symbol-function #'outline-hide-subtree)
                      (symbol-function #'outline-hide-entry)))
             (outline-toggle-children)))
          ((+fold--ts-fold-p) (ts-fold-toggle))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-toggle-hiding))))))

;;;###autoload
(defun +fold/open ()
  "Open the folded region at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-unfold))
          ((+fold--outline-fold-p) (outline-show-subtree))
          ((+fold--ts-fold-p) (ts-fold-open))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-show-block))))))

;;;###autoload
(defun +fold/close ()
  "Close the folded region at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-refold))
          ((+fold--ts-fold-p) (ts-fold-close))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-hide-block)))
          ((+fold--outline-fold-p) (outline-hide-subtree)))))

;;;###autoload
(defun +fold/open-all (&optional level)
  "Open folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (cond ((+fold--ts-fold-p)
         (ts-fold-open-all))
        ((featurep 'vimish-fold)
         (vimish-fold-unfold-all))
        ((save-excursion
           (+fold--ensure-hideshow-mode)
           (if (integerp level)
               (progn
                 (outline-hide-sublevels (max 1 (1- level)))
                 (hs-life-goes-on
                  (hs-hide-level-recursive (1- level) (point-min) (point-max))))
             (hs-show-all)
             (when (fboundp 'outline-show-all)
               (outline-show-all)))))))

;;;###autoload
(defun +fold/close-all (&optional level)
  "Close folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (save-excursion
    (if (+fold--ts-fold-p)
        (ts-fold-close-all)
      (progn
        (when (featurep 'vimish-fold)
          (vimish-fold-refold-all))
        (+fold--ensure-hideshow-mode)
        (hs-life-goes-on
         (if (integerp level)
             (hs-hide-level-recursive (1- level) (point-min) (point-max))
           (hs-hide-all)))
        (when (fboundp 'outline-hide-sublevels)
          (outline-hide-sublevels (or level 1)))))))

;;;###autoload
(defun +fold/next (count)
  "Jump to the next vimish fold, outline heading or folded region."
  (interactive "p")
  (cl-loop with orig-pt = (point)
           for fn
           in (list (lambda ()
                      (when (bound-and-true-p hs-block-start-regexp)
                        (car (+fold--invisible-points count))))
                    (lambda ()
                      (when (featurep 'vimish-fold)
                        (if (> count 0)
                            (evil-vimish-fold/next-fold count)
                          (evil-vimish-fold/previous-fold (- count))))
                      (if (/= (point) orig-pt) (point)))
                    (lambda ()
                      ;; ts-fold does not define movement functions so we need to do it ourselves
                      (when (+fold--ts-fold-p)
                        (let* ((arg-list (if (> count 0) ;; depending on direction we need to change the ranges
                                             (list (point) (point-max))
                                           (list (point-min) (point))))
                               (comp-fun (if (> count 0) ;; also depending on direction we need to change how we sort the list
                                             #'<
                                           #'>))
                               (ovs (cl-remove-if-not
                                     (lambda (ov)
                                       (eq (overlay-get ov 'creator) 'ts-fold))
                                     ;; `overlays-in' does not provide a list that is sorted
                                     ;; (in the way we need it atleast) so we need to sort it based on direction
                                     (cl-sort (apply #'overlays-in arg-list) comp-fun :key #'overlay-start))))
                          (if (and ovs (<= (abs count) (length ovs)))
                              (goto-char (overlay-start (nth (- (abs count) 1) ovs))))))))
           if (save-excursion (funcall fn))
           collect it into points
           finally do
           (if-let* ((pt (car (sort points (if (> count 0) #'< #'>)))))
               (goto-char pt)
             (message "No more folds %s point" (if (> count 0) "after" "before"))
             (goto-char orig-pt))))

;;;###autoload
(defun +fold/previous (count)
  "Jump to the previous vimish fold, outline heading or folded region."
  (interactive "p")
  (+fold/next (- count)))
