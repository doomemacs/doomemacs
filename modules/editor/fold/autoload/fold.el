;;; editor/fold/autoload/fold.el -*- lexical-binding: t; -*-

;; `hideshow' is a decent code folding implementation, but it won't let you
;; create custom folds. `vimish-fold' offers custom folds, but essentially
;; ignores any other type of folding (indent or custom markers, which hideshow
;; and `outline-mode' give you). This is my effort to combine them.

;;
;;; Helpers

;;;###autoload
(defun +fold--ensure-hideshow-mode ()
  "Enable `hs-minor-mode' if not already enabled.

Return non-nil if successful in doing so."
  (if (not (bound-and-true-p hs-minor-mode))
      ;; `hs-grok-mode-type' applies this test; if it fails, it produces an
      ;; error indicating that `hs-minor-mode' is not supported here.
      (when (and (bound-and-true-p comment-start)
                 (bound-and-true-p comment-end))
        (hs-minor-mode +1))
    t))

(defun +fold--vimish-fold-p ()
  (and (featurep 'vimish-fold)
       (cl-some #'vimish-fold--vimish-overlay-p
                (overlays-at (point)))))

(defun +fold--outline-fold-p ()
  (and (or (bound-and-true-p outline-minor-mode)
           (derived-mode-p 'outline-mode))
       (outline-on-heading-p)))

(defun +fold--hideshow-fold-p ()
  (when (+fold--ensure-hideshow-mode)
    (save-excursion
      (ignore-errors
        (or (funcall hs-looking-at-block-start-p-func)
            (funcall hs-find-block-beginning-func)
            (unless (eolp)
              (end-of-line)
              (+fold--hideshow-fold-p)))))))

(defun +fold--treesit-fold-p ()
  (and (treesit-available-p)
       (treesit-parser-list)
       (require 'treesit-fold nil t)
       (treesit-fold-usable-mode-p)))

(defun +fold--invisible-points (count)
  (let (points)
    (save-excursion
      (catch 'abort
        (if (< count 0) (beginning-of-line))
        (while (re-search-forward hs-block-start-regexp nil t
                                  (if (> count 0) 1 -1))
          (unless (invisible-p (point))
            (end-of-line)
            (when (ignore-errors (hs-already-hidden-p))
              (push (point) points)
              (when (>= (length points) count)
                (throw 'abort nil))))
          (forward-line (if (> count 0) 1 -1)))))
    (nreverse points)))

(defmacro +fold-from-eol (&rest body)
  "Perform action after moving to the end of the line."
  `(save-excursion
     (end-of-line)
     ,@body))

(defun +fold--union ()
  "Get the combined region covered by all folds at point."
  ;; We are supporting four folding systems that weren't really designed to work
  ;; together. No doubt users will find novel, unanticipated ways to nest
  ;; different types of folds (especially easy to do with `outline-minor-mode').
  ;; So, we need code that can deal with any arbitrary overlap.
  (cl-reduce
   (lambda (&optional acc cur)
     (when (and acc cur)
       (cons (min (car acc) (car cur))
             (max (cdr acc) (cdr cur)))))
   (nconc
    (when (+fold--vimish-fold-p)
      (mapcar (lambda (ov)
                (cons (overlay-start ov) (overlay-end ov)))
              (seq-filter #'vimish-fold--vimish-overlay-p
                          (or (overlays-at (point)) '()))))
    (when (+fold--outline-fold-p)
      (save-excursion
        (let ((beg (progn (outline-back-to-heading) (point)))
              (end (progn (outline-end-of-subtree) (point))))
          (list (cons beg end)))))
    (when-let ((start (+fold--hideshow-fold-p)))
      ;; `start' could be start of the block, or 't' if that wasn't found.
      ;; In either case, we know the fold is on the same line.
      (let* ((start (or (and (numberp start) start)
                        (line-beginning-position)))
             (end (line-end-position))
             (ov (hs-overlay-at start)))
        (while (and (not ov) (< start end))
          (setq start (next-overlay-change start)
                ov (hs-overlay-at start)))
        (when ov
          (list (cons (overlay-start ov) (overlay-end ov))))))
    (when (+fold--treesit-fold-p)
      (when-let* ((node (treesit-fold--foldable-node-at-pos))
                  (beg (treesit-node-start node))
                  (end (treesit-node-end node)))
        (list (cons beg end)))))))

(defun +fold--open-rec-between (beg end)
  "Recursively open all folds betwen BEG and END."
  (when (featurep 'vimish-fold)
    ;; from `vimish-fold-unfold-all'
    (mapc #'vimish-fold--unfold (vimish-fold--folds-in beg end)))
  (and (+fold--outline-fold-p)
       (outline-show-subtree))
  (hs-life-goes-on
   ;; from `hs-show-all'
   (let ((hs-allow-nesting nil))
     (hs-discard-overlays beg end))
   (run-hooks 'hs-show-hook))
  (when (+fold--treesit-fold-p)
    (treesit-fold--ensure-ts
      ;; from `ts-fold-open-all'
      (thread-last (overlays-in beg end)
                   (seq-filter
                    (lambda (ov)
                      (eq (overlay-get ov 'invisible) 'treesit-fold)))
                   (mapc #'delete-overlay)))))

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
           (letf! ((#'outline-hide-subtree #'outline-hide-entry))
             (outline-toggle-children)))
          ((+fold--treesit-fold-p) (treesit-fold-toggle))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-toggle-hiding))))))

;;;###autoload
(defun +fold/open-rec ()
  "Recursively open the folded region at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (cl-destructuring-bind (beg . end) (+fold--union)
    (+fold--open-rec-between beg end)))

;;;###autoload
(defun +fold/open ()
  "Open the folded region at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-unfold))
          ((+fold--outline-fold-p)
           (outline-show-branches)
           (outline-show-entry))
          ((+fold--treesit-fold-p) (treesit-fold-open))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-show-block))))))

;;;###autoload
(defun +fold/close ()
  "Close the folded region at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-refold))
          ((+fold--outline-fold-p) (outline-hide-subtree))
          ((+fold--treesit-fold-p) (treesit-fold-close))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-hide-block))))))

;;;###autoload
(defun +fold/open-all (&optional level)
  "Open folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (cond ((+fold--treesit-fold-p)
         (treesit-fold-open-all))
        ((and (featurep 'vimish-fold) (+fold--vimish-fold-p))
         (vimish-fold-unfold-all))
        ((save-excursion
           (when (+fold--ensure-hideshow-mode)
             (hs-life-goes-on
              (if (integerp level)
                  (hs-hide-level-recursive level (point-min) (point-max))
                (hs-show-all))))
           (if (integerp level)
               (outline-hide-sublevels (max 1 level))
             (when (fboundp 'outline-show-all)
               (outline-show-all)))))))

;;;###autoload
(defun +fold/close-all (&optional level)
  "Close folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (save-excursion
    (if (+fold--treesit-fold-p)
        (treesit-fold-close-all)
      (when (featurep 'vimish-fold)
        (vimish-fold-refold-all))
      (when (+fold--ensure-hideshow-mode)
        (hs-life-goes-on
         (if (integerp level)
             (hs-hide-level-recursive level (point-min) (point-max))
           (hs-hide-all))))
      (if (integerp level)
          (outline--show-headings-up-to-level level)
        (when (fboundp 'outline-hide-sublevels)
          (outline-show-only-headings))))))

;;;###autoload
(defun +fold/next (count)
  "Jump to the next COUNT-th folded region."
  (interactive "p")
  (cl-loop with orig-pt = (point)
           for fn in
           (list (lambda ()
                   (when (and (bound-and-true-p hs-block-start-regexp)
                              (bound-and-true-p hs-minor-mode))
                     (+fold--invisible-points count)))
                 (lambda ()
                   (when (featurep 'vimish-fold)
                     (let (pts)
                       (dotimes (_ count)
                         (if (> count 0)
                             (vimish-fold-next-fold)
                           (vimish-fold-previous-fold))
                         (push (point) pts))
                       (nreverse pts))))
                 (lambda ()
                   (when (or (bound-and-true-p outline-minor-mode)
                             (derived-mode-p 'outline-mode))
                     (cl-destructuring-bind (count fn bound-fn)
                         (if (> count 0)
                             `(,count outline-next-visible-heading eobp)
                           `(,(- count) outline-previous-visible-heading bobp))
                       (let (pts)
                         (while (and (not (funcall bound-fn))
                                     (funcall fn count)
                                     (or (not (outline-invisible-p (pos-eol)))
                                         (prog1 (>= (cl-decf count) 0)
                                           (push (point) pts)))))
                         (nreverse pts)))))
                 (lambda ()
                   ;; `treesit-fold' does not define movement functions so we
                   ;; need to do it ourselves.
                   (when (+fold--treesit-fold-p)
                     (mapcar
                      #'overlay-start
                      (cl-delete-if (lambda (ov)
                                      (or (not (eq (overlay-get ov 'creator) 'treesit-fold))
                                          (if (> count 0)
                                              (<= (1- (overlay-start ov)) orig-pt)
                                            (>= (overlay-end ov) orig-pt))))
                                    (if (> count 0)
                                        (overlays-in (point) (point-max))
                                      (overlays-in (point-min) (point))))))))
           if (save-excursion (funcall fn))
           nconc it into points
           finally do
           (if-let* ((pt (nth (1- (abs count)) (sort points (if (> count 0) #'< #'>)))))
               (goto-char pt)
             (user-error "No more folds %s point" (if (> count 0) "after" "before")))))

;;;###autoload
(defun +fold/previous (count)
  "Jump to the previous COUNT-th folded region."
  (interactive "p")
  (+fold/next (- count)))
