;;; emacs/hideshow/autoload.el -*- lexical-binding: t; -*-

(defface +hideshow-folded-face
  `((t (:inherit font-lock-comment-face :weight light)))
  "Face to hightlight `hideshow' overlays."
  :group 'doom-themes)

;;;###autoload
(defun +hideshow*ensure-mode (&rest _)
  "Ensure hs-minor-mode is enabled."
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode +1)))

;;;###autoload
(defun +hideshow-haml-forward-sexp (arg)
  (haml-forward-sexp arg)
  (move-beginning-of-line 1))

;;;###autoload
(defun +hideshow-forward-block-by-indent (_arg)
  (let ((start (current-indentation)))
    (forward-line)
    (unless (= start (current-indentation))
      (let ((range (+hideshow-indent-range)))
        (goto-char (cadr range))
        (end-of-line)))))

;;;###autoload
(defun +hideshow-set-up-overlay (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (when (featurep 'vimish-fold)
      (overlay-put
       ov 'before-string
       (propertize "…" 'display
                   (list vimish-fold-indication-mode
                         'empty-line
                         'vimish-fold-fringe))))
    (overlay-put
     ov 'display (propertize "  [...]  " 'face '+hideshow-folded-face))))


;;
;; Indentation detection

(defun +hideshow--empty-line-p ()
  (string= "" (string-trim (thing-at-point 'line))))

(defun +hideshow--geq-or-empty-p ()
  (or (+hideshow--empty-line-p) (>= (current-indentation) base)))

(defun +hideshow--g-or-empty-p ()
  (or (+hideshow--empty-line-p) (> (current-indentation) base)))

(defun +hideshow--seek (start direction before skip predicate)
  "Seeks forward (if direction is 1) or backward (if direction is -1) from start, until predicate
fails. If before is nil, it will return the first line where predicate fails, otherwise it returns
the last line where predicate holds."
  (save-excursion
    (goto-char start)
    (goto-char (point-at-bol))
    (let ((bnd (if (> 0 direction)
                   (point-min)
                 (point-max)))
          (pt (point)))
      (when skip (forward-line direction))
      (cl-loop while (and (/= (point) bnd) (funcall predicate))
               do (progn
                    (when before (setq pt (point-at-bol)))
                    (forward-line direction)
                    (unless before (setq pt (point-at-bol)))))
      pt)))

(defun +hideshow-indent-range (&optional point)
  "Return the point at the begin and end of the text block with the same (or
greater) indentation. If `point' is supplied and non-nil it will return the
begin and end of the block surrounding point."
  (save-excursion
    (when point
      (goto-char point))
    (let ((base (current-indentation))
          (begin (point))
          (end (point)))
      (setq begin (+hideshow--seek begin -1 t nil #'+hideshow--geq-or-empty-p)
            begin (+hideshow--seek begin 1 nil nil #'+hideshow--g-or-empty-p)
            end   (+hideshow--seek end 1 t nil #'+hideshow--geq-or-empty-p)
            end   (+hideshow--seek end -1 nil nil #'+hideshow--empty-line-p))
      (list begin end base))))
