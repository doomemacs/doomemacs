;;; editor/fold/autoload/hideshow.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +fold-hideshow-haml-forward-sexp-fn (arg)
  (haml-forward-sexp arg)
  (move-beginning-of-line 1))

;;;###autoload
(defun +fold-hideshow-forward-block-by-indent-fn (_arg)
  (let ((start (current-indentation)))
    (forward-line)
    (unless (= start (current-indentation))
      (let ((range (+fold-hideshow-indent-range)))
        (goto-char (cadr range))
        (end-of-line)))))

;;;###autoload
(defun +fold-hideshow-set-up-overlay-fn (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (when (featurep 'vimish-fold)
      (overlay-put
       ov 'before-string
       (propertize "…" 'display
                   (list vimish-fold-indication-mode
                         'empty-line
                         'vimish-fold-fringe))))
    (overlay-put
     ov 'display (propertize +fold-ellipsis
                             'face '+fold-hideshow-folded-face))))


;;
;;; Indentation detection

(defun +fold--hideshow-empty-line-p (_)
  (string= "" (string-trim (thing-at-point 'line 'no-props))))

(defun +fold--hideshow-geq-or-empty-p (base-indent)
  (or (+fold--hideshow-empty-line-p base-indent)
      (>= (current-indentation) base-indent)))

(defun +fold--hideshow-g-or-empty-p (base-indent)
  (or (+fold--hideshow-empty-line-p base-indent)
      (> (current-indentation) base-indent)))

(defun +fold--hideshow-seek (start direction before skip predicate base-indent)
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
      (cl-loop while (and (/= (point) bnd) (funcall predicate base-indent))
               do (progn
                    (when before (setq pt (point-at-bol)))
                    (forward-line direction)
                    (unless before (setq pt (point-at-bol)))))
      pt)))

(defun +fold-hideshow-indent-range (&optional point)
  "Return the point at the begin and end of the text block with the same (or
greater) indentation. If `point' is supplied and non-nil it will return the
begin and end of the block surrounding point."
  (save-excursion
    (when point
      (goto-char point))
    (let ((base-indent (current-indentation))
          (begin (point))
          (end (point)))
      (setq begin (+fold--hideshow-seek begin -1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
            begin (+fold--hideshow-seek begin 1 nil nil #'+fold--hideshow-g-or-empty-p base-indent)
            end   (+fold--hideshow-seek end 1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
            end   (+fold--hideshow-seek end -1 nil nil #'+fold--hideshow-empty-line-p base-indent))
      (list begin end base-indent))))
