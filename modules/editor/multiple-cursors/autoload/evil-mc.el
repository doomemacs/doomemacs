;;; editor/multiple-cursors/autoload/evil-mc.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor evil)

;;;###autoload
(defun +multiple-cursors/evil-mc-toggle-cursors ()
  "Toggle frozen state of evil-mc cursors."
  (interactive)
  (unless (evil-mc-has-cursors-p)
    (user-error "No cursors exist to be toggled"))
  (setq evil-mc-frozen (not (and (evil-mc-has-cursors-p)
                                 evil-mc-frozen)))
  (if evil-mc-frozen
      (message "evil-mc paused")
    (message "evil-mc resumed")))

;;;###autoload (autoload '+multiple-cursors/evil-mc-make-cursor-here "editor/multiple-cursors/autoload/evil-mc" nil t)
(evil-define-command +multiple-cursors/evil-mc-make-cursor-here ()
  "Create a cursor at point. If in visual block or line mode, then create
cursors on each line of the selection, on the column of the cursor. Otherwise
pauses cursors."
  :repeat nil
  :keep-visual nil
  :evil-mc t
  (interactive)
  (cond ((memq evil-this-type '(block line))
         (let ((col (evil-column))
               (line-at-pt (line-number-at-pos)))
           ;; Fix off-by-one error
           (when (= evil-visual-direction 1)
             (cl-decf col)
             (backward-char))
           (save-excursion
             (evil-apply-on-block
              (lambda (ibeg _)
                (unless (= line-at-pt (line-number-at-pos ibeg))
                  (goto-char ibeg)
                  (move-to-column col)
                  (when (= (current-column) col)
                    (evil-mc-make-cursor-here))))
              evil-visual-beginning
              (if (eq evil-this-type 'line) (1- evil-visual-end) evil-visual-end)
              nil)
             (evil-exit-visual-state))))
        (t
         (evil-mc-pause-cursors)
         ;; I assume I don't want the cursors to move yet
         (evil-mc-make-cursor-here))))

;;;###autoload (autoload '+multiple-cursors:evil-mc "editor/multiple-cursors/autoload/evil-mc" nil t)
(evil-define-command +multiple-cursors:evil-mc (beg end type pattern &optional flags bang)
  "Create mc cursors at each match of PATTERN within BEG and END.

This leaves the cursor at the final match. If BANG, then treat PATTERN as
literal. PATTERN is a delimited regexp (the same that :g or :s uses)."
  :move-point nil
  :evil-mc t
  (interactive "<R><//!><!>")
  (unless (and (stringp pattern)
               (not (string-empty-p pattern)))
    (user-error "A regexp pattern is required"))
  (require 'evil-mc)
  (setq evil-mc-pattern
        (cons (evil-ex-make-search-pattern
               (if bang (regexp-quote pattern) pattern))
              (list beg end type)))
  (evil-with-restriction beg end
    (let ((point (point)))
      (save-excursion
        (goto-char (point-min))
        (while (eq (evil-ex-find-next (evil-mc-get-pattern) 'forward t) t)
          (goto-char (1- (point)))
          (when (/= point (point))
            (evil-mc-run-cursors-before)
            (evil-mc-make-cursor-at-pos (point)))
          (goto-char
           (if (memq ?g flags)
               (line-beginning-position 2)
             (1+ (point))))))))
  (evil-exit-visual-state)
  (evil-mc-goto-cursor
   (if (= (evil-visual-direction) 1)
       (evil-mc-find-last-cursor)
     (evil-mc-find-first-cursor))
   nil)
  (evil-mc-undo-cursor-at-pos (point))
  (if (evil-mc-has-cursors-p)
      (evil-mc-print-cursors-info "Created")
    (evil-mc-message "No cursors were created")))
