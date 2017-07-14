;;; feature/evil/autoload/evil-mc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +evil/mc-toggle-cursors ()
  "Toggle frozen state of evil-mc cursors."
  (interactive)
  (setq evil-mc-frozen (not (and (evil-mc-has-cursors-p)
                                 evil-mc-frozen)))
  (if evil-mc-frozen
      (message "evil-mc paused")
    (message "evil-mc resumed")))

;;;###autoload (autoload '+evil/mc-make-cursor-here "feature/evil/autoload/evil-mc" nil t)
(evil-define-command +evil/mc-make-cursor-here ()
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

;;;###autoload (autoload '+evil:mc "feature/evil/autoload/evil-mc" nil t)
(evil-define-command +evil:mc (beg end type pattern &optional bang)
  "Create mc cursors at each match of PATTERN within BEG and END, and leave the
cursor at the final match. If BANG, then treat PATTERN as literal."
  :move-point nil
  :evil-mc t
  (interactive "<R><//g><!>")
  (require 'evil-mc)
  (setq evil-mc-pattern (cons (evil-mc-make-pattern (if bang (regexp-quote pattern) pattern) nil)
                              (list beg end type)))
  (save-excursion
    (evil-with-restriction beg end
      (evil-mc-make-cursors-for-all)
      (evil-mc-print-cursors-info "Created")))
  (evil-exit-visual-state)
  (evil-mc-goto-cursor
   (if (= (evil-visual-direction) 1)
       (evil-mc-find-last-cursor)
     (evil-mc-find-first-cursor))
   nil))
