;;; editor/multiple-cursors/autoload/evil-mc.el -*- lexical-binding: t; -*-
;;;###if (modulep! :editor evil)

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

;;;###autoload (autoload '+multiple-cursors/evil-mc-toggle-cursor-here "editor/multiple-cursors/autoload/evil-mc" nil t)
(evil-define-command +multiple-cursors/evil-mc-toggle-cursor-here ()
  "Create a cursor at point. If in visual block or line mode, then create
cursors on each line of the selection, on the column of the cursor. Otherwise
pauses cursors."
  :repeat nil
  :keep-visual nil
  :evil-mc t
  (interactive)
  (cond ((and (evil-mc-has-cursors-p)
              (evil-normal-state-p)
              (let* ((pos (point))
                     (cursor (cl-find-if (lambda (cursor)
                                           (eq pos (evil-mc-get-cursor-start cursor)))
                                         evil-mc-cursor-list)))
                (when cursor
                  (evil-mc-delete-cursor cursor)
                  (setq evil-mc-cursor-list (delq cursor evil-mc-cursor-list))
                  t))))

        ((memq evil-this-type '(block line))
         (let ((col (evil-column))
               (line-at-pt (line-number-at-pos)))
           ;; Fix off-by-one error
           (when (= evil-visual-direction 1)
             (cl-decf col)
             (backward-char))
           (save-excursion
             (evil-apply-on-block
              (lambda (ibeg _)
                (unless (or (= line-at-pt (line-number-at-pos ibeg))
                            (invisible-p ibeg))
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

This leaves the cursor where the final cursor would be. If BANG, then treat
PATTERN as literal. PATTERN is a delimited regexp (the same that :g or :s uses).
FLAGS can be g and/or i; which mean the same thing they do in
`evil-ex-substitute'."
  :evil-mc t
  :keep-visual t
  (interactive "<R><//!><!>")
  (unless (and (stringp pattern)
               (not (string-empty-p pattern)))
    (user-error "A regexp pattern is required"))
  (require 'evil-mc)
  (let ((m (evil-ex-make-pattern
            (if bang (regexp-quote pattern) pattern)
            (cond ((memq ?i flags) 'insensitive)
                  ((memq ?I flags) 'sensitive)
                  ((not +multiple-cursors-evil-mc-ex-case)
                   evil-ex-search-case)
                  (t +multiple-cursors-evil-mc-ex-case))
            (or (and +multiple-cursors-evil-mc-ex-global
                     (not (memq ?g flags)))
                (and (not +multiple-cursors-evil-mc-ex-global)
                     (memq ?g flags))))))
    (evil-mc-run-cursors-before)
    (setq evil-mc-pattern (cons m (list beg end type)))
    (evil-with-restriction beg end
      (goto-char beg)
      (while (eq (evil-ex-find-next m 'forward t) t)
        (evil-mc-make-cursor-at-pos (1- (point)))
        (unless (evil-ex-pattern-whole-line m)
          (goto-char (line-beginning-position 2)))))
    (evil-mc-goto-cursor
     (if (= (evil-visual-direction) 1)
         (evil-mc-find-last-cursor)
       (evil-mc-find-first-cursor))
     nil)
    (evil-mc-undo-cursor-at-pos (1- (point)))
    (if (evil-mc-has-cursors-p)
        (evil-mc-print-cursors-info "Created")
      (evil-mc-message "No cursors were created"))))

;;;###autoload (autoload '+multiple-cursors/evil-mc-undo-cursor "editor/multiple-cursors/autoload/evil-mc" nil t)
(evil-define-command +multiple-cursors/evil-mc-undo-cursor ()
  "Undos last cursor, or all cursors in visual region."
  :repeat nil
  :evil-mc t
  (interactive)
  (if (evil-visual-state-p)
      (or (mapc (lambda (c)
                  (evil-mc-delete-cursor c)
                  (setq evil-mc-cursor-list (delq c evil-mc-cursor-list)))
                (cl-remove-if-not
                 (lambda (pos)
                   (and (>= pos evil-visual-beginning)
                        (<  pos evil-visual-end)))
                 evil-mc-cursor-list
                 :key #'evil-mc-get-cursor-start))
          (message "No cursors to undo in region"))
    (evil-mc-undo-last-added-cursor)))


;;;###autoload (autoload '+multiple-cursors-execute-default-operator-fn "editor/multiple-cursors/autoload/evil-mc" nil t)

(after! evil-mc
  (evil-mc-define-handler +multiple-cursors-execute-default-operator-fn ()
    :cursor-clear region
    (evil-mc-with-region-or-execute-macro region t
      (funcall (evil-mc-get-command-name) region-start region-end))))
