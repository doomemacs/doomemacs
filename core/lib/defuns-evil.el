;;; defuns-evil.el
;; for ../core-evil.el

;;;###autoload (autoload 'narf:evil-open-folds "defuns-evil" nil t)
(evil-define-command narf/evil-open-folds (count)
  "Instead of `evil-open-folds'. Accepts COUNT for dictating fold level."
  (interactive "P")
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode 1))
  (if count (hs-hide-level count) (evil-open-folds)))

;;;###autoload (autoload 'narf:evil-open-folds "defuns-evil" nil t)
(evil-define-command narf/evil-close-folds (count)
  "Instead of `evil-close-folds'. Accepts COUNT for dictating fold level."
  (interactive "P")
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode 1))
  (if count (hs-hide-level count) (evil-close-folds)))

;;;###autoload (autoload 'narf/multi-next-line "defuns-evil" nil t)
(evil-define-motion narf/multi-next-line (count)
  "Move down 6 lines"
  :type line (evil-line-move 6))

;;;###autoload (autoload 'narf/multi-previous-line "defuns-evil" nil t)
(evil-define-motion narf/multi-previous-line (count)
  "Move up 6 lines"
  :type line (evil-line-move -6))

;;;###autoload
(defun narf/evil-visual-line-state-p ()
  "Returns non-nil if in visual-line mode, nil otherwise."
  (and (evil-visual-state-p)
       (eq (evil-visual-type) 'line)))

;;;###autoload
(defun narf:iedit-restrict-to-region ()
  (interactive)
  (if (iedit-current-occurrence-string)
      (let ((current-prefix-arg '(4)))
        (iedit-done)
        (call-interactively 'iedit-mode)
        (save-excursion (iedit-restrict-region (region-beginning) (region-end)))
        (evil-previous-line))
    (call-interactively 'evil-ret)))

;;;###autoload
(defun narf*evil-exchange-off ()
  (when evil-exchange--overlays
    (evil-exchange-cancel)))

;;;###autoload
(defun narf/evil-surround-escaped ()
  "Escaped surround characters."
  (let* ((char (string (read-char "\\")))
         (pair (cond ((string-match-p "[]})[{(]" char)
                      (let ((-pair (cdr (assoc (string-to-char char) evil-surround-pairs-alist))))
                        `(,(car -pair) . ,(cdr -pair))))
                     (t
                      `(,char . ,char))))
         (format (if (sp-point-in-string) "\\\\%s" "\\%s")))
    (cons (format format (car pair))
          (format format (cdr pair)))))

;;;###autoload
(defun narf/evil-surround-latex ()
  "LaTeX commands"
  (let* ((command (read-string "\\")))
    (cons (format "\\%s{" command) "}")))

;;;###autoload (autoload 'narf/evil-macro-on-all-lines "defuns-evil" nil t)
(evil-define-operator narf/evil-macro-on-all-lines (beg end &optional arg)
  "Apply macro to each line. Courtesy of PythonNut/emacs-config"
  (evil-with-state
    (evil-normal-state)
    (goto-char end)
    (evil-visual-state)
    (goto-char beg)
    (evil-ex-normal (region-beginning) (region-end)
      (concat "@"
        (single-key-description
          (read-char "What macro?"))))))

;;;###autoload
(defun narf/evil-window-split ()
  (interactive)
  (call-interactively 'evil-window-split)
  (evil-window-down 1))

;;;###autoload
(defun narf/undo-window-change ()
  (interactive)
  (call-interactively (if (wg-current-workgroup t) 'wg-undo-wconfig-change 'winner-undo)))

;;;###autoload
(defun narf/redo-window-change ()
  (interactive)
  (call-interactively (if (wg-current-workgroup t) 'wg-redo-wconfig-change 'winner-redo)))

;;;###autoload
(defun narf/evil-window-vsplit ()
  (interactive)
  (call-interactively 'evil-window-vsplit)
  (evil-window-right 1))

(defun narf--evil-window-move (direction)
  "Move current window to the next window in DIRECTION. If there is no window,
then make an empty split and switch their buffers."
  (let* ((this-window (get-buffer-window))
         (this-buffer (current-buffer))
         (that-window (windmove-find-other-window direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (unless that-window
      (setq that-window
            (split-window this-window nil (cond ((eq direction 'up) 'above)
                                                ((eq direction 'down) 'below)
                                                (t direction))))
      (with-selected-window that-window
        (switch-to-buffer "*scratch*"))
      (setq that-buffer (window-buffer that-window)))
    (with-selected-window this-window
      (switch-to-buffer that-buffer))
    (with-selected-window that-window
      (switch-to-buffer this-buffer))
    (select-window that-window)))

;;;###autoload
(defun narf/evil-window-move-left ()
  "See `narf--evil-window-move'"
  (interactive)
  (narf--evil-window-move 'left))
;;;###autoload
(defun narf/evil-window-move-down ()
  "See `narf--evil-window-move'"
  (interactive)
  (narf--evil-window-move 'down))
;;;###autoload
(defun narf/evil-window-move-up ()
  "See `narf--evil-window-move'"
  (interactive)
  (narf--evil-window-move 'up))
;;;###autoload
(defun narf/evil-window-move-right ()
  "See `narf--evil-window-move'"
  (interactive)
  (narf--evil-window-move 'right))

;;;###autoload
(defmacro define-text-object! (key start-regex end-regex)
  (let ((inner-name (make-symbol "narf--inner-name"))
        (outer-name (make-symbol "narf--outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(provide 'defuns-evil)
;;; defuns-evil.el ends here
