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
  :type line
  (let ((line-move-visual visual-line-mode))
    (evil-line-move (* 6 (or count 1)))))

;;;###autoload (autoload 'narf/multi-previous-line "defuns-evil" nil t)
(evil-define-motion narf/multi-previous-line (count)
  "Move up 6 lines"
  :type line
  (let ((line-move-visual visual-line-mode))
    (evil-line-move (- (* 6 (or count 1))))))

;;;###autoload
(defun narf/evil-visual-line-state-p ()
  "Returns non-nil if in visual-line mode, nil otherwise."
  (and (evil-visual-state-p)
       (eq (evil-visual-type) 'line)))

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
(evil-define-operator narf/evil-macro-on-all-lines (beg end &optional macro)
  "Apply macro to each line."
  :motion nil
  :move-point nil
  (interactive "<r><a>")
  (unless (and beg end)
    (setq beg (region-beginning)
          end (region-end)))
  (evil-ex-normal beg end
                  (concat "@"
                          (single-key-description
                           (or macro (read-char "@-"))))))

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

;;; Custom argument handlers
;;;###autoload
(defun narf/-ex-match-init (name &optional face update-hook)
  (with-current-buffer evil-ex-current-buffer
    (cond
     ((eq flag 'start)
      (evil-ex-make-hl name
        :face (or face 'evil-ex-substitute-matches)
        :update-hook (or update-hook #'evil-ex-pattern-update-ex-info))
      (setq flag 'update))

     ((eq flag 'stop)
      (evil-ex-delete-hl name)))))

;;;###autoload
(defun narf/-ex-buffer-match (arg &optional hl-name flags beg end)
  (when (and (eq flag 'update)
             evil-ex-substitute-highlight-all
             (not (zerop (length arg))))
    (condition-case lossage
        (let ((pattern (evil-ex-make-substitute-pattern
                        (if evil-ex-bang (regexp-quote arg) arg)
                        (or flags (list))))
              (range (or (evil-copy-range evil-ex-range)
                         (evil-range (or beg (line-beginning-position))
                                     (or end (line-end-position))
                                     'line
                                     :expanded t))))
          (evil-expand-range range)
          (evil-ex-hl-set-region hl-name
                                 (max (evil-range-beginning range) (window-start))
                                 (min (evil-range-end range) (window-end)))
          (evil-ex-hl-change hl-name pattern))
      (end-of-file
       (evil-ex-pattern-update-ex-info nil "incomplete replacement"))
      (user-error
       (evil-ex-pattern-update-ex-info nil (format "?%s" lossage))))))

(provide 'defuns-evil)
;;; defuns-evil.el ends here
