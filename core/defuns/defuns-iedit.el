;;; defuns-iedit.el

;;;###autoload
(defun narf:iedit-restrict-to-region (&optional beg end)
  (interactive)
  (if (iedit-current-occurrence-string)
      (let ((current-prefix-arg '(4))
            (beg (or beg (region-beginning)))
            (end (or end (region-end))))
        (iedit-done)
        (call-interactively 'iedit-mode)
        (save-excursion (iedit-restrict-region beg end))
        (evil-previous-line))
    (call-interactively 'evil-ret)))

;; Many packages try to do the multiple-cursors thing, but when it comes to
;; multiple-cursors and evil-mode, things get complicated. `evil-mc' doesn't seem stable
;; enough (I haven't been able to get it to work with my setup), `multiple-cursors'
;; doesn't work well with evil's vimmish opinion of where the cursor is...
;;
;; So, this is my solution: using `iedit' and `evil-iedit-mode', I wrote
;; `narf/mark-and-next' and `narf/mark-and-prev', and it works like a charm! When I want
;; "match all" functionality, I use `evil-iedit-mode/iedit-mode' while in visual mode and
;; I get just that.

(defvar narf-mark-first nil)
(defvar narf-mark-pt nil)
(defvar narf-mark-index '(1 . 1))

(defun narf|mark-end ()
  (setq narf-mark-first nil
        narf-mark-pt nil
        narf-mark-index (cons 1 1)))
(add-hook! (iedit-mode-end-hook iedit-aborting) 'narf|mark-end)

;;;###autoload
(defun narf/mark-and-prev ()
  "See `narf/mark-and-next'"
  (interactive)
  (narf/mark-and-next t))

;;;###autoload
(defun narf/mark-and-next (&optional backwards-p)
  "Emulates Sublime Text's (and Atom's) multiple cursors functionality by marking the
current word/selection and marking the next one on consecutive executions of this
function."
  (interactive)
  (setq evil-ex-search-direction (if backwards-p 'backward 'forward))
  (save-excursion
    (if narf-mark-first
        (let ((i (if backwards-p (cdr narf-mark-index) (car narf-mark-index))))
          (goto-char narf-mark-pt)
          (while (and (> i 0)
                      (evil-ex-find-next nil (if backwards-p 'backward 'forward) t))
            (decf i))
          (if (> i 0)
              (message "End of the buffer!")
            (incf (if backwards-p
                      (cdr narf-mark-index)
                    (car narf-mark-index))))
          (unless (iedit-find-current-occurrence-overlay)
            (iedit-toggle-selection)))
      (let* ((bounds (if (evil-visual-state-p)
                         (cons evil-visual-beginning evil-visual-end)
                       (bounds-of-thing-at-point 'word)))
             (beg (car bounds))
             (end (cdr bounds))
             (occurrence (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (setq narf-mark-first (if backwards-p beg end)
              narf-mark-pt (if backwards-p beg end))
        (evil-normal-state)
        (setq iedit-initial-string-local occurrence)
        (iedit-start (iedit-regexp-quote occurrence) beg end)
        (evil-iedit-state)
        (setq evil-ex-search-pattern (evil-ex-make-search-pattern (regexp-quote occurrence)))
        (evil-ex-find-next nil nil t)))))

(provide 'defuns-iedit)
;;; defuns-iedit.el ends here
