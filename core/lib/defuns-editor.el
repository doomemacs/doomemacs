;;; defuns-editor.el
;; for ../core-editor.el

;; A hacky attempt to replace ace-jump line mode that incrementally shows where
;; you will land as you type the line number.
(defun narf--goto-line (line)
  (let ((lines (count-lines (point-min) (point-max))))
    (if (and (<= line (1+ lines))
             (> line 0))
        (narf/nlinum-hl-line line)
      (narf/nlinum-hl-line))))

;;;###autoload
(defun narf/editor-goto-line ()
  (interactive)
  (let ((keys '())
        (orig-point (point))
        (echo-keystrokes 0))
    (evil-save-echo-area
      (catch 'abort
        (while t
          (let* ((keystr (concat keys))
                 (key (read-event (concat ":" keystr))))
            (cond ((eq key 'escape)
                   (message "%s" key)
                   (throw 'abort t))
                  ((eq key 'return)
                   (when keys
                     (goto-line (string-to-number keystr)))
                   (throw 'abort t))
                  ((eq key 'backspace)
                   (let ((key-len (length keys)))
                     (if (= key-len 0)
                         (throw 'abort t)
                       (if (> key-len 1)
                           (progn
                             (nbutlast keys)
                             (narf--goto-line (string-to-number (concat keys))))
                         (setq keys '())
                         (narf/nlinum-hl-line)))))
                  ((and (characterp key)
                        (s-numeric? (char-to-string key)))
                   (setq keys (append keys (list key)))
                   (narf--goto-line (string-to-number (concat keys))))
                  (t
                   (if (or (char-equal key ?\C-n)
                           (char-equal key ?\C-j))
                       (progn
                         (setq keys (number-to-string (1+ (string-to-number (concat keys)))))
                         (narf--goto-line (string-to-number (concat keys))))
                     (when (or (char-equal key ?\C-p)
                               (char-equal key ?\C-k))
                       (setq keys (number-to-string (1- (string-to-number (concat keys)))))
                       (narf--goto-line (string-to-number (concat keys)))))))))))))

(provide 'defuns-editor)
;;; defuns-editor.el ends here
