;;; defuns-highlight-indentation.el

;;;###autoload
(defun doom/add-whitespace (&optional start end)
  "Maintain indentation whitespace in buffer. Used so that highlight-indentation will
display consistent guides. Whitespace is stripped out on save, so this doesn't affect the
end file."
  (interactive (progn (barf-if-buffer-read-only)
                      (if (use-region-p)
                          (list (region-beginning) (region-end))
                        (list nil nil))))
  (unless indent-tabs-mode
    (save-match-data
      (save-excursion
        (let ((end-marker (copy-marker (or end (point-max))))
              (start (or start (point-min))))
          (goto-char start)
          (while (and (re-search-forward "^$" end-marker t) (not (>= (point) end-marker)))
            (let (line-start line-end next-start next-end)
              (save-excursion
                ;; Check previous line indent
                (forward-line -1)
                (setq line-start (point)
                      line-end (save-excursion (back-to-indentation) (point)))
                ;; Check next line indent
                (forward-line 2)
                (setq next-start (point)
                      next-end (save-excursion (back-to-indentation) (point)))
                ;; Back to origin
                (forward-line -1)
                ;; Adjust indent
                (let* ((line-indent (- line-end line-start))
                       (next-indent (- next-end next-start))
                       (indent (min line-indent next-indent)))
                  (insert (make-string (if (zerop indent) 0 (1+ indent)) ? )))))
            (forward-line 1)))))
    (set-buffer-modified-p nil))
  nil)

;;;###autoload
(defun doom*hl-indent-guess-offset ()
  (string-to-int (gethash 'indent_size (editorconfig-get-properties))))

(provide 'defuns-highlight-indentation)
;;; defuns-highlight-indentation.el ends here
