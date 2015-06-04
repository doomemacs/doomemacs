;;;; HTML ;;;;

;;;###autoload
(defun narf:replace-ms-word-chars (beg end)
  "Replace smart quotes and other MS Word verbiage into plain text"
  (interactive "r")
  (replace-regexp "…" "..."   nil beg end)
  (replace-regexp "[‘’]" "'"  nil beg end)
  (replace-regexp "[“”]" "\"" nil beg end))

;;;###autoload
(defun narf:replace-email2mailto (beg end)
  "Email address with mailto link"
  (interactive "r")
  (replace-regexp "\\b\\([a-zA-Z0-9._+-%]+@[a-zA-Z0-9-.]+\\.[a-zA-Z]+\\)\\b"
                  "<a href=\"mailto:\\1\">\\1</a>"
                  nil beg end))

;;;###autoload
(defun narf:replace-url2anchor (beg end)
  "Link with anchor"
  (interactive "r")
  (replace-regexp "\\bhttps?://.+?\\b"
                  "<a href=\"\\1\">\\1</a>"
                  nil beg end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A hacky attempt to replace ace-jump line mode that incrementally shows where
;; you will land as you type the line number.
(defun narf--goto-line (line)
  (let ((lines (count-lines (point-min) (point-max))))
    (if (and (<= line (1+ lines))
             (> line 0))
        (narf/nlinum-hl-line line)
      (narf/nlinum-hl-line))))

;;;###autoload
(defun narf:goto-line ()
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

;;;###autoload (autoload 'narf::align "defuns-edit")
(evil-define-command narf::align (beg end &optional regexp bang)
  :repeat nil
  (interactive "<r><a><!>")
  (when regexp
    (align-regexp beg end
                  (concat "\\(\\s-*\\)" (rxt-pcre-to-elisp regexp)) 1 1)))

;;;###autoload (autoload 'narf::retab "defuns-edit")
(evil-define-operator narf::retab (beg end)
  "Akin to vim's narf::retab, this changes all tabs-to-spaces or spaces-to-tabs,
  depending on `indent-tab-mode'. Untested."
  :motion nil
  :move-point nil
  :type line
  (interactive "<r>")
  (unless (and beg end)
    (setq beg (point-min))
    (setq end (point-max)))
  (if indent-tabs-mode
      (tabify beg end)
    (untabify beg end)))

;;;###autoload (autoload 'narf::narrow-indirect-or-widen "defuns-edit")
(evil-define-operator narf::narrow-indirect-or-widen (beg end)
  "Indirectly narrow the region from BEG to END."
  :move-point nil
  :type exclusive
  :repeat nil
  (interactive "<r>")
  (evil-normal-state)
  (if (buffer-narrowed-p)
      (narf:widen)
    (narf:narrow-to-region-indirect beg end)))

;;;###autoload
(defun narf:toggle-delete-trailing-whitespace ()
  (interactive)
  (if (-contains-p before-save-hook 'delete-trailing-whitespace)
      (progn (narf|disable-delete-trailing-whitespace)
             (message "delete-trailing-whitespaces OFF"))
    (add-hook 'before-save-hook 'delete-trailing-whitespace)
    (message "delete-trailing-whitespaces ON")))


(provide 'defuns-edit)
;;; defuns-edit.el ends here
