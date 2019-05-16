;;; core/autoload/text.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-surrounded-p (pair &optional inline balanced)
  "Returns t if point is surrounded by a brace delimiter: {[(

If INLINE is non-nil, only returns t if braces are on the same line, and
whitespace is balanced on either side of the cursor.

If INLINE is nil, returns t if the opening and closing braces are on adjacent
lines, above and below, with only whitespace in between."
  (when pair
    (let ((beg (plist-get pair :beg))
          (end (plist-get pair :end))
          (pt (point)))
      (when (and (> pt beg) (< pt end))
        (when-let* ((cl (plist-get pair :cl))
                    (op (plist-get pair :op)))
          (and (not (string= op ""))
               (not (string= cl ""))
               (let ((nbeg (+ (length op) beg))
                     (nend (- end (length cl))))
                 (let ((content (buffer-substring-no-properties nbeg nend)))
                   (and (string-match-p (format "[ %s]*" (if inline "" "\n")) content)
                        (or (not balanced)
                            (= (- pt nbeg) (- nend pt))))))))))))


;;
;; Commands

;;;###autoload
(defun doom/backward-to-bol-or-indent ()
  "Jump between the indentation column (first non-whitespace character) and the
beginning of the line. The opposite of
`doom/forward-to-last-non-comment-or-eol'."
  (interactive)
  (let ((pos (point))
        (indent (save-excursion
                  (beginning-of-visual-line)
                  (skip-chars-forward " \t\r")
                  (point))))
    (cond ((or (> pos indent) (= pos (line-beginning-position)))
           (goto-char indent))
          ((<= pos indent)
           (beginning-of-visual-line)))))

;;;###autoload
(defun doom/forward-to-last-non-comment-or-eol ()
  "Jumps between the last non-blank, non-comment character in the line and the
true end of the line. The opposite of `doom/backward-to-bol-or-indent'."
  (interactive)
  (let ((eol (save-excursion (if visual-line-mode
                                 (end-of-visual-line)
                               (end-of-line))
                             (point))))
    (if (or (and (< (point) eol)
                 (sp-point-in-comment))
            (not (sp-point-in-comment eol)))
        (goto-char eol)
      (let* ((bol (save-excursion (beginning-of-visual-line) (point)))
             (boc (or (save-excursion
                        (if (not comment-use-syntax)
                            (progn
                              (goto-char bol)
                              (when (re-search-forward comment-start-skip eol t)
                                (or (match-end 1) (match-beginning 0))))
                          (goto-char eol)
                          (while (and (sp-point-in-comment)
                                      (> (point) bol))
                            (backward-char))
                          (skip-chars-backward " " bol)
                          (point)))
                      eol)))
        (cond ((= boc (point))
               (goto-char eol))
              ((/= bol boc)
               (goto-char boc)))))))

;;;###autoload
(defun doom/dumb-indent ()
  "Inserts a tab character (or spaces x tab-width)."
  (interactive)
  (if indent-tabs-mode
      (insert "\t")
    (let* ((movement (% (current-column) tab-width))
           (spaces (if (= 0 movement) tab-width (- tab-width movement))))
      (insert (make-string spaces ? )))))

;;;###autoload
(defun doom/dumb-dedent ()
  "Dedents the current line."
  (interactive)
  (if indent-tabs-mode
      (call-interactively #'backward-delete-char)
    (unless (bolp)
      (save-excursion
        (when (> (current-column) (current-indentation))
          (back-to-indentation))
        (let ((movement (% (current-column) tab-width)))
          (delete-char
           (- (if (= 0 movement)
                  tab-width
                (- tab-width movement)))))))))

;;;###autoload
(defun doom/backward-kill-to-bol-and-indent ()
  "Kill line to the first non-blank character. If invoked again
afterwards, kill line to beginning of line."
  (interactive)
  (let ((empty-line-p (save-excursion (beginning-of-line)
                                      (looking-at-p "[ \t]*$"))))
    (funcall (if (fboundp 'evil-delete)
                 #'evil-delete
               #'delete-region)
             (point-at-bol) (point))
    (unless empty-line-p
      (indent-according-to-mode))))

;;;###autoload
(defun doom/retab (arg &optional beg end)
  "Converts tabs-to-spaces or spaces-to-tabs within BEG and END (defaults to
buffer start and end, to make indentation consistent. Which it does depends on
the value of `indent-tab-mode'.

If ARG (universal argument) is non-nil, retab the current buffer using the
opposite indentation style."
  (interactive "Pr")
  (unless (and beg end)
    (setq beg (point-min)
          end (point-max)))
  (let ((indent-tabs-mode (if arg (not indent-tabs-mode) indent-tabs-mode)))
    (if indent-tabs-mode
        (tabify beg end)
      (untabify beg end))))

;;;###autoload
(defun doom/delete-trailing-newlines ()
  "Trim trailing newlines.

Respects `require-final-newline'."
  (interactive)
  (goto-char (point-max))
  (skip-chars-backward " \t\n\v")
  (when (looking-at "\n\\(\n\\|\\'\\)")
    (forward-char 1))
  (when require-final-newline
    (unless (bolp)
      (insert "\n")))
  (when (looking-at "\n+")
    (replace-match "")))

;;;###autoload
(defun doom/dos2unix ()
  "Convert the current buffer to a Unix file encoding."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;###autoload
(defun doom/unix2dos ()
  "Convert the current buffer to a DOS file encoding."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))


;;
;; Hooks

;;;###autoload
(defun doom|enable-delete-trailing-whitespace ()
  "Enables the automatic deletion of trailing whitespaces upon file save.

i.e. enables `ws-butler-mode' in the current buffer."
  (ws-butler-mode +1))

;;;###autoload
(defun doom|disable-delete-trailing-whitespace ()
  "Disables the automatic deletion of trailing whitespaces upon file save.

i.e. disables `ws-butler-mode' in the current buffer."
  (ws-butler-mode -1))

;;;###autoload
(defun doom|enable-show-trailing-whitespace ()
  "Enable `show-trailing-whitespace' in the current buffer."
  (setq-local show-trailing-whitespace t))

;;;###autoload
(defun doom|disable-show-trailing-whitespace ()
  "Disable `show-trailing-whitespace' in the current buffer."
  (setq-local show-trailing-whitespace nil))
