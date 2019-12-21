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

;;;###autoload
(defun doom-point-in-comment-p (&optional pos)
  "Return non-nil if POS is in a comment.

POS defaults to the current position."
  ;; REVIEW Should we cache `syntax-ppss'?
  (let* ((pos (or pos (point)))
         (ppss (syntax-ppss pos)))
    (or (nth 4 ppss)
        (nth 8 ppss)
        (and (< pos (point-max))
             (memq (char-syntax (char-after pos)) '(?< ?>))
             (not (eq (char-after pos) ?\n)))
        (when-let (s (car (syntax-after pos)))
          (or (and (/= 0 (logand (lsh 1 16) s))
                   (nth 4 (doom-syntax-ppss (+ pos 2))))
              (and (/= 0 (logand (lsh 1 17) s))
                   (nth 4 (doom-syntax-ppss (+ pos 1))))
              (and (/= 0 (logand (lsh 1 18) s))
                   (nth 4 (doom-syntax-ppss (- pos 1))))
              (and (/= 0 (logand (lsh 1 19) s))
                   (nth 4 (doom-syntax-ppss (- pos 2)))))))))

;;;###autoload
(defun doom-point-in-string-p (&optional pos)
  "Return non-nil if POS is in a string."
  ;; REVIEW Should we cache `syntax-ppss'?
  (nth 3 (syntax-ppss pos)))

;;;###autoload
(defun doom-point-in-string-or-comment-p (&optional pos)
  "Return non-nil if POS is in a string or comment."
  (or (doom-point-in-string-p pos)
      (doom-point-in-comment-p pos)))


;;
;;; Commands

(defvar doom--last-backward-pt most-positive-fixnum)
;;;###autoload
(defun doom/backward-to-bol-or-indent ()
  "Jump between the indentation column (first non-whitespace character) and the
beginning of the line. The opposite of
`doom/forward-to-last-non-comment-or-eol'."
  (interactive)
  (let ((pt (point)))
    (cl-destructuring-bind (bol . bot)
        (save-excursion
          (beginning-of-visual-line)
          (cons (point)
                (progn (skip-chars-forward " \t\r")
                       (point))))
      (cond ((> pt bot)
             (goto-char bot))
            ((= pt bol)
             (goto-char (min doom--last-backward-pt bot))
             (setq doom--last-backward-pt most-positive-fixnum))
            ((<= pt bot)
             (setq doom--last-backward-pt pt)
             (goto-char bol))))))

(defvar doom--last-forward-pt -1)
;;;###autoload
(defun doom/forward-to-last-non-comment-or-eol ()
  "Jumps between the last non-blank, non-comment character in the line and the
true end of the line. The opposite of `doom/backward-to-bol-or-indent'."
  (interactive)
  (let ((eol (if (not visual-line-mode)
                 (line-end-position)
               (save-excursion (end-of-visual-line) (point)))))
    (if (or (and (< (point) eol)
                 (sp-point-in-comment))
            (not (sp-point-in-comment eol)))
        (if (= (point) eol)
            (progn
              (goto-char doom--last-forward-pt)
              (setq doom--last-forward-pt -1))
          (setq doom--last-forward-pt (point))
          (goto-char eol))
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
        (when (> doom--last-forward-pt boc)
          (setq boc doom--last-forward-pt))
        (if (or (= eol (point))
                (> boc (point)))
            (progn
              (goto-char boc)
              (setq doom--last-forward-pt -1))
          (setq doom--last-forward-pt (point))
          (goto-char eol))))))

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

;;;###autoload
(defun doom/toggle-indent-style ()
  "Switch between tabs and spaces indentation style in the current buffer."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "Indent style changed to %s" (if indent-tabs-mode "tabs" "spaces")))

(defvar editorconfig-lisp-use-default-indent)
;;;###autoload
(defun doom/set-indent-width (width)
  "Change the indentation size to WIDTH of the current buffer.

The effectiveness of this command is significantly improved if you have
editorconfig or dtrt-indent installed."
  (interactive
   (list (if (integerp current-prefix-arg)
             current-prefix-arg
           (read-number "New indent size: "))))
  (setq tab-width width)
  (setq-local standard-indent width)
  (when (boundp 'evil-shift-width)
    (setq evil-shift-width width))
  (cond ((require 'editorconfig nil t)
         (let (editorconfig-lisp-use-default-indent)
           (editorconfig-set-indentation nil width)))
        ((require 'dtrt-indent nil t)
         (when-let (var (nth 2 (assq major-mode dtrt-indent-hook-mapping-list)))
           (doom-log "Updated %s = %d" var width)
           (set var width))))
  (message "Changed indentation to %d" width))


;;
;;; Hooks

;;;###autoload
(defun doom-enable-delete-trailing-whitespace-h ()
  "Enables the automatic deletion of trailing whitespaces upon file save.

i.e. enables `ws-butler-mode' in the current buffer."
  (ws-butler-mode +1))

;;;###autoload
(defun doom-disable-delete-trailing-whitespace-h ()
  "Disables the automatic deletion of trailing whitespaces upon file save.

i.e. disables `ws-butler-mode' in the current buffer."
  (ws-butler-mode -1))

;;;###autoload
(defun doom-enable-show-trailing-whitespace-h ()
  "Enable `show-trailing-whitespace' in the current buffer."
  (setq-local show-trailing-whitespace t))

;;;###autoload
(defun doom-disable-show-trailing-whitespace-h ()
  "Disable `show-trailing-whitespace' in the current buffer."
  (setq-local show-trailing-whitespace nil))
