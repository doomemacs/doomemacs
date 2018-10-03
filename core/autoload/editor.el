;;; core/autoload/editor.el -*- lexical-binding: t; -*-

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
    (if (and (sp-point-in-comment) (not (= (point) eol)))
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
    (funcall (if (featurep 'evil)
                 #'evil-delete
               #'delete-region)
             (point-at-bol) (point))
    (unless empty-line-p
      (indent-according-to-mode))))

;;;###autoload
(defun doom/backward-delete-whitespace-to-column ()
  "Delete back to the previous column of whitespace, or as much whitespace as
possible, or just one char if that's not possible."
  (interactive)
  (let* ((context (ignore-errors (sp-get-thing)))
         (op (plist-get context :op))
         (cl (plist-get context :cl))
         open-len close-len)
    (cond ;; When in strings (sp acts weird with quotes; this is the fix)
          ;; Also, skip closing delimiters
          ((and op cl
                (string= op cl)
                (and (string= (char-to-string (or (char-before) 0)) op)
                     (setq open-len (length op)))
                (and (string= (char-to-string (or (char-after) 0)) cl)
                     (setq close-len (length cl))))
           (delete-char (- open-len))
           (delete-char close-len))

          ;; Delete up to the nearest tab column IF only whitespace between
          ;; point and bol.
          ((and (not indent-tabs-mode)
                (not (bolp))
                (not (sp-point-in-string))
                (save-excursion (>= (- (skip-chars-backward " \t")) tab-width)))
           (let ((movement (% (current-column) tab-width)))
             (when (= movement 0)
               (setq movement tab-width))
             (delete-char (- movement)))
           (unless (memq (char-before) (list ?\n ?\ ))
             (insert " ")))

          ;; Otherwise do a regular delete
          (t (delete-char -1)))))

;;;###autoload
(defun doom/delete-backward-char (n &optional killflag)
  "Same as `delete-backward-char', but preforms these additional checks:

+ If point is surrounded by (balanced) whitespace and a brace delimiter ({} []
  ()), delete a space on either side of the cursor.
+ If point is at BOL and surrounded by braces on adjacent lines, collapse
  newlines:
  {
  |
  } => {|}
+ Otherwise, resort to `doom/backward-delete-whitespace-to-column'.
+ Resorts to `delete-char' if n > 1"
  (interactive "p\nP")
  (or (integerp n)
      (signal 'wrong-type-argument (list 'integerp n)))
  (cond ((and (use-region-p)
              delete-active-region
              (= n 1))
         ;; If a region is active, kill or delete it.
         (if (eq delete-active-region 'kill)
             (kill-region (region-beginning) (region-end) 'region)
           (funcall region-extract-function 'delete-only)))
        ;; In Overwrite mode, maybe untabify while deleting
        ((null (or (null overwrite-mode)
                   (<= n 0)
                   (memq (char-before) '(?\t ?\n))
                   (eobp)
                   (eq (char-after) ?\n)))
         (let ((ocol (current-column)))
           (delete-char (- n) killflag)
           (save-excursion
             (insert-char ?\s (- ocol (current-column)) nil))))
        ;;
        ((and (= n 1) (bound-and-true-p smartparens-mode))
         (cond ((and (memq (char-before) (list ?\  ?\t))
                     (save-excursion
                       (and (> (- (skip-chars-backward " \t" (line-beginning-position))) 0)
                            (bolp))))
                (doom/backward-delete-whitespace-to-column))
               ((let* ((pair (ignore-errors (sp-get-thing)))
                       (op   (plist-get pair :op))
                       (cl   (plist-get pair :cl))
                       (beg  (plist-get pair :beg))
                       (end  (plist-get pair :end)))
                  (cond ((and end beg (= end (+ beg (length op) (length cl))))
                         (sp-backward-delete-char 1))
                        ((doom-surrounded-p pair 'inline 'balanced)
                         (delete-char -1 killflag)
                         (delete-char 1)
                         (when (= (point) (+ (length cl) beg))
                           (sp-backward-delete-char 1)
                           (sp-insert-pair op)))
                        ((and (bolp) (doom-surrounded-p pair nil 'balanced))
                         (delete-region beg end)
                         (sp-insert-pair op)
                         t)
                        ((run-hook-with-args-until-success 'doom-delete-backward-functions))
                        ((doom/backward-delete-whitespace-to-column)))))))
        ;; Otherwise, do simple deletion.
        ((delete-char (- n) killflag))))

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

(defvar-local doom--buffer-narrowed-origin nil)
;;;###autoload
(defun doom/clone-and-narrow-buffer (beg end &optional clone-p)
  "Restrict editing in this buffer to the current region, indirectly. With CLONE-P,
clone the buffer and hard-narrow the selection. If mark isn't active, then widen
the buffer (if narrowed).

Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/"
  (interactive "rP")
  (cond ((or (region-active-p)
             (and beg end))
         (deactivate-mark)
         (when clone-p
           (let ((old-buf (current-buffer)))
             (switch-to-buffer (clone-indirect-buffer nil nil))
             (setq doom--buffer-narrowed-origin old-buf)))
         (narrow-to-region beg end))
        (doom--buffer-narrowed-origin
         (kill-this-buffer)
         (switch-to-buffer doom--buffer-narrowed-origin)
         (setq doom--buffer-narrowed-origin nil))
        (t
         (widen))))

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


;;
;; Advice

;;;###autoload
(defun doom*newline-indent-and-continue-comments (_orig-fn)
  "Inserts a newline and possibly indents it. Also continues comments if
executed from a commented line; handling special cases for certain languages
with weak native support."
  (interactive)
  (cond ((sp-point-in-string) (newline))
        ((and (sp-point-in-comment)
              comment-line-break-function)
         (funcall comment-line-break-function))
        (t
         (newline nil t)
         (indent-according-to-mode))))


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
