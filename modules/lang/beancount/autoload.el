;;; lang/beancount/autoload.el -*- lexical-binding: t; -*-

;;
;;; Helpers

;; Lifted from ledger
(defconst +beancount--payee-any-status-regex
  "^[0-9]+[-/][-/.=0-9]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+\\(.+?\\)\\s-*\\(;\\|$\\)")

(defun +beancount--sort-startkey ()
  "Return the actual date so the sort subroutine doesn't sort on the entire first line."
  (buffer-substring-no-properties (point) (+ 10 (point))))

(defun +beancount--navigate-next-xact ()
  "Move point to beginning of next xact."
  ;; make sure we actually move to the next xact, even if we are the beginning
  ;; of one now.
  (if (looking-at +beancount--payee-any-status-regex)
      (forward-line))
  (if (re-search-forward  +beancount--payee-any-status-regex nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-max))))

(defun +beancount--navigate-start-xact-or-directive-p ()
  "Return t if at the beginning of an empty or all-whitespace line."
  (not (looking-at "[ \t]\\|\\(^$\\)")))

(defun +beancount--navigate-next-xact-or-directive ()
  "Move to the beginning of the next xact or directive."
  (interactive)
  (beginning-of-line)
  (if (+beancount--navigate-start-xact-or-directive-p) ; if we are the start of an xact, move forward to the next xact
      (progn
        (forward-line)
        (if (not (+beancount--navigate-start-xact-or-directive-p)) ; we have moved forward and are not at another xact, recurse forward
            (+beancount--navigate-next-xact-or-directive)))
    (while (not (or (eobp)  ; we didn't start off at the beginning of an xact
                    (+beancount--navigate-start-xact-or-directive-p)))
      (forward-line))))

(defun +beancount--navigate-next-xact ()
  "Move point to beginning of next xact."
  ;; make sure we actually move to the next xact, even if we are the
  ;; beginning of one now.
  (if (looking-at +beancount--payee-any-status-regex)
      (forward-line))
  (if (re-search-forward  +beancount--payee-any-status-regex nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-max))))

(defun +beancount--navigate-beginning-of-xact ()
  "Move point to the beginning of the current xact."
  ;; need to start at the beginning of a line in case we are in the first line of an xact already.
  (beginning-of-line)
  (let ((sreg (concat "^[=~[:digit:]]")))
    (unless (looking-at sreg)
      (re-search-backward sreg nil t)
      (beginning-of-line)))
  (point))

(defun +beancount--navigate-end-of-xact ()
  "Move point to end of xact."
  (+beancount--navigate-next-xact-or-directive)
  (re-search-backward ".$")
  (end-of-line)
  (point))


;;
;;; Commands

;;;###autoload
(defun +beancount/sort-buffer (&optional reverse)
  "Sort all transactions in the buffer.
If REVERSE (the prefix arg) is non-nil, sort them in reverse."
  (interactive "P")
  (+beancount/sort-region (point-min) (point-max) reverse))

;;;###autoload
(defun +beancount/sort-region (beg end &optional reverse)
  "Sort the transactions inside BEG and END.
If REVERSE (the prefix arg) is non-nil, sort the transactions in reverst order."
  (interactive
   (list (region-beginning)
         (region-end)
         (and current-prefix-arg t)))
  (let* ((new-beg beg)
         (new-end end)
         (bounds (save-excursion
                   (list (+beancount--navigate-beginning-of-xact)
                         (+beancount--navigate-end-of-xact))))
         (point-delta (- (point) (car bounds)))
         (target-xact (buffer-substring (car bounds) (cadr bounds)))
         (inhibit-modification-hooks t))
    (save-excursion
      (save-restriction
        (goto-char beg)
        ;; make sure beg of region is at the beginning of a line
        (beginning-of-line)
        ;; make sure point is at the beginning of a xact
        (unless (looking-at +beancount--payee-any-status-regex)
          (+beancount--navigate-next-xact))
        (setq new-beg (point))
        (goto-char end)
        (+beancount--navigate-next-xact)
        ;; make sure end of region is at the beginning of next record after the
        ;; region
        (setq new-end (point))
        (narrow-to-region new-beg new-end)
        (goto-char new-beg)
        (let ((inhibit-field-text-motion t))
          (sort-subr
           reverse
           '+beancount--navigate-next-xact
           '+beancount--navigate-end-of-xact
           '+beancount--sort-startkey))))
    (goto-char (point-min))
    (re-search-forward (regexp-quote target-xact))
    (goto-char (+ (match-beginning 0) point-delta))))

(defvar compilation-read-command)
;;;###autoload
(defun +beancount/balance ()
  "Run 'bean-report bal'."
  (interactive)
  (let (compilation-read-command)
    (beancount--run "bean-report" buffer-file-name "bal")))

;;;###autoload
(defun +beancount/clone-transaction ()
  "TODO"
  (interactive)
  (save-restriction
    (widen)
    (when-let (transaction
               (completing-read
                "Clone transaction: "
                (string-lines (buffer-string))
                (doom-partial #'string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [*!] ")
                t))
      (goto-char (point-min))
      (re-search-forward (concat "^" (regexp-quote transaction)))
      (+beancount/clone-this-transaction t))))

;;;###autoload
(defun +beancount/clone-this-transaction (&optional arg)
  "Copy the current transaction to the bottom of the ledger.
Updates the date to today"
  (interactive "P")
  (if (and (not arg) (looking-at-p "^$"))
      (call-interactively #'+beancount/clone-transaction)
    (save-restriction
      (widen)
      (let ((transaction
             (buffer-substring-no-properties
              (save-excursion
                (beancount-goto-transaction-begin)
                (re-search-forward " " nil t)
                (point))
              (save-excursion
                (beancount-goto-transaction-end)
                (point)))))
        (goto-char (point-max))
        (delete-blank-lines)
        (beancount-insert-date)
        (insert transaction)))))

;;;###autoload
(defun +beancount/next-transaction (&optional count)
  "Jump to the start of the next COUNT-th transaction."
  (interactive "p")
  (dotimes (_ (or count 1))
    (beancount-goto-next-transaction)))

;;;###autoload
(defun +beancount/previous-transaction (&optional count)
  "Jump to the start of current or previous COUNT-th transaction."
  (interactive "p")
  (re-search-backward
   beancount-transaction-regexp nil t (or count 1)))
