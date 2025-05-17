;;; lang/beancount/autoload.el -*- lexical-binding: t; -*-

;;
;;; Helpers

;; Lifted from ledger-mode
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
           #'+beancount--navigate-next-xact
           #'+beancount--navigate-end-of-xact
           #'+beancount--sort-startkey))))
    (goto-char (point-min))
    (re-search-forward (regexp-quote target-xact))
    (goto-char (+ (match-beginning 0) point-delta))))

(defvar compilation-read-command)
;;;###autoload
(defun +beancount/balance (&optional all-accounts)
  "Display a balance report with bean-report (bean-report bal)."
  (interactive "P")
  (let (compilation-read-command
        current-prefix-arg)
    (beancount--run "bean-query"
                    buffer-file-name
                    (format (concat "SELECT account, sum(position) as balance %s "
                                    "GROUP BY account "
                                    "HAVING not empty(sum(position)) "
                                    "ORDER BY account")
                            (if all-accounts
                                "" (format "WHERE account ~ \"^(Assets|Liabilities)\"" ))))))

;;;###autoload
(defun +beancount/clone-transaction ()
  "Clones a transaction from (and to the bottom of) the current ledger buffer.

Updates the date to today."
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
  "Clones the transaction at point to the bottom of the ledger.

Updates the date to today."
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
(defun +beancount/occur (account &optional disable?)
  "Hide transactions that don't involve ACCOUNT.

If DISABLE? (universal arg), reveal hidden accounts without prompting."
  (interactive
   (list (unless current-prefix-arg
           ;; REVIEW: Could/should this be generalized to search for arbitrary
           ;;   regexps, if desired?
           (completing-read "Account: " #'beancount-account-completion-table))
         current-prefix-arg))
  (with-silent-modifications
    (save-excursion
      (setq header-line-format nil)
      ;; TODO: Namespace these text-properties, in case of conflicts
      (remove-text-properties (point-min) (point-max) '(invisible nil display nil))
      (unless disable?
        ;; TODO: Prettier header-line display
        (setq header-line-format `("" "Filtering by account: " ,account))
        (let ((start (point-min))
              (placeholder (propertize "[...]\n" 'face 'shadow)))
          (goto-char start)
          (while (re-search-forward (concat "\\_<" (regexp-quote account) "\\_>") nil t)
            (save-excursion
              (seq-let (beg end) (beancount-find-transaction-extents (point))
                ;; TODO: Highlight entry (ala org-occur)
                (if (= beg end)
                    (setq end (save-excursion (goto-char end) (1+ (pos-eol)))))
                (put-text-property start beg 'invisible t)
                (put-text-property start beg 'display placeholder)
                (setq start end))))
          (put-text-property start (point-max) 'invisible t)
          (put-text-property start (point-max) 'display placeholder))))))

;;;###autoload
(defun +beancount/next-transaction (&optional count)
  "Jump to the start of the next COUNT-th transaction."
  (interactive "p")
  (let ((beancount-transaction-regexp
         ;; Don't skip over timestamped directives (like balance or event
         ;; declarations).
         (concat beancount-timestamped-directive-regexp
                 "\\|" beancount-transaction-regexp)))
    (dotimes (_ (or count 1))
      (beancount-goto-next-transaction))))

;;;###autoload
(defun +beancount/previous-transaction (&optional count)
  "Jump to the start of current or previous COUNT-th transaction.

Return non-nil if successful."
  (interactive "p")
  (let ((pos (point)))
    (condition-case e
        (progn
          ;; Ensures "jump to top of current transaction" behavior that is
          ;; common for jump-to-previous commands like this in other Emacs modes
          ;; (like org-mode).
          (or (bolp) (goto-char (pos-eol)))
          (re-search-backward
           (concat beancount-timestamped-directive-regexp
                   "\\|" beancount-transaction-regexp)))
      ('search-failed (goto-char pos) nil))))

;;;###autoload
(defun +beancount--flymake-bean-check--run-a (report-fn &rest _ignored)
  (unless (executable-find flymake-bean-check-executable)
    (error "The executable %s doesn't exist. See `flymake-bean-check-executable'"
           flymake-bean-check-executable))
  (when (and flymake-bean-check-process
             (process-live-p flymake-bean-check-process))
    (kill-process flymake-bean-check-process))
  (let* ((source (current-buffer))
         (buffer (generate-new-buffer "*flymake-bean-check*"))
         (cache-file (flymake-bean-check-cache-filename (buffer-file-name))))
    (setq flymake-bean-check-process
          (make-process :buffer buffer
                        :name "flymake-bean-check"
                        :noquery t
                        :connection-type 'pipe
                        :command (list flymake-bean-check-executable
                                       "/dev/stdin"
                                       "--cache-filename" cache-file)
                        :sentinel
                        (lambda (proc _event)
                          (when (memq (process-status proc) '(exit signal))
                            (unwind-protect
                                (with-current-buffer buffer
                                  (goto-char (point-min))
                                  (let (result)
                                    (while (re-search-forward flymake-bean-check-location-regexp
                                                              nil t)
                                      (pcase-let*
                                          ((message (match-string 2))
                                           (`(,begin . ,end) (flymake-diag-region
                                                              source
                                                              (string-to-number (match-string 1)))))
                                        (push (flymake-make-diagnostic source begin end
                                                                       :error message)
                                              result)))
                                    (funcall report-fn (nreverse result))))
                              (kill-buffer buffer))))))
    (process-send-string
     flymake-bean-check-process
     (save-restriction
       (widen)
       (with-temp-buffer
         (save-excursion (insert-buffer-substring source))
         (while (re-search-forward (rx bol
                                       (or (seq (= 4 num) "-" (= 2 num) "-" (= 2 num) (+ " ")
                                                "document" (+ " ")
                                                (+ (or alnum ":" "_" "-")))
                                           "include"
                                           (seq "option" (+ " ") "\"documents\""))
                                       (+ " ") "\""
                                       (group (+ (not "\""))))
                                   nil t)
           (replace-match (expand-file-name
                           (match-string-no-properties 1))
                          t t nil 1))
         (buffer-substring-no-properties (point-min) (point-max)))))
    (process-send-eof flymake-bean-check-process)))
