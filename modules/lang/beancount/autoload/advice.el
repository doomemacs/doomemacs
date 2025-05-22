;;; lang/beancount/autoload/advice.el -*- lexical-binding: t; -*-

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
         (save-excursion
           (while (re-search-forward "^;+# " nil t)
             (replace-match "" t t)))
         (while (re-search-forward
                 (rx bol
                     (or (seq (= 4 num) "-" (= 2 num) "-" (= 2 num) (+ " ")
                              "document" (+ " ")
                              (+ (or alnum ":" "_" "-")))
                         "include"
                         (seq "option" (+ " ") "\"documents\""))
                     (+ " ") "\""
                     (group (+ (not "\""))))
                 nil t)
           (unless (file-name-absolute-p (match-string-no-properties 1))
             (replace-match (expand-file-name
                             (match-string-no-properties 1))
                            t t nil 1)))
         (buffer-substring-no-properties (point-min) (point-max)))))
    (process-send-eof flymake-bean-check-process)))

;;; advice.el ends here
