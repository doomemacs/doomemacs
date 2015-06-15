(defvar require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require
  (around build-require-times (feature &optional filename noerror) activate)
  "Note in `require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (add-to-list 'require-times
                     (cons feature
                           (float-time (time-subtract (current-time) require-start-time)))
                     t)))))

(defun list-times ()
  (interactive)
  (let ((temp-buffer (get-buffer-create "*benchmark*"))
        (sum 0.0))
    (popwin:popup-buffer temp-buffer :stick t)
    (erase-buffer)
    (org-mode)
    (dolist (feature require-times)
      (if (eq feature 'null)
          (progn
            (insert "|----+----+----|\n")
            (insert (format "| %6f | Subtotal |\n" sum))
            (insert "|----+----+----|\n"))
        (let ((time (cdr feature)))
          (insert (format "| %6f | %s | %s |\n" time (car feature) (cond ((>= time 0.4)  "XXX")
                                                                         ((>= time 0.1)  "X")
                                                                         ((>= time 0.05) ".")
                                                                         (t              " "))))
          (setq sum (+ sum time)))))
    (save-excursion
      (insert "|----+----+----|\n")
      (insert (format "| %6f | Total |\n" sum))
      (insert (format "| %s | On Init |\n" (emacs-init-time))))
    (org-table-align)))


(provide 'benchmark)
