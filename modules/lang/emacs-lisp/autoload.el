;;; lang/emacs-lisp/autoload.el

;; ---- emacs-lisp ---------------------------------------------------

;;;###autoload
(defun +emacs-lisp/repl ()
  "Open the Emacs Lisp REPL (`ielm')."
  (interactive)
  (pop-to-buffer
   (or (get-buffer "*ielm*")
       (progn (ielm)
              (let ((buf (get-buffer "*ielm*")))
                (bury-buffer buf)
                buf)))))


;; ---- ert ---------------------------------------------------

(defsubst +ert--pre ()
  (save-buffer) (eval-buffer))

;;;###autoload
(defun +ert/run-test ()
  (interactive)
  (let (case-fold-search thing)
    (+ert--pre)
    (setq thing (thing-at-point 'defun t))
    (if thing
        (if (string-match "(ert-deftest \\([^ ]+\\)" thing)
            (ert-run-tests-interactively
             (substring thing (match-beginning 1) (match-end 1)))
          (user-error "Invalid test at point"))
      (user-error "No test found at point"))))

;;;###autoload
(defun +ert/rerun-test ()
  (interactive)
  (let (case-fold-search thing)
    (+ert--pre)
    (setq thing (car-safe ert--selector-history))
    (if thing
        (ert-run-tests-interactively thing)
      (message "No test found in history, looking for test at point")
      (+ert-run-test))))

;;;###autoload
(defun +ert/run-all-tests ()
  (interactive)
  (ert-delete-all-tests)
  (+ert--pre)
  (ert-run-tests-interactively t))

