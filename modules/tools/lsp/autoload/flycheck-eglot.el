;;; flycheck-eglot --- Hacky eglot support in flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;; This file sets up flycheck so that, when eglot receives a publishDiagnostics method
;; from the server, then eglot calls a report function that creates diagnostics for
;; flycheck.
;;
;; It works by creating an eglot-specific callback function, and using this as 
;; the REPORT-FN argument of `eglot-flymake-backend', which internally registers 
;; that lambda as the function to use whenever there is a publishDiagnostics method.
;; Calling `+lsp--flycheck-eglot-init' "too late" is not a problem, since if there
;; are any unreported/missed diagnostics, eglot ensures that the
;; REPORT-FN function is called immediately.
;;
;; Note: as long as joaotavora/eglot#596 isn't fixed/dealt with, this checker cannot
;; work. Please check the issue on github for more context
;;; Code:
(defun +lsp--flycheck-eglot-init (checker callback)
  "CHECKER is the checker (eglot).
CALLBACK is the function that we need to call when we are done, on all the errors."
  (cl-labels
      ((flymake-diag->flycheck-err
        (diag)
        (with-current-buffer (flymake--diag-buffer diag)
          (flycheck-error-new-at-pos
           (flymake--diag-beg diag)
           (pcase (flymake--diag-type diag)
             ('eglot-note 'info)
             ('eglot-warning 'warning)
             ('eglot-error 'error)
             (_ (error "Unknown diagnostic type, %S" diag)))
           (flymake--diag-text diag)
           :end-pos (flymake--diag-end diag)
           :checker checker
           :buffer (current-buffer)
           :filename (buffer-file-name)))))
    ;; NOTE: Setting up eglot to automatically create flycheck errors for the buffer.
    ;; Internally, this sets the lambda as the callback to be used by eglot
    ;; when it receives a publishDiagnostics method from the server
    (eglot-flymake-backend
     (lambda (flymake-diags &rest _)
       (funcall callback
                'finished
                (mapcar #'flymake-diag->flycheck-err flymake-diags))))))

(defun +lsp--flycheck-eglot-available-p ()
  (bound-and-true-p eglot--managed-mode))

(flycheck-define-generic-checker 'eglot
  "Report `eglot' diagnostics using `flycheck'."
  :start #'+lsp--flycheck-eglot-init
  :predicate #'+lsp--flycheck-eglot-available-p
  :modes '(prog-mode text-mode))

(push 'eglot flycheck-checkers)

(add-hook! 'eglot-managed-mode-hook
  (defun +lsp-eglot-prefer-flycheck-h ()
    (when eglot--managed-mode
      (when-let ((current-checker (flycheck-get-checker-for-buffer)))
        (unless (equal current-checker 'eglot)
          (flycheck-add-next-checker 'eglot current-checker)))
      (flycheck-add-mode 'eglot major-mode)
      (flycheck-mode 1)
      (flymake-mode -1))))

;;; flycheck-eglot.el ends here
