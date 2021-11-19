;;; flycheck-eglot --- Hacky eglot support in flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;; This file sets up flycheck so that, when eglot receives a publishDiagnostics method
;; from the server, flycheck updates the reports.
;;
;; Thanks to:
;; - joaotavora for adding a handle to plug flycheck, and
;; - purcell for finding out the initial stub and the current implementation
;;
;; It works by creating a bridge function which can be used as the argument of
;; `eglot-flymake-backend', which both consumes diagnostics and queue a call to
;; 'flycheck-buffer'
;;
;;; Code:

(defvar-local +lsp--flycheck-eglot--current-errors nil)

(defun +lsp--flycheck-eglot-init (checker callback)
  "CHECKER is the checker (eglot).
CALLBACK is the function that we need to call when we are done, on all the errors."
  (eglot-flymake-backend #'+lsp--flycheck-eglot--on-diagnostics)
  (funcall callback 'finished +lsp--flycheck-eglot--current-errors))

(defun +lsp--flycheck-eglot--on-diagnostics (diags &rest _)
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
           :checker 'eglot
           :buffer (current-buffer)
           :filename (buffer-file-name)))))
    (setq +lsp--flycheck-eglot--current-errors
          (mapcar #'flymake-diag->flycheck-err diags))
    ;; Call Flycheck to update the diagnostics annotations
    (flycheck-buffer-deferred)))

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
      (flymake-mode -1)
      (when-let ((current-checker (flycheck-get-checker-for-buffer)))
        (unless (equal current-checker 'eglot)
          (flycheck-add-next-checker 'eglot current-checker)))
      (flycheck-add-mode 'eglot major-mode)
      (flycheck-mode 1)
      ;; Call flycheck on initilization to make sure to display initial
      ;; errors
      (flycheck-buffer-deferred))))

(after! flymake
  (when (and
         (not (fboundp 'flymake--diag-buffer))
         (fboundp 'flymake--diag-locus))
    (defalias 'flymake--diag-buffer 'flymake--diag-locus)))

;;; flycheck-eglot.el ends here
