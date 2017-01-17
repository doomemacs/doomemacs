;;; core-dashboard.el

(setq initial-major-mode 'doom-mode
      initial-scratch-message "\n  Loading..."
      inhibit-startup-screen t
      ;; shuts up emacs at startup
      inhibit-startup-echo-area-message user-login-name)

(define-derived-mode doom-mode special-mode
  (concat "v" doom-version)
  "Major mode for DOOM buffers.")

(define-derived-mode doom-dashboard-mode doom-mode
  (concat "v" doom-version)
  "Major mode for DOOM buffers.")

(defvar doom-dashboard-buffer value
  "")

(defvar doom-dashboard-buffer-name " *doom*"
  "")

(defvar doom-dashboard--edited nil
  "")

(defvar doom-dashboard-inhibit-reload nil
  "")

(defvar doom-dashboard-mode-line-format (assq 'minimal doom-modeline-formats)
  "")

(provide 'core-dashboard)
;;; core-dashboard.el ends here
