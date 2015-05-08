(use-package rust-mode
  :mode "\\.rs$"
  :config
  ;; (after "company"
  ;;   (let ((racer-dir (concat my-contrib-dir "racer/")))
  ;;     (setq racer-rust-src-path (concat racer-dir "src"))
  ;;     (setq racer-cmd (concat racer-dir "bin/racer"))
  ;;     (add-to-list 'load-path (concat racer-dir "editors/"))
  ;;     (require 'racer)))
  )

(provide 'init-rust)
;;; init-rust.el ends here
