(use-package rust-mode
  :modes "\\.rs$"
  :config
  (after "company"
    (let ((racer-dir (concat my-elisp-dir "racer/")))
      (setq racer-rust-src-path (concat racer-dir "src"))
      (setq racer-cmd (concat racer-dir "bin/racer"))
      (add-to-list 'load-path (concat racer-dir "editors/"))
      (require 'racer))))

(provide 'init-rust)
;;; init-rust.el ends here
