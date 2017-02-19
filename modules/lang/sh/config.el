;;; lang/sh/config.el

(@def-package sh-script ; built-in
  :mode (("\\.zsh$" . sh-mode)
         ("/bspwmrc$" . sh-mode))
  :init
  (@add-hook sh-mode '(flycheck-mode highlight-numbers-mode +sh|extra-fontify))
  :config
  (@set :company-backend 'sh-mode '(company-shell))
  (@set :electric 'sh-mode :words "else" "elif" "fi" "done" "then" "do" "esac" ";;")
  (@set :repl 'sh-mode '+sh/repl)
  (setq sh-indent-after-continuation 'always)

  ;; [pedantry intensifies]
  (@add-hook sh-mode (setq mode-name "sh"))

  (defun +sh|detect-zsh ()
    (when (and buffer-file-name (string-match-p "\\.zsh\\'" buffer-file-name))
      (sh-set-shell "zsh")))
  (add-hook 'sh-mode-hook '+sh|detect-zsh))


(@def-package company-shell
  :after sh-script
  :config (setq company-shell-delete-duplicates t))

