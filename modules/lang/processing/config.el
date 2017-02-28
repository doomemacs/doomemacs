;;; module-processing.el

(def-package! processing-mode
  :commands (processing-mode processing-find-sketch)
  :mode "\\.pde$"
  :init
  (add-hook 'processing-compilation-mode-hook 'doom-hide-modeline-mode)
  :config
  (set! :build 'build-sketch 'processing-mode 'processing-sketch-build)
  (set! :popup "*processing-compilation*" :size 10 :noselect t)
  (set! :eval 'processing-mode
        '((:command . ,processing-location)
          (:exec . (lambda () (format "--sketch=%s --output=%s --force --run"
                                 (doom/project-root) processing-output-dir)))
          (:description . "Run Processing sketch")))
  (set! :company-backend 'processing-mode
        '(company-keywords :with company-yasnippet company-dabbrev-code))

  (setq processing-location "/usr/local/bin/processing-java"
        processing-application-dir "/Applications/Processing.app"
        processing-sketchbook-dir "~/Dropbox/work/pde"
        processing-output-dir "/tmp")

  (map! :map processing-mode-map
        :nv "M-r" 'processing-sketch-run
        :m "gd" 'processing-find-in-reference

        (:localleader
         :n "e" 'processing-export-application
         :n "h" 'processing-open-reference
         :n "e" 'processing-open-examples
         :n "o" 'processing-open-sketchbook

         :prefix "f"
         :n "s" 'processing-find-sketch))

  (after! company-keywords
    (nconc company-keywords-alist
           (cons 'processing-mode (append processing-functions
                                          processing-builtins
                                          processing-constants)))))
