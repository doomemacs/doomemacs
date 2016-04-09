;;; module-processing.el

(use-package processing-mode
  :when IS-MAC
  :commands (processing-mode processing-find-sketch)
  :mode "\\.pde$"
  :config
  (setq processing-location "/usr/local/bin/processing-java"
        processing-application-dir "/Applications/Processing.app"
        processing-sketchbook-dir "~/Dropbox/work/pde"
        processing-output-dir "/tmp")

  (define-builder! processing-mode processing-sketch-build)
  (after! quickrun
    (quickrun-add-command
     "processing" `((:command . ,processing-location)
                    (:exec . (lambda () (format "--sketch=%s --output=%s --force --run"
                                           (narf/project-root) processing-output-dir)))
                    (:description . "Run Processing sketch"))
     :mode 'processing-mode))

  (map! :map processing-mode-map
        :nv "M-r" 'processing-sketch-run
        :m "gD" 'processing-find-in-reference
        :m "gF" 'processing-find-sketch
        (:localleader
          "e" 'processing-export-application
          "h" 'processing-open-reference
          "e" 'processing-open-examples
          "o" 'processing-open-sketchbook))

  (add-hook 'processing-compilation-mode-hook 'narf|hide-mode-line)
  (add-hook! processing-mode
    (setq-local company-backends '((company-keywords
                                    :with
                                    company-yasnippet
                                    company-dabbrev-code)))
    (make-local-variable 'company-keywords-alist)
    (add-to-list 'company-keywords-alist
                 (cons 'processing-mode (append processing-functions
                                                processing-builtins
                                                processing-constants)))))

(provide 'module-processing)
;;; module-processing.el ends here
