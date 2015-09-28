;;; module-python.el

(use-package python
  :mode        ("\\.py\\'" . python-mode)
  :interpreter ("python"   . python-mode)
  :commands    python-mode
  :init
  (add-hook! python-mode '(narf|enable-tab-width-4 emr-initialize flycheck-mode))
  (setq python-indent-offset 4
        python-environment-directory narf-temp-dir
        python-shell-interpreter "ipython")
  :config
  (define-key python-mode-map (kbd "DEL") nil)) ; interferes with smartparens

(use-package nose
  :commands nose-mode
  :preface  (defvar nose-mode-map (make-sparse-keymap))
  :init     (associate! nose-mode :pattern "/test_.+\\.py\\'")
  :config
  (bind! :map nose-mode-map
         (:prefix ","
           :n "tr" 'nosetests-again
           :n "ta" 'nosetests-all
           :n "ts" 'nosetests-one
           :n "tv" 'nosetests-module
           :n "tA" 'nosetests-pdb-all
           :n "tO" 'nosetests-pdb-one
           :n "tV" 'nosetests-pdb-module)))

(use-package anaconda-mode
  :defines (anaconda-mode-map anaconda-nav-mode-map)
  :functions (anaconda-mode-running-p)
  :init (add-hook! python-mode '(anaconda-mode eldoc-mode))
  :config
  (bind! :map anaconda-mode-map     :m "gd"     'anaconda-mode-goto-definitions)
  (bind! :map anaconda-nav-mode-map :n [escape] 'anaconda-nav-quit)

  (advice-add 'anaconda-mode-doc-buffer :after 'narf*anaconda-mode-doc-buffer)

  (after! company
    (require 'company-anaconda)
    (add-company-backend! python-mode (anaconda)))

  (after! emr
    (emr-declare-command
     'anaconda-mode-view-doc
     :title "view documentation"
     :modes 'python-mode
     :predicate (lambda () (and (anaconda-mode-running-p)
                                (not (use-region-p))
                                (not (sp-point-in-string-or-comment)))))
    (emr-declare-command
     'anaconda-mode-goto-assignments
     :title "go to assignments"
     :modes 'python-mode
     :predicate (lambda () (and (anaconda-mode-running-p)
                                (not (use-region-p))
                                (not (sp-point-in-string-or-comment)))))
    (emr-declare-command
     'anaconda-mode-goto-definitions
     :title "go to definition"
     :modes 'python-mode
     :predicate (lambda () (and (anaconda-mode-running-p)
                                (not (use-region-p))
                                (not (sp-point-in-string-or-comment)))))
    (emr-declare-command
     'anaconda-mode-usages
     :title "show usages"
     :modes 'python-mode
     :predicate (lambda () (and (anaconda-mode-running-p)
                                (not (use-region-p))
                                (not (sp-point-in-string-or-comment)))))))

(provide 'module-python)
;;; module-python.el ends here
