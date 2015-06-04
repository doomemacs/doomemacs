(use-package python
  :mode        ("\\.py\\'" . python-mode)
  :interpreter ("python"   . python-mode)
  :commands    (python-mode)
  :init
  (add-to-hook 'python-mode-hook '(narf|enable-tab-width-4 emr-initialize))
  :config
  (progn
    (setq-default python-indent-offset 4)
    (setq python-environment-directory TMP-DIR)
    (setq python-shell-interpreter "ipython")
    ;; interferes with smartparens
    (define-key python-mode-map (kbd "DEL") nil)

    (use-package anaconda-mode
      :defines (anaconda-mode-map anaconda-nav-mode-map)
      :functions (anaconda-mode-running-p)
      :init (add-to-hook 'python-mode-hook '(anaconda-mode eldoc-mode))
      :config
      (progn
        (bind :motion :map anaconda-mode-map     "gd"     'anaconda-mode-goto-definitions)
        (bind :normal :map anaconda-nav-mode-map [escape] 'anaconda-nav-quit)

        ;; Delete the window on escape or C-g
        (defadvice anaconda-mode-doc-buffer (after anaconda-doc-buffer-escape-to-close activate)
          (with-current-buffer (get-buffer "*anaconda-doc*")
            (local-set-key [escape] 'anaconda-nav-quit)
            (local-set-key [?\C-g] 'anaconda-nav-quit)))

        (after "emr"
          (emr-declare-command 'anaconda-mode-view-doc
            :title "view documentation"
            :modes 'python-mode
            :predicate (lambda () (and (anaconda-mode-running-p)
                                       (not (use-region-p))
                                       (not (sp-point-in-string-or-comment)))))
          (emr-declare-command 'anaconda-mode-goto-assignments
            :title "go to assignments"
            :modes 'python-mode
            :predicate (lambda () (and (anaconda-mode-running-p)
                                       (not (use-region-p))
                                       (not (sp-point-in-string-or-comment)))))

          (emr-declare-command 'anaconda-mode-goto-definitions
            :title "go to definition"
            :modes 'python-mode
            :predicate (lambda () (and (anaconda-mode-running-p)
                                       (not (use-region-p))
                                       (not (sp-point-in-string-or-comment)))))

          (emr-declare-command 'anaconda-mode-usages
            :title "show usages"
            :modes 'python-mode
            :predicate (lambda () (and (anaconda-mode-running-p)
                                       (not (use-region-p))
                                       (not (sp-point-in-string-or-comment))))))

        (after "company"
          (use-package company-anaconda
            :config
            (narf/add-company-backend python-mode (company-anaconda))))))

    (use-package nose
      :commands nose-mode
      :preface  (defvar nose-mode-map (make-sparse-keymap))
      :init     (associate-minor-mode "/test_.+\\.py\\'" 'nose-mode)
      :config
      (bind normal :map nose-mode-map
            :prefix leader
            "tr" 'nosetests-again
            "ta" 'nosetests-all
            "ts" 'nosetests-one
            "tv" 'nosetests-module
            "tA" 'nosetests-pdb-all
            "tO" 'nosetests-pdb-one
            "tV" 'nosetests-pdb-module))))


(provide 'init-python)
;;; init-python.el ends here
