(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (progn
    (add-hook 'python-mode-hook 'enable-tab-width-4)
    (add-hook 'python-mode-hook 'emr-initialize))
  :config
  (progn
    (setq-default python-indent-offset 4)
    (setq python-environment-directory my-tmp-dir)
    (setq python-shell-interpreter "ipython")
    ;; interferes with smartparens
    (define-key python-mode-map (kbd "DEL") nil)

    (use-package anaconda-mode
      :init
      (progn
        (add-hook 'python-mode-hook 'anaconda-mode)
        (add-hook 'python-mode-hook 'eldoc-mode)
        ;; (add-hook! 'anaconda-mode-hook
        ;;   (process-buffer (python-shell-get-or-create-process python-shell-interpreter t nil)))
        )
      :config
      (progn
        (bind 'motion anaconda-mode-map "gd" 'anaconda-mode-goto-definitions)
        (bind 'normal anaconda-nav-mode-map [escape] 'anaconda-nav-quit)

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
            :config (company--backend-on 'python-mode-hook 'company-anaconda)))))

    (use-package nose
      :commands nose-mode
      :init
      (progn
        ;; Reset nose keymap, we'll set new ones in my-keymaps.el
        (defvar nose-mode-map (make-sparse-keymap))
        (associate-minor-mode "/test_.+\\.py\\'" 'nose-mode))
      :config
      (bind 'normal nose-mode-map
            ",tr" 'nosetests-again
            ",ta" 'nosetests-all
            ",ts" 'nosetests-one
            ",tv" 'nosetests-module
            ",tA" 'nosetests-pdb-all
            ",tO" 'nosetests-pdb-one
            ",tV" 'nosetests-pdb-module))))


(provide 'init-python)
;;; init-python.el ends here
