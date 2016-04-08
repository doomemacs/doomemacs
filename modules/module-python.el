;;; module-python.el

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :commands python-mode
  :init
  (setq-default
   python-environment-directory narf-temp-dir
   python-shell-interpreter "ipython"
   python-shell-interpreter-args "--deep-reload"
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

  (define-docset! python-mode "py,py3,python")
  (define-env-command! python-mode "python --version 2>&1 | cut -d' ' -f2")
  (define-repl! python-mode narf/inf-python)
  (add-hook! python-mode '(emr-initialize narf|flycheck-enable-maybe))

  :config
  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens

  (use-package anaconda-mode
    :init
    (add-hook! python-mode '(anaconda-mode anaconda-eldoc-mode eldoc-mode))
    (setq anaconda-mode-installation-directory (concat narf-temp-dir "/anaconda/")
          anaconda-mode-eldoc-as-single-line t)

    :config
    (map! :map anaconda-mode-map     :m "gd"     'anaconda-mode-goto-definitions)
    (map! :map anaconda-nav-mode-map :n [escape] 'anaconda-nav-quit)

    (advice-add 'anaconda-mode-doc-buffer :after 'narf*anaconda-mode-doc-buffer)

    (require 'company-anaconda)
    (define-company-backend! python-mode (anaconda))
    (after! emr
      (mapc (lambda (x)
              (let ((command-name (car x))
                    (title (cadr x))
                    (region-p (caddr x))
                    predicate)
                (setq predicate (lambda () (and (anaconda-mode-running-p)
                                           (not (use-region-p))
                                           (not (sp-point-in-string-or-comment)))))
                (emr-declare-command (intern (format "anaconda-mode-%s" (symbol-name command-name)))
                                     :title title :modes 'python-mode :predicate predicate)))
            '((show-doc          "view documentation" t)
              (find-assignments  "find assignments"  t)
              (find-definitions  "find definitions"  t)
              (find-file         "find assignments"  t)
              (find-references   "show usages"  nil))))))

(use-package nose
  :commands nose-mode
  :preface (defvar nose-mode-map (make-sparse-keymap))
  :init (associate! nose-mode :match "/test_.+\\.py\\'")
  :config
  (map! :map nose-mode-map
        (:localleader
          :n "tr" 'nosetests-again
          :n "ta" 'nosetests-all
          :n "ts" 'nosetests-one
          :n "tv" 'nosetests-module
          :n "tA" 'nosetests-pdb-all
          :n "tO" 'nosetests-pdb-one
          :n "tV" 'nosetests-pdb-module)))

(provide 'module-python)
;;; module-python.el ends here
