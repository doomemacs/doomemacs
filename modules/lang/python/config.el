;;; lang/python/config.el

(def-package! python
  :commands python-mode
  :init
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--deep-reload"
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

  (add-hook 'python-mode-hook 'flycheck-mode)

  :config
  (set! :repl 'python-mode '+python/repl)
  (define-key python-mode-map (kbd "DEL") nil)) ; interferes with smartparens


(def-package! anaconda-mode
  :after python
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode)
  (setq anaconda-mode-installation-directory (concat doom-cache-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t)

  :config
  (set! :company-backend 'python-mode '(company-anaconda))

  (map! :map anaconda-mode-map :m "gd" 'anaconda-mode-find-definitions)

  (advice-add 'anaconda-mode-doc-buffer :after 'doom*anaconda-mode-doc-buffer))

(def-package! company-anaconda
  :after anaconda-mode
  :config
  (map! :map python-mode-map
        :localleader
        :prefix "r"
        :nv "fd" 'anaconda-mode-show-doc
        :nv "fa" 'anaconda-mode-find-assignments
        :nv "fa" 'anaconda-mode-find-definitions
        :nv "ff" 'anaconda-mode-find-file
        :nv "u"  'anaconda-mode-find-references))


(def-package! pip-requirements
  :mode ("/requirements.txt$" . pip-requirements-mode))


(def-package! nose
  :commands nose-mode
  :preface
  (defvar nose-mode-map (make-sparse-keymap))
  :init
  (associate! nose-mode :match "/test_.+\\.py$" :in (python-mode))
  :config
  (set! :popup "*nosetests*" :size 0.4 :noselect t)
  (set! :yas-minor-mode 'nose-mode)
  (map! :map nose-mode-map
        :localleader
        :n "tr" 'nosetests-again
        :n "ta" 'nosetests-all
        :n "ts" 'nosetests-one
        :n "tv" 'nosetests-module
        :n "tA" 'nosetests-pdb-all
        :n "tO" 'nosetests-pdb-one
        :n "tV" 'nosetests-pdb-module))

