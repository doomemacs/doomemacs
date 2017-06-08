;;; lang/python/config.el -*- lexical-binding: t; -*-

(def-package! python
  :commands python-mode
  :init
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "python")

  :config
  (add-hook! 'python-mode-hook #'(flycheck-mode highlight-numbers-mode))

  (set! :repl 'python-mode #'+python/repl)
  (set! :electric 'python-mode :chars '(?:))
  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens

  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt --no-color-info"
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-setup-code
          "from IPython.core.completerlib import module_completion"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

  (sp-with-modes 'python-mode
    (sp-local-pair "'" nil :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-same-p))))


(def-package! anaconda-mode
  :after python
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'anaconda-mode-hook #'anaconda-eldoc-mode)
  (setq anaconda-mode-installation-directory (concat doom-etc-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t)

  :config
  (set! :popup "*anaconda-mode*" :size 10 :noselect t :autoclose t :autokill t)

  (map! :map anaconda-mode-map :m "gd" #'anaconda-mode-find-definitions)

  (advice-add #'anaconda-mode-doc-buffer :after #'doom*anaconda-mode-doc-buffer))


(def-package! company-anaconda
  :when (featurep! :completion company)
  :after anaconda-mode
  :config
  (set! :company-backend 'python-mode '(company-anaconda))
  (map! :map python-mode-map
        :m "gd" #'anaconda-mode-find-definitions
        :m "gD" #'anaconda-mode-find-references
        :localleader
        :prefix "f"
        :nv "d" #'anaconda-mode-find-definitions
        :nv "h" #'anaconda-mode-show-doc
        :nv "a" #'anaconda-mode-find-assignments
        :nv "f" #'anaconda-mode-find-file
        :nv "u" #'anaconda-mode-find-references))


(def-package! pip-requirements
  :mode ("/requirements.txt$" . pip-requirements-mode))


(def-package! nose
  :commands nose-mode
  :preface
  (defvar nose-mode-map (make-sparse-keymap))
  :init
  (associate! nose-mode :match "/test_.+\\.py$" :modes (python-mode))
  :config
  (set! :popup "*nosetests*" :size 0.4 :noselect t)
  (set! :yas-minor-mode 'nose-mode)
  (map! :map nose-mode-map
        :localleader
        :prefix "t"
        :n "r" #'nosetests-again
        :n "a" #'nosetests-all
        :n "s" #'nosetests-one
        :n "v" #'nosetests-module
        :n "A" #'nosetests-pdb-all
        :n "O" #'nosetests-pdb-one
        :n "V" #'nosetests-pdb-module))

