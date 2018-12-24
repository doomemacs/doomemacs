;;; lang/python/config.el -*- lexical-binding: t; -*-

(defconst +python-mode-line-indicator '("" +python--version)
  "Format for the python version/env indicator in the mode-line.")

(defvar +python-ipython-repl-args "-i --simple-prompt --no-color-info"
  "CLI arguments to initialize ipython with when `+python/open-ipython-repl' is
called.")

(defvar +python-jupyter-repl-args "--simple-prompt"
  "CLI arguments to initialize 'jupiter console %s' with when
`+python/open-ipython-repl' is called.")

(defvar-local +python--version nil
  "The python version in the current buffer.")


;;
;; Packages

(def-package! python
  :defer t
  :init
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil)
  :config
  (set-env! "PYTHONPATH" "PYENV_ROOT" "ANACONDA_HOME")
  (set-electric! 'python-mode :chars '(?:))
  (set-repl-handler! 'python-mode #'+python/repl)

  (set-pretty-symbols! 'python-mode
    ;; Functional
    :def "def"
    :lambda "lambda"
    ;; Types
    :null "None"
    :true "True" :false "False"
    :int "int" :str "str"
    :float "float"
    :bool "bool"
    :tuple "tuple"
    ;; Flow
    :not "not"
    :in "in" :not-in "not in"
    :and "and" :or "or"
    :for "for"
    :return "return" :yield "yield")

  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens
  (sp-local-pair 'python-mode "'" nil
                 :unless '(sp-point-before-word-p
                           sp-point-after-word-p
                           sp-point-before-same-p))

  (setq-hook! 'python-mode-hook tab-width python-indent-offset)

  ;; Add python/pipenv version string to the major mode in the modeline
  (defun +python|adjust-mode-line ()
    (setq mode-name +python-mode-line-indicator))
  (add-hook 'python-mode-hook #'+python|adjust-mode-line)

  (add-hook 'python-mode-hook #'+python|update-version))


(def-package! anaconda-mode
  :hook python-mode
  :init
  (setq anaconda-mode-installation-directory (concat doom-etc-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t)
  :config
  (add-hook 'anaconda-mode-hook #'anaconda-eldoc-mode)
  (set-company-backend! 'anaconda-mode '(company-anaconda))
  (set-lookup-handlers! 'anaconda-mode
    :definition #'anaconda-mode-find-definitions
    :references #'anaconda-mode-find-references
    :documentation #'anaconda-mode-show-doc)
  (set-popup-rule! "^\\*anaconda-mode" :select nil)

  (defun +python|auto-kill-anaconda-processes ()
    "Kill anaconda processes if this buffer is the last python buffer."
    (when (and (eq major-mode 'python-mode)
               (not (delq (current-buffer)
                          (doom-buffers-in-mode 'python-mode (buffer-list)))))
      (anaconda-mode-stop)))
  (add-hook! 'python-mode-hook
    (add-hook 'kill-buffer-hook #'+python|auto-kill-anaconda-processes nil t))

  (when (featurep 'evil)
    (add-hook 'anaconda-mode-hook #'evil-normalize-keymaps))
  (map! :localleader
        :map anaconda-mode-map
        :prefix "f"
        "d" #'anaconda-mode-find-definitions
        "h" #'anaconda-mode-show-doc
        "a" #'anaconda-mode-find-assignments
        "f" #'anaconda-mode-find-file
        "u" #'anaconda-mode-find-references))


(def-package! nose
  :commands nose-mode
  :preface (defvar nose-mode-map (make-sparse-keymap))
  :init (associate! nose-mode :match "/test_.+\\.py$" :modes (python-mode))
  :config
  (set-popup-rule! "^\\*nosetests" :size 0.4 :select nil)
  (set-yas-minor-mode! 'nose-mode)
  (when (featurep 'evil)
    (add-hook 'nose-mode-hook #'evil-normalize-keymaps))

  (map! :localleader
        :map nose-mode-map
        :prefix "t"
        "r" #'nosetests-again
        "a" #'nosetests-all
        "s" #'nosetests-one
        "v" #'nosetests-module
        "A" #'nosetests-pdb-all
        "O" #'nosetests-pdb-one
        "V" #'nosetests-pdb-module))


(def-package! python-pytest
  :defer t
  :init
  (map! :after python
        :localleader
        :map python-mode-map
        :prefix "t"
        "f" #'python-pytest-file
        "k" #'python-pytest-file-dwim
        "m" #'python-pytest-repeat
        "p" #'python-pytest-popup))


;;
;; Environment management

(def-package! pipenv
  :commands pipenv-project-p
  :hook (python-mode . pipenv-mode)
  :init (setq pipenv-with-projectile nil)
  :config
  (set-eval-handler! 'python-mode
    '((:command . (lambda () python-shell-interpreter))
      (:exec (lambda ()
               (if-let* ((bin (executable-find "pipenv"))
                         (_ (pipenv-project-p)))
                   (format "PIPENV_MAX_DEPTH=9999 %s run %%c %%o %%s %%a" bin)
                 "%c %o %s %a")))
      (:description . "Run Python script")))

  (advice-add #'pipenv-activate   :after-while #'+python|update-version-in-all-buffers)
  (advice-add #'pipenv-deactivate :after-while #'+python|update-version-in-all-buffers))


(def-package! pyenv-mode
  :when (featurep! +pyenv)
  :after python
  :config
  (pyenv-mode +1)
  (when (executable-find "pyenv")
    (add-to-list 'exec-path (expand-file-name "shims" (or (getenv "PYENV_ROOT") "~/.pyenv"))))
  (advice-add #'pyenv-mode-set :after #'+python|update-version-in-all-buffers)
  (advice-add #'pyenv-mode-unset :after #'+python|update-version-in-all-buffers))


(def-package! pyvenv
  :when (featurep! +pyvenv)
  :after python
  :config
  (defun +python-current-pyvenv () pyvenv-virtual-env-name)
  (add-hook 'pyvenv-post-activate-hooks #'+python|update-version-in-all-buffers)
  (add-hook 'pyvenv-post-deactivate-hooks #'+python|update-version-in-all-buffers)
  (add-to-list '+python-mode-line-indicator
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name))
               'append))


(def-package! conda
  :when (featurep! +conda)
  :after python
  :config
  ;; The location of your anaconda home will be guessed from the following:
  ;;
  ;; + `conda-anaconda-home's default value:
  ;;   + ANACONDA_HOME
  ;;   + ~/.anaconda3
  ;; + ~/.anaconda
  ;; + ~/.miniconda
  ;; + ~/usr/bin/anaconda3
  ;; + ~/usr/local/anaconda3
  ;; + ~/usr/local/miniconda3
  ;;
  ;; If none of these work for you, you must set `conda-anaconda-home'
  ;; explicitly. Once set, run M-x `conda-env-activate' to switch between
  ;; environments
  (unless (cl-loop for dir in (list conda-anaconda-home
                                    "~/.anaconda"
                                    "~/.miniconda"
                                    "~/.miniconda3"
                                    "/usr/bin/anaconda3"
                                    "/usr/local/anaconda3"
                                    "/usr/local/miniconda3")
                   if (file-directory-p dir)
                   return (setq conda-anaconda-home dir
                                conda-env-home-directory dir))
    (message "Cannot find Anaconda installation"))

  ;; integration with term/eshell
  (conda-env-initialize-interactive-shells)
  (after! eshell (conda-env-initialize-eshell))

  (add-hook 'conda-postactivate-hook #'+python|update-version-in-all-buffers)
  (add-hook 'conda-postdeactivate-hook #'+python|update-version-in-all-buffers)
  (add-to-list '+python-mode-line-indicator
               '(conda-env-current-name (" conda:" conda-env-current-name))
               'append))
