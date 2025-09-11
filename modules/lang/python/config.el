;;; lang/python/config.el -*- lexical-binding: t; -*-

(defvar +python-ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info")
  "Command to initialize the ipython REPL for `+python/open-ipython-repl'.")

(defvar +python-jupyter-command '("jupyter" "console" "--simple-prompt")
  "Command to initialize the jupyter REPL for `+python/open-jupyter-repl'.")

(after! projectile
  (pushnew! projectile-project-root-files "pyproject.toml" "requirements.txt" "setup.py"))


;;
;;; Packages

(use-package! python
  :mode ("/\\(?:Pipfile\\|\\.?flake8\\)\\'" . conf-mode)
  :init
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil)

  (when (modulep! +tree-sitter)
    (set-tree-sitter! 'python-mode 'python-ts-mode
      `((python :url "https://github.com/tree-sitter/tree-sitter-python"
                :rev ,(if (< (treesit-library-abi-version) 15) "v0.23.6")
                :commit "bffb65a8cfe4e46290331dfef0dbf0ef3679de11"))))

  :config
  ;; HACK: `python-base-mode' (and `python-ts-mode') don't exist on pre-29
  ;;   versions of Emacs, Rather than litter this module with conditionals, I
  ;;   shim the keymap in.
  (unless (boundp 'python-base-mode-map)
    (defvaralias 'python-base-mode-map 'python-mode-map))

  (when (modulep! +lsp)
    (add-hook 'python-mode-local-vars-hook #'lsp! 'append)
    (add-hook 'python-ts-mode-local-vars-hook #'lsp! 'append)
    ;; Use "mspyls" in eglot if in PATH
    (when (executable-find "Microsoft.Python.LanguageServer")
      (set-eglot-client! '(python-mode python-ts-mode) '("Microsoft.Python.LanguageServer"))))

  (set-repl-handler! '(python-mode python-ts-mode) #'+python/open-repl
    :persist t
    :send-region #'python-shell-send-region
    :send-buffer #'python-shell-send-buffer)

  (set-docsets! '(python-mode python-ts-mode inferior-python-mode)
    "Python 3" "NumPy" "SciPy" "Pandas")

  (set-ligatures! '(python-mode python-ts-mode)
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

  ;; Stop the spam!
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems link the unversioned one to Python 2.
  (when (and (string= python-shell-interpreter "python") ; only if unmodified
             (executable-find "python3"))
    (setq python-shell-interpreter "python3"))

  (add-hook! '(python-mode-hook python-ts-mode-hook)
    (defun +python-use-correct-flycheck-executables-h ()
      "Use the correct Python executables for Flycheck."
      (let ((executable python-shell-interpreter))
        (save-excursion
          (goto-char (point-min))
          (save-match-data
            (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                      (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
              (setq executable (substring-no-properties (match-string 1))))))
        ;; Try to compile using the appropriate version of Python for
        ;; the file.
        (setq-local flycheck-python-pycompile-executable executable)
        ;; We might be running inside a virtualenv, in which case the
        ;; modules won't be available. But calling the executables
        ;; directly will work.
        (setq-local flycheck-python-pylint-executable "pylint")
        (setq-local flycheck-python-flake8-executable "flake8"))))

  ;; Affects pyenv and conda
  (when (modulep! :ui modeline)
    (advice-add #'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
    (advice-add #'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h))

  ;; HACK: `python-mode' doesn't update `tab-width' to reflect
  ;;   `python-indent-offset', causing issues anywhere `tab-width' is respected.
  (setq-hook! '(python-mode-hook python-ts-mode-hook) tab-width python-indent-offset))


(use-package! pyimport
  :defer t
  :init
  (map! :after python
        :map python-base-mode-map
        :localleader
        :prefix ("i" . "imports")
        :desc "Insert missing imports" "i" #'pyimport-insert-missing
        :desc "Remove unused imports"  "R" #'pyimport-remove-unused
        :desc "Optimize imports"       "o" #'+python/optimize-imports))


(use-package! py-isort
  :defer t
  :init
  (map! :after python
        :map python-base-mode-map
        :localleader
        (:prefix ("i" . "imports")
         :desc "Sort imports"      "s" #'py-isort-buffer
         :desc "Sort region"       "r" #'py-isort-region)))


(use-package! nose
  :commands nose-mode
  :preface (defvar nose-mode-map (make-sparse-keymap))
  :minor ("/test_.+\\.py$" . nose-mode)
  :config
  (set-popup-rule! "^\\*nosetests" :size 0.4 :select nil)
  (set-yas-minor-mode! 'nose-mode)
  (when (featurep 'evil)
    (add-hook 'nose-mode-hook #'evil-normalize-keymaps))

  (map! :localleader
        :map nose-mode-map
        :prefix ("t" . "test")
        "r" #'nosetests-again
        "a" #'nosetests-all
        "s" #'nosetests-one
        "v" #'nosetests-module
        "A" #'nosetests-pdb-all
        "O" #'nosetests-pdb-one
        "V" #'nosetests-pdb-module))


(use-package! python-pytest
  :commands python-pytest-dispatch
  :init
  (map! :after python
        :localleader
        :map python-base-mode-map
        :prefix ("t" . "test")
        "a" #'python-pytest
        "f" #'python-pytest-file-dwim
        "F" #'python-pytest-file
        "t" #'python-pytest-run-def-or-class-at-point-dwim
        "T" #'python-pytest-run-def-or-class-at-point
        "r" #'python-pytest-repeat
        "p" #'python-pytest-dispatch))


;;
;;; Environment management

(use-package! pipenv
  :commands pipenv-project-p
  :hook (python-mode . pipenv-mode)
  :init (setq pipenv-with-projectile nil)
  :config
  (set-eval-handler! '(python-mode python-ts-mode)
    '((:command . (lambda () python-shell-interpreter))
      (:exec (lambda ()
               (if-let* ((bin (executable-find "pipenv" t))
                         (_ (pipenv-project-p)))
                   (format "PIPENV_MAX_DEPTH=9999 %s run %%c %%o %%s %%a" bin)
                 "%c %o %s %a")))
      (:description . "Run Python script")))
  (map! :map python-base-mode-map
        :localleader
        :prefix ("e" . "pipenv")
        :desc "activate"    "a" #'pipenv-activate
        :desc "deactivate"  "d" #'pipenv-deactivate
        :desc "install"     "i" #'pipenv-install
        :desc "lock"        "l" #'pipenv-lock
        :desc "open module" "o" #'pipenv-open
        :desc "run"         "r" #'pipenv-run
        :desc "shell"       "s" #'pipenv-shell
        :desc "uninstall"   "u" #'pipenv-uninstall))


(use-package! pyvenv
  :after python
  :init
  (when (modulep! :ui modeline)
    (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
    (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h))
  :config
  (add-hook! '(python-mode-local-vars-hook python-ts-mode-local-vars-hook)
             #'pyvenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
               'append))



(use-package! pyenv-mode
  :when (modulep! +pyenv)
  :after python
  :config
  (when (executable-find "pyenv")
    (pyenv-mode +1)
    (add-to-list 'exec-path (expand-file-name "shims" (or (getenv "PYENV_ROOT") "~/.pyenv"))))
  (add-hook! '(python-mode-local-vars-hook python-ts-mode-local-vars-hook)
             #'+python-pyenv-mode-set-auto-h)
  (add-hook 'doom-switch-buffer-hook #'+python-pyenv-mode-set-auto-h))


(use-package! conda
  :when (modulep! +conda)
  :after python
  :config
  ;; integration with term/eshell
  (conda-env-initialize-interactive-shells)
  (add-hook 'eshell-load-hook #'conda-env-initialize-eshell)

  (add-to-list 'global-mode-string
               '(conda-env-current-name (" conda:" conda-env-current-name " "))
               'append))


(use-package! poetry
  :when (modulep! +poetry)
  :after python
  :hook (doom-first-buffer . poetry-tracking-mode)
  :init (setq poetry-tracking-strategy 'switch-buffer))


(use-package! cython-mode
  :when (modulep! +cython)
  :defer t
  :config
  (setq cython-default-compile-format "cython -a %s")
  (map! :map cython-mode-map
        :localleader
        :prefix "c"
        :desc "Cython compile buffer"    "c" #'cython-compile))


(use-package! flycheck-cython
  :when (modulep! +cython)
  :when (modulep! :checkers syntax -flymake)
  :after cython-mode)


(use-package! pip-requirements
  :defer t
  :config
  ;; HACK `pip-requirements-mode' performs a sudden HTTP request to
  ;;   https://pypi.org/simple, which causes unexpected hangs (see #5998). This
  ;;   advice defers this behavior until the first time completion is invoked.
  ;; REVIEW More sensible behavior should be PRed upstream.
  (defadvice! +python--init-completion-a (&rest args)
    "Call `pip-requirements-fetch-packages' first time completion is invoked."
    :before #'pip-requirements-complete-at-point
    (unless pip-packages (pip-requirements-fetch-packages)))
  (defadvice! +python--inhibit-pip-requirements-fetch-packages-a (fn &rest args)
    "No-op `pip-requirements-fetch-packages', which can be expensive."
    :around #'pip-requirements-mode
    (letf! ((#'pip-requirements-fetch-packages #'ignore))
      (apply fn args))))


;;
;;; LSP

(use-package! lsp-pyright
  :when (modulep! +lsp)
  :when (modulep! +pyright)
  :when (modulep! :tools lsp -eglot)
  :defer t)
