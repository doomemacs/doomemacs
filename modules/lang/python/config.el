;;; lang/python/config.el -*- lexical-binding: t; -*-

(defvar +python-pyenv-root nil
  "The path to pyenv's root directory. This is automatically set when `python'
is loaded.")

(defvar +python-pyenv-versions nil
  "Available versions of python in pyenv.")

(defvar +python-conda-home '("~/.anaconda3" "/usr/bin/anaconda3" "~/.anaconda")
  "A list of host pattern and corresponding anaconda home.")

(defvar +python/set-conda-home--history nil)

(defvar-local +python-current-version nil
  "The currently active pyenv version.")


;;
;; Plugins
;;

(def-package! python
  :defer t
  :init
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "python")
  :config
  (add-hook! 'python-mode-hook #'(flycheck-mode highlight-numbers-mode))

  (set! :env "PYTHONPATH" "PYENV_ROOT")
  (set! :company-backend 'python-mode '(company-anaconda))
  (set! :electric 'python-mode :chars '(?:))
  (set! :repl 'python-mode #'+python/repl)

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

  ;; Version management with pyenv
  (defun +python|add-version-to-modeline ()
    "Add version string to the major mode in the modeline."
    (setq mode-name
          (if +python-current-version
              (format "Python %s" +python-current-version)
            "Python")))
  (add-hook 'python-mode-hook #'+python|add-version-to-modeline)

  (if (not (executable-find "pyenv"))
      (setq +python-current-version (string-trim (shell-command-to-string "python --version 2>&1 | cut -d' ' -f2")))
    (setq +python-pyenv-root     (string-trim (shell-command-to-string "pyenv root"))
          +python-pyenv-versions (split-string (shell-command-to-string "pyenv versions --bare") "\n" t))

    (defun +python|detect-pyenv-version ()
      "Detect the pyenv version for the current project and set the relevant
environment variables."
      (when-let* ((version-str (shell-command-to-string "python --version 2>&1 | cut -d' ' -f2")))
        (setq version-str (string-trim version-str)
              +python-current-version version-str)
        (let ((pyenv-current-path (concat +python-pyenv-root "/versions/" version-str)))
          (when (file-directory-p pyenv-current-path)
            (setq pythonic-environment pyenv-current-path)))
        (when (member version-str +python-pyenv-versions)
          (setenv "PYENV_VERSION" version-str))))
    (add-hook 'python-mode-hook #'+python|detect-pyenv-version))

  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens
  (sp-with-modes 'python-mode
    (sp-local-pair "'" nil :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-same-p))))

(def-package! conda
  :when (featurep! +conda)
  :after (python)
  :config
  (advice-add 'anaconda-mode-bootstrap :override #'*anaconda-mode-bootstrap)
  (conda-env-autoactivate-mode -1)
  ;; (add-hook 'python-mode-hook #'conda-env-activate-for-buffer)
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  ;; Version management with conda
  (add-hook 'conda-postactivate-hook #'+python|add-version-to-modeline)
  (add-hook 'conda-postdeactivate-hook #'+python|add-version-to-modeline))

(def-package! lpy
  :when (featurep! +lpy)
  :hook ((python-mode . lpy-mode))
  :config
  (require 'le-python)
  (require 'zoutline)
  (define-minor-mode lpy-mode "Minor mode for navigating Python code similarly to LISP."
    :keymap lpy-mode-map
    :lighter " LPY"
    (if lpy-mode
        (progn
          (setq lispy-outline-header "# ")
          (setq-local outline-regexp "# \\*+")
          (setq-local lispy-outline (concat "^" outline-regexp))
          (setq-local outline-heading-end-regexp "\n")
          (setq-local outline-level 'lpy-outline-level)
          (setq-local fill-paragraph-function 'lpy-fill-paragraph)
          (setq-local fill-forward-paragraph-function 'lpy-fill-forward-paragraph-function)
          (setq-local completion-at-point-functions '(lispy-python-completion-at-point t))
          ;; (setq-local forward-sexp-function 'lpy-forward-sexp-function)
          )
      (setq-local forward-sexp-function nil)))
  (map! :map lpy-mode-map
        "n" nil
        :i "C-p" #'previous-line
        :i "C-n" #'next-line)
  (advice-add 'lispy--python-proc :override #'*lispy--python-proc)
  (advice-add 'lispy-short-process-name :override #'*lispy-short-process-name)
  (advice-add 'lispy-set-python-process-action :override #'*lispy-set-python-process-action))

(def-package! anaconda-mode
  :hook python-mode
  :init
  (setq anaconda-mode-installation-directory (concat doom-etc-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t)
  :config
  (add-hook 'anaconda-mode-hook #'anaconda-eldoc-mode)
  (set! :popup "^\\*anaconda-mode" nil '((select)))
  (set! :lookup 'python-mode
    :definition #'anaconda-mode-find-definitions
    :references #'anaconda-mode-find-references
    :documentation #'anaconda-mode-show-doc)
  (advice-add #'anaconda-mode-doc-buffer :after #'doom*anaconda-mode-doc-buffer)

  (defun +python|auto-kill-anaconda-processes ()
    "Kill anaconda processes if this buffer is the last python buffer."
    (when (and (eq major-mode 'python-mode)
               (not (delq (current-buffer)
                          (doom-buffers-in-mode 'python-mode (buffer-list)))))
      (anaconda-mode-stop)))
  (add-hook! 'python-mode-hook
    (add-hook 'kill-buffer-hook #'+python|auto-kill-anaconda-processes nil t)))


(def-package! company-anaconda
  :when (featurep! :completion company)
  :after anaconda-mode
  :config
  (map! :map anaconda-mode-map
        :localleader
        :prefix "f"
        :nv "d" #'anaconda-mode-find-definitions
        :nv "h" #'anaconda-mode-show-doc
        :nv "a" #'anaconda-mode-find-assignments
        :nv "f" #'anaconda-mode-find-file
        :nv "u" #'anaconda-mode-find-references))


(def-package! nose
  :commands nose-mode
  :preface
  (defvar nose-mode-map (make-sparse-keymap))
  :init
  (associate! nose-mode :match "/test_.+\\.py$" :modes (python-mode))
  :config
  (set! :popup "^\\*nosetests" '((size . 0.4)) '((select)))
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

