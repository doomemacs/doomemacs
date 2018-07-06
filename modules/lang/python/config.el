;;; lang/python/config.el -*- lexical-binding: t; -*-

(defvar +python-pyenv-root nil
  "The path to pyenv's root directory. This is automatically set when `python'
is loaded.")

(defvar +python-pyenv-versions nil
  "Available versions of python in pyenv.")

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
  (add-hook 'python-mode-hook #'highlight-numbers-mode)

  (set-env! "PYTHONPATH" "PYENV_ROOT")
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
      (setq-default +python-current-version (string-trim (shell-command-to-string "python --version 2>&1 | cut -d' ' -f2")))
    (setq +python-pyenv-root     (string-trim (shell-command-to-string "pyenv root"))
          +python-pyenv-versions (split-string (shell-command-to-string "pyenv versions --bare") "\n" t))

    (defun +python|detect-pyenv-version ()
      "Detect the pyenv version for the current project and set the relevant
environment variables."
      (when-let* ((version-str (shell-command-to-string "PYENV_VERSION= python --version 2>&1 | cut -d' ' -f2")))
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


(def-package! anaconda-mode
  :after python
  :init
  (setq anaconda-mode-installation-directory (concat doom-etc-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t)
  :config
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'anaconda-mode-hook #'anaconda-eldoc-mode)
  (set-company-backend! 'python-mode '(company-anaconda))
  (set-popup-rule! "^\\*anaconda-mode" :select nil)
  (set-lookup-handlers! 'python-mode
    :definition #'anaconda-mode-find-definitions
    :references #'anaconda-mode-find-references
    :documentation #'anaconda-mode-show-doc)

  (defun +python|auto-kill-anaconda-processes ()
    "Kill anaconda processes if this buffer is the last python buffer."
    (when (and (eq major-mode 'python-mode)
               (not (delq (current-buffer)
                          (doom-buffers-in-mode 'python-mode (buffer-list)))))
      (anaconda-mode-stop)))
  (add-hook! 'python-mode-hook
    (add-hook 'kill-buffer-hook #'+python|auto-kill-anaconda-processes nil t))

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
  (set-popup-rule! "^\\*nosetests" :size 0.4 :select nil)
  (set-yas-minor-mode! 'nose-mode)
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


;;
;; Evil integration
;;

(when (featurep! :feature evil +everywhere)
  (add-hook! '(anaconda-mode-hook nose-mode-hook)
    #'evil-normalize-keymaps))
