;;; lang/odin/config.el -*- lexical-binding: t; -*-

(defcustom +odin-build-command "odin build . -debug"
  "Command used to build Odin projects.
Can be overridden per-project via .dir-locals.el."
  :safe #'stringp
  :type 'string
  :group '+odin)

(defcustom +odin-test-command "odin test ."
  "Command used to test Odin projects.
Can be overridden per-project via .dir-locals.el."
  :safe #'stringp
  :type 'string
  :group '+odin)


;;
;;; Packages

(defun +odin-common-config (mode)
  (set-formatter! 'odinfmt '("odinfmt") :modes (list mode))
  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append))
  (map! :map ,(intern (format "%s-map" mode))
        :localleader
        "b" #'+odin/build
        "t" #'+odin/test))


(use-package! odin-mode
  :defer t
  :config
  (+odin-common-config 'odin-mode))


(use-package! odin-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'odin-mode 'odin-ts-mode
    '((odin :url "https://github.com/tree-sitter-grammars/tree-sitter-odin"
            :rev "v1.3.0")))
  :config
  (+odin-common-config 'odin-ts-mode))
