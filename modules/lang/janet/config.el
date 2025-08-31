;;; lang/janet/config.el -*- lexical-binding: t; -*-

(use-package! janet-mode
  ;; HACK: Needed to override autoloaded `auto-mode-alist' and
  ;;   `interpreter-mode-alist' entries from this or `janet-ts-mode', so we can
  ;;   rely on `major-mode-remap-defaults' instead.
  :mode "\\.\\(jdn\\|janet\\)\\'"
  :interpreter "janet[0-9]*\\'"
  :config
  (after! dtrt-indent
    (add-to-list 'dtrt-indent-hook-mapping-list '(janet-mode janet janet-indent)))

  ;; HACK: janet-mode calls `janet--set-indentation' each time it's activated,
  ;;   making its (global) side-effects unnecessarily difficult to change, so I
  ;;   disable it and call it manually (and once).
  ;; REVIEW: PR this upstream?
  (defadvice! +janet--inhibit-indent-init-a (fn &rest args)
    :around #'janet-mode
    (letf! ((#'janet--set-indentation #'ignore))
      (apply fn args)))

  (janet--set-indentation)
  (dolist (sym '(with label catseq eachp eachk))
    (put sym 'janet-indent-function 'defun)))


(use-package! janet-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'janet-mode 'janet-ts-mode
    `((janet-simple :url "https://github.com/sogaiu/tree-sitter-janet-simple"
                    :cc ,(if (featurep :system 'windows) "gcc.exe")))))
