;;; lang/plantuml/config.el -*- lexical-binding: t; -*-

(use-package! plantuml-mode
  :defer t
  :init
  (setq plantuml-jar-path (concat doom-etc-dir "plantuml.jar")
        org-plantuml-jar-path plantuml-jar-path)
  :config
  (set-popup-rule! "^\\*PLANTUML" :size 0.4 :select nil :ttl 0))


(use-package! flycheck-plantuml
  :when (featurep! :tools flycheck)
  :after plantuml-mode
  :config (flycheck-plantuml-setup))

;;
;; 1. Add `:cmdline -charset utf-8' to org-src-block:plantuml
;;
;; 2. Fix `@start' prefix execute error
;; When `C-c C-c' is executed in org-src-block:plantuml, if the code starts with
;; `@', execution will go wrong. Must be preceded by `\' or `,' to execute
;; normally. This code is automatically added `\' before `@start' when `C-c C-c'
;; is executed, so that the execution can be carried out normally.
;;
(after! ob-plantuml
  (defadvice! +plantuml--fix-atstart-in-org-src-blocks-a (args)
    :filter-args #'org-babel-execute:plantuml
    (cl-destructuring-bind (body params) args
      (let* ((origin-body body)
             (fix-body
              (replace-regexp-in-string
               "^[[:blank:]\n]*\\(@start\\)"
               "\\\\\\1"
               origin-body)))
        (list fix-body params))))
  (add-to-list 'org-babel-default-header-args:plantuml
               '(:cmdline . "-charset utf-8")))
