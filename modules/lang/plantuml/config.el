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


(after! ob-plantuml
  (defadvice! +plantuml--fix-atstart-in-org-src-blocks-a (args)
    "Fix error when executing plantuml src blocks in org-mode for code that
begins with '@'. This character needs to be escaped with a backslash or comma
for the block to execute correctly, so we do it automatically."
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
