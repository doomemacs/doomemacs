;;; lang/plantuml/config.el -*- lexical-binding: t; -*-

(use-package! plantuml-mode
  :commands plantuml-download-jar
  :init
  (setq plantuml-jar-path (concat doom-etc-dir "plantuml.jar")
        org-plantuml-jar-path plantuml-jar-path)
  :config
  (set-popup-rule! "^\\*PLANTUML" :size 0.4 :select nil :ttl 0)

  (setq plantuml-default-exec-mode
        (cond ((executable-find "plantuml") 'executable)
              ((file-exists-p plantuml-jar-path) 'jar)
              (plantuml-default-exec-mode))))


(use-package! flycheck-plantuml
  :when (featurep! :checkers syntax)
  :after plantuml-mode
  :config (flycheck-plantuml-setup))


(after! ob-plantuml
  ;; HACK We force ob-plantuml to use `plantuml-mode''s building mechanism,
  ;; which is more sophisticated.
  (advice-add #'org-babel-execute:plantuml
              :override #'+plantuml-org-babel-execute:plantuml-a)
  (add-to-list 'org-babel-default-header-args:plantuml
               '(:cmdline . "-charset utf-8")))
