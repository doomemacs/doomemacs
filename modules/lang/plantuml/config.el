;;; lang/plantuml/config.el -*- lexical-binding: t; -*-

(use-package! plantuml-mode
  :commands plantuml-download-jar
  :init
  (setq plantuml-jar-path (concat doom-data-dir "plantuml.jar")
        org-plantuml-jar-path plantuml-jar-path)
  (let ((doom-jar-path (concat doom-data-dir "plantuml.jar"))
        (system-jar-path "/usr/share/plantuml/plantuml.jar"))
    (setq plantuml-jar-path
          (cond ((file-exists-p doom-jar-path) doom-jar-path)
                ((file-exists-p system-jar-path) system-jar-path)
                (t doom-jar-path)))
    (setq org-plantuml-jar-path plantuml-jar-path))

  :config
  (set-popup-rule! "^\\*PLANTUML" :size 0.4 :select nil :ttl 0)
  (advice-add 'plantuml-download-jar :around
              (lambda (orig-fun &rest args)
                (let ((plantuml-jar-path (concat doom-data-dir "plantuml.jar")))
                  (apply orig-fun args)
                  (let ((doom-jar-path (concat doom-data-dir "plantuml.jar"))
                        (system-jar-path "/usr/share/plantuml/plantuml.jar"))
                    (setq plantuml-jar-path
                          (cond ((file-exists-p doom-jar-path) doom-jar-path)
                                ((file-exists-p system-jar-path) system-jar-path)
                                (t doom-jar-path)))
                    (setq org-plantuml-jar-path plantuml-jar-path)))))

  (setq plantuml-default-exec-mode
        (cond ((file-exists-p plantuml-jar-path) 'jar)
              ((executable-find "plantuml") 'executable)
              (plantuml-default-exec-mode))))


(use-package! flycheck-plantuml
  :when (modulep! :checkers syntax)
  :after plantuml-mode
  :config
  (flycheck-plantuml-setup)
  (when (eq plantuml-default-exec-mode 'executable)
    ;; Surprisingly, this works, even though flycheck-plantuml specifies -Djava.awt...
    (setq-default flycheck-plantuml-executable plantuml-executable-path)))


(after! ob-plantuml
  ;; HACK Force ob-plantuml to use `plantuml-mode''s building mechanism, which
  ;;      is more sophisticated.
  (advice-add #'org-babel-execute:plantuml
              :override #'+plantuml-org-babel-execute:plantuml-a)
  (add-to-list 'org-babel-default-header-args:plantuml
               '(:cmdline . "-charset utf-8")))
