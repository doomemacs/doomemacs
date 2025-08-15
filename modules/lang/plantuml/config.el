;;; lang/plantuml/config.el -*- lexical-binding: t; -*-

(use-package! plantuml-mode
  :commands plantuml-download-jar
  :init
  (setq plantuml-jar-path (concat doom-data-dir "plantuml.jar"))
  :config
  (set-popup-rule! "^\\*PLANTUML" :size 0.4 :select nil :ttl 0)
  (setq plantuml-default-exec-mode
        (cond ((file-exists-p plantuml-jar-path) 'jar)
              ((executable-find plantuml-executable-path) 'executable)
              (plantuml-default-exec-mode)))
  ;; HACK: If plantuml-jar-path is missing at startup, then plantuml-mode will
  ;;   operate in another `plantuml-default-exec-mode'. After using
  ;;   plantuml-download-jar, we change it to `jar' just for this session.
  (defadvice! +plantuml--use-downloaded-jar-a (fn &rest args)
    "Configure plantuml-mode to use the downloaded jar for this session."
    :around #'plantuml-download-jar
    (let ((downloaded? (not (file-exists-p plantuml-jar-path))))
      (prog1 (apply fn args)
        (when (and downloaded? (file-exists-p plantuml-jar-path))
          (setq org-plantuml-jar-path plantuml-jar-path
                plantuml-default-exec-mode 'jar))))))


(use-package! flycheck-plantuml
  :when (modulep! :checkers syntax)
  :after plantuml-mode
  :config
  (flycheck-plantuml-setup)
  (when (eq plantuml-default-exec-mode 'executable)
    ;; Surprisingly, this works, even though flycheck-plantuml specifies -Djava.awt...
    (setq-default flycheck-plantuml-executable plantuml-executable-path)))


(after! ob-plantuml
  ;; The nested `after!' is needed to ensure `org-plantuml-jar-path's new
  ;; default without overwriting any user config.
  (after! plantuml-mode
    (when (equal org-plantuml-jar-path "")
      (setq org-plantuml-jar-path plantuml-jar-path)))

  ;; HACK: Force ob-plantuml to use `plantuml-mode''s building mechanism, which
  ;;   is more sophisticated.
  (advice-add #'org-babel-execute:plantuml
              :override #'+plantuml-org-babel-execute:plantuml-a)
  (add-to-list 'org-babel-default-header-args:plantuml
               '(:cmdline . "-charset utf-8")))
