;;; checkers/grammar/config.el -*- lexical-binding: t; -*-

(use-package! langtool
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :init (setq langtool-default-language "en-US")
  :config
  (unless (or langtool-bin
              langtool-language-tool-jar
              langtool-java-classpath)
    (cond (IS-MAC
           (cond
            ;; is user using home brew?
            ((file-directory-p "/usr/local/Cellar/languagetool")
             (setq langtool-language-tool-jar
                   (locate-file "libexec/languagetool-commandline.jar"
                                (doom-files-in "/usr/local/Cellar/languagetool"
                                               :type 'dirs
                                               :depth 2))))
            ;; macports compatibility
            ((file-directory-p "/opt/local/share/java/LanguageTool")
             (setq langtool-java-classpath "/opt/local/share/java/LanguageTool/*"))))
          (IS-LINUX
           (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")))))


;; Detects weasel words, passive voice and duplicates. Proselint would be a
;; better choice.
(use-package! writegood-mode
  :hook (org-mode markdown-mode rst-mode asciidoc-mode latex-mode)
  :config
  (map! :localleader
        :map writegood-mode-map
        "g" #'writegood-grade-level
        "r" #'writegood-reading-ease))
