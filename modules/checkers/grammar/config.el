;;; checkers/grammar/config.el -*- lexical-binding: t; -*-

(use-package! langtool
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :init (setq langtool-default-language "en-US")
  :config
  (unless langtool-language-tool-jar
    (setq langtool-language-tool-jar
          (cond (IS-MAC
                 (locate-file "libexec/languagetool-commandline.jar"
                              (doom-files-in "/usr/local/Cellar/languagetool"
                                             :type 'dirs
                                             :depth 2)))
                (IS-LINUX
                 "/usr/share/java/languagetool/languagetool-commandline.jar")))))


;; Detects weasel words, passive voice and duplicates. Proselint would be a
;; better choice.
(use-package! writegood-mode
  :hook (org-mode markdown-mode rst-mode asciidoc-mode latex-mode)
  :config
  (map! :localleader
        :map writegood-mode-map
        "g" #'writegood-grade-level
        "r" #'writegood-reading-ease))
