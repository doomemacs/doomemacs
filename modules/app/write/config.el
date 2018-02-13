;;; app/write/config.el -*- lexical-binding: t; -*-

(when (featurep! +langtool)
  (defvar +langtool-default-lang "en-US"
    "default language for langtool")
  (defvar +langtool-mother-tongue nil
    "mother tongue of user")
  (defvar +langtool-jar-path "/usr/local/Cellar/languagetool/4.0/libexec/languagetool-commandline.jar"
    "TODO")
  (def-package! langtool
    :commands (langtool-check
               langtool-check-done
               langtool-switch-default-language
               langtool-show-message-at-point
               langtool-correct-buffer)
    :init
    (setq langtool-default-language +langtool-default-lang
          langtool-mother-tongue +langtool-mother-tongue
          langtool-language-tool-jar +langtool-jar-path)))
(when (featurep! +wordnut)
  (def-package! wordnut
  :commands (wordnut-search
             wordnut-lookup-current-word)))
(when (featurep! +synosaurus)
  (def-package! synosaurus
  :commands (synosaurus-mode
             synosaurus-lookup
             synosaurus-choose-and-replace)
  :init
  (require 'synosaurus-wordnet)
  :config
  (setq synosaurus-choose-method 'default)))

(def-package! mixed-pitch
  :config
  (setq mixed-pitch-fixed-pitch-faces
   (append mixed-pitch-fixed-pitch-faces
           '(org-todo-keyword-todo
             org-todo-keyword-habt
             org-todo-keyword-done
             org-todo-keyword-wait
             org-todo-keyword-kill
             org-todo-keyword-outd
             org-special-keyword
             org-date
             org-property-value
             org-special-keyword
             org-property-value
             org-ref-cite-face
             org-tag
             font-lock-comment-face))))
