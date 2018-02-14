;;; app/write/config.el -*- lexical-binding: t; -*-

(defvar +write-text-scale nil
  "What to scale the text up to in `+write-mode'. Uses `text-scale-set'.")

(defvar +write-line-spacing nil
  "What to set `line-spacing' in `+write-mode'.")

(add-hook! '+write-mode-hook
  #'(flyspell-mode
     visual-fill-column-mode
     visual-line-mode
     mixed-pitch-mode
     doom|enable-line-numbers
     +write|init-org-mode))


;;
;; Plugins
;;

(when (featurep! +langtool)
  (def-package! langtool
    :commands (langtool-check
               langtool-check-done
               langtool-switch-default-language
               langtool-show-message-at-point
               langtool-correct-buffer)
    :init
    (setq langtool-default-language "en-US")
    :config
    (unless langtool-language-tool-jar
      (setq langtool-language-tool-jar
            (cond (IS-MAC
                   "/usr/local/Cellar/languagetool/4.0/libexec/languagetool-commandline.jar")
                  (IS-LINUX
                   "/usr/share/java/languagetool/languagetool-commandline.jar")))
      (unless (file-exists-p langtool-language-tool-jar)
        (warn! "Couldn't find languagetool-commandline.jar")))))


(when (featurep! +wordnut)
  (def-package! wordnut
    :commands (wordnut-search
               wordnut-lookup-current-word)))


(def-package! synosaurus
  :commands (synosaurus-mode
             synosaurus-lookup
             synosaurus-choose-and-replace)
  :config
  (setq synosaurus-choose-method 'default))

(def-package! synosaurus-wordnet
  :commands synosaurus-backend-wordnet)


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
