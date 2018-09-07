;;; app/write/config.el -*- lexical-binding: t; -*-

(defvar +write-text-scale nil
  "What to scale the text up to in `+write-mode'. Uses `text-scale-set'.")

(defvar +write-line-spacing nil
  "What to set `line-spacing' in `+write-mode'.")

(defun +write|init-line-numbers ()
  (display-line-numbers-mode (if +write-mode +1 -1)))

(defun +write|init-mixed-pitch ()
  (mixed-pitch-mode (if +write-mode +1 -1)))

(defun +write|init-visual-fill-column ()
  (visual-fill-column-mode (if +write-mode +1 -1)))

(add-hook! '+write-mode-hook
  #'(flyspell-mode
     visual-line-mode
     +write|init-mixed-pitch
     +write|init-visual-fill-column
     +write|init-line-numbers
     +write|init-org-mode))


;;
;; Packages

(def-package! langtool
  :when (featurep! +langtool)
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
                                             :depth 1)))
                (IS-LINUX
                 "/usr/share/java/languagetool/languagetool-commandline.jar")))))


;; `synosaurus'
(setq synosaurus-choose-method 'default)


;; `mixed-pitch'
(after! mixed-pitch
  (setq mixed-pitch-fixed-pitch-faces
        (append mixed-pitch-fixed-pitch-faces
                '(org-todo-keyword-todo
                  org-todo-keyword-habt
                  org-todo-keyword-done
                  org-todo-keyword-wait
                  org-todo-keyword-kill
                  org-todo-keyword-outd
                  org-todo
                  line-number
                  line-number-current-line
                  org-special-keyword
                  org-date
                  org-property-value
                  org-special-keyword
                  org-property-value
                  org-ref-cite-face
                  org-tag
                  font-lock-comment-face))))
