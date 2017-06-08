;;; lang/web/config.el -*- lexical-binding: t; -*-

(load! +html)
(load! +css)


(def-package! web-beautify
  :commands (web-beautify-html web-beautify-css)
  :init
  (map! (:map* (css-mode-map scss-mode-map less-css-mode-map)
          :n "gQ" #'web-beautify-css)
        (:map* web-mode-map
          :n "gQ" #'web-beautify-html)))


(def-package! emmet-mode
  :commands emmet-mode
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :init
  (add-hook! (css-mode web-mode html-mode haml-mode nxml-mode rjsx-mode)
    'emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t)
  (map! :map emmet-mode-keymap
        :v "M-e" #'emmet-wrap-with-markup
        :i "M-e" #'emmet-expand-yas
        :i "M-E" #'emmet-expand-line))


;;
;; Frameworks
;;

(def-project-mode! +web-angularjs-mode
  :modes (+javascript-npm-mode)
  :when (+javascript-npm-dep-p 'angular))

(def-project-mode! +web-jekyll-mode
  :modes (web-mode js-mode coffee-mode css-mode haml-mode pug-mode)
  :files (and "config.yml" (or "_layouts/" "_posts/"))
  :init
  (defun +web|init-jekyll-mode ()
    (when (eq major-mode 'web-mode)
      (web-mode-set-engine "django")))
  (add-hook '+web-jekyll-mode-hook #'+web|init-jekyll-mode))

(def-project-mode! +web-wordpress-mode
  :modes (php-mode web-mode css-mode haml-mode pug-mode)
  :files (or "wp-config.php" "wp-config-sample.php"))

(def-project-mode! +web-react-mode
  :modes (+javascript-npm-mode)
  :when (+javascript-npm-dep-p 'react))

(def-project-mode! +web-phaser-mode
  :modes (+javascript-npm-mode)
  :when (+javascript-npm-dep-p '(or phaser phaser-ce)))
