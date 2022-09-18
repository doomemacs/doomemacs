;;; lang/web/config.el -*- lexical-binding: t; -*-

(load! "+html")
(load! "+css")


(use-package! emmet-mode
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :hook (css-mode web-mode html-mode haml-mode nxml-mode rjsx-mode reason-mode)
  :config
  (when (require 'yasnippet nil t)
    (add-hook 'emmet-mode-hook #'yas-minor-mode-on))
  (setq emmet-move-cursor-between-quotes t)
  (setq-hook! 'rjsx-mode-hook emmet-expand-jsx-className? t)
  (map! :map emmet-mode-keymap
        :v [tab] #'emmet-wrap-with-markup
        [tab] #'+web/indent-or-yas-or-emmet-expand
        "M-E" #'emmet-expand-line))


;;
;;; Framework-based minor-modes

(def-project-mode! +web-jekyll-mode
  :modes '(web-mode js-mode coffee-mode css-mode haml-mode pug-mode)
  :files (and (or "_config.yml" "_config.toml")
              (or "_layouts/" "_posts/"))
  :on-enter
  (when (eq major-mode 'web-mode)
    (web-mode-set-engine "django")))

(def-project-mode! +web-django-mode
  :modes '(web-mode python-mode)
  :files ("manage.py")
  :on-enter
  (when (derived-mode-p 'web-mode)
    (web-mode-set-engine "django")))

(def-project-mode! +web-wordpress-mode
  :modes '(php-mode web-mode css-mode haml-mode pug-mode)
  :files (or "wp-config.php" "wp-config-sample.php"))

(when (modulep! :lang javascript)
  (def-project-mode! +web-angularjs-mode
    :modes '(+javascript-npm-mode)
    :when (+javascript-npm-dep-p '(angular @angular/core))
    :on-enter
    (when (derived-mode-p 'web-mode)
      (web-mode-set-engine "angular")))

  (def-project-mode! +web-react-mode
    :modes '(+javascript-npm-mode)
    :when (+javascript-npm-dep-p 'react))

  (def-project-mode! +web-phaser-mode
    :modes '(+javascript-npm-mode)
    :when (+javascript-npm-dep-p '(or phaser phaser-ce))))
