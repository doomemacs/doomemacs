;;; lang/web/config.el

(load! +html)
(load! +css)


(def-package! web-beautify
  :commands (web-beautify-html web-beautify-css)
  :init
  (map! (:map* (css-mode-map scss-mode-map less-css-mode-map)
          :n "gQ" 'web-beautify-css)
        (:map* web-mode-map
          :n "gQ" 'web-beautify-html)))


(def-package! emmet-mode
  :commands emmet-mode
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :init
  (add-hook! (css-mode web-mode html-mode haml-mode nxml-mode rjsx-mode)
    'emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t)
  (map! :map emmet-mode-keymap
        :v "M-e" 'emmet-wrap-with-markup
        :i "M-e" 'emmet-expand-yas
        :i "M-E" 'emmet-expand-line))


;;
;; Frameworks
;;

(def-project-mode! +web-bower-mode
  :files "bower.json")

(def-project-mode! +web-angularjs-mode
  :modes (+javascript-npm-mode +web-bower-mode)
  :when
  (let* ((project-root (doom-project-root))
         (bower (and (bound-and-true-p +web-bower-mode)
                     (+web-bower-conf project-root)))
         (npm   (and (bound-and-true-p +javascript-npm-mode)
                     (+javascript-npm-conf project-root))))
    (assq 'angular (append (cdr (assq 'dependencies bower))
                           (cdr (assq 'dependencies npm))
                           (cdr (assq 'devDependencies bower))
                           (cdr (assq 'devDependencies npm))))))

(def-project-mode! +web-jekyll-mode
  :modes (web-mode js-mode coffee-mode css-mode haml-mode pug-mode)
  :files (and "config.yml" (or "_layouts/" "_posts/"))
  :init
  (add-hook! '+web-jekyll-mode-hook
    (when (eq major-mode 'web-mode)
      (web-mode-set-engine "django"))))

(def-project-mode! +web-wordpress-mode
  :modes (php-mode web-mode css-mode haml-mode pug-mode)
  :files (or "wp-config.php" "wp-config-sample.php"))

(def-project-mode! +web-react-mode
  :modes (+javascript-npm-mode)
  :when (when-let (npm (+javascript-npm-conf))
          (and (assq 'react (append (cdr (assq 'dependencies npm))
                                    (cdr (assq 'devDependencies npm)))))))
