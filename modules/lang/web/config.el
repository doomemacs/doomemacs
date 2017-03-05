;;; lang/web/config.el

(load! +html)
(load! +css)


;;
;; Frameworks
;;

(def-project-mode! +web-bower-mode
  :files "bower.json")

(def-project-mode! +web-angularjs-mode
  :modes (+javascript-npm-mode +web-bower-mode)
  :when
  (and (or (bound-and-true-p +web-bower-mode)
           (bound-and-true-p +javascript-npm-mode))
       (let* ((project-root (doom-project-root))
              (bower (and +web-bower-mode (+web-bower-conf project-root)))
              (npm   (and +javascript-npm-mode (+javascript-npm-conf project-root))))
         (assq 'angular (append (cdr (assq 'dependencies bower))
                                (cdr (assq 'dependencies npm))
                                (cdr (assq 'devDependencies bower))
                                (cdr (assq 'devDependencies npm)))))))

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
