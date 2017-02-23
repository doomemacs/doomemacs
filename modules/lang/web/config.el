;;; lang/web/config.el

(load! +html)
(load! +css)


;;
;; TODO Frameworks
;;

;; (defvar bower-conf (make-hash-table :test 'equal))
;; (def-project! bower "bower"
;;   :modes (web-mode js-mode coffee-mode css-mode sass-mode pug-mode)
;;   :files ("bower.json")
;;   :when
;;   (lambda (&rest _)
;;     (let* ((project-path (doom-project-root))
;;            (hash (gethash project-path bower-conf))
;;            (package-file (f-expand "bower.json" project-path))
;;            deps)
;;       (awhen (and (not hash) (f-exists? package-file)
;;                   (ignore-errors (json-read-file package-file)))
;;         (puthash project-path it bower-conf)))
;;     t))

;; (def-project! angularjs "angular"
;;   :modes (nodejs-project-mode bower-project-mode)
;;   :when
;;   (lambda (&rest _)
;;     (let* ((project (doom-project-root))
;;            (bower (gethash project bower-conf))
;;            (npm (gethash project npm-conf))
;;            (deps (append (cdr-safe (assq 'dependencies bower))
;;                          (cdr-safe (assq 'dependencies npm))
;;                          (cdr-safe (assq 'devDependencies bower))
;;                          (cdr-safe (assq 'devDependencies npm)))))
;;       (assq 'angular deps))))

;; (def-project! jekyll ":{"
;;   :modes (web-mode js-mode coffee-mode css-mode haml-mode pug-mode)
;;   :match "/\\(\\(css\\|_\\(layouts\\|posts\\|sass\\)\\)/.+\\|.+.html\\)$"
;;   :files ("config.yml" "_layouts/")
;;   (add-hook! mode
;;     (when (eq major-mode 'web-mode)
;;       (web-mode-set-engine "django"))))

;; (def-project! wordpress "wp"
;;   :modes (php-mode web-mode css-mode haml-mode pug-mode)
;;   :match "/wp-\\(\\(content\\|admin\\|includes\\)/\\)?.+$"
;;   :files ("wp-config.php" "wp-content/"))
