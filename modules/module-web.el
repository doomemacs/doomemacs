;;; module-web.el

(use-package haml-mode :mode "\\.haml$")

(use-package pug-mode
  :mode ("\\.jade$" "\\.pug$")
  :config
  (def-company-backend! pug-mode (yasnippet))
  (push '("jade" "html") projectile-other-file-alist)
  (push '("pug" "html")  projectile-other-file-alist)
  (map! :map pug-mode-map
        :i [tab] 'doom/dumb-indent
        :i [backtab] 'doom/dumb-dedent))

(use-package web-mode
  :mode ("\\.p?html?$"
         "\\.\\(tpl\\|blade\\)\\(\\.php\\)?$"
         "\\.erb$"
         "\\.jsp$"
         "\\.as[cp]x$"
         "\\.mustache$"
         "wp-content/themes/.+/.+\\.php$")
  :init
  (add-hook 'web-mode-hook 'turn-off-smartparens-mode)
  :config
  (def-company-backend! web-mode (web-html yasnippet))
  (setq web-mode-enable-html-entities-fontification t)
  (push '("html" "jade" "pug" "jsx" "tsx") projectile-other-file-alist)

  (map! :map web-mode-map :i "SPC" 'self-insert-command)

  (after! nlinum
    ;; Fix blank line numbers after unfolding
    (advice-add 'web-mode-fold-or-unfold :after 'nlinum--flush))

  (map! :map web-mode-map
        "M-/" 'web-mode-comment-or-uncomment
        :n  "M-r" 'doom/web-refresh-browser
        :n  "za" 'web-mode-fold-or-unfold
        (:localleader :n "t" 'web-mode-element-rename)
        :nv "]a" 'web-mode-attribute-next
        :nv "[a" 'web-mode-attribute-previous
        :nv "]t" 'web-mode-tag-next
        :nv "[t" 'web-mode-tag-previous
        :nv "]T" 'web-mode-element-child
        :nv "[T" 'web-mode-element-parent))


;;
;; Tools
;;

(use-package emmet-mode
  :commands (emmet-mode)
  :init
  (add-hook! (scss-mode web-mode html-mode haml-mode nxml-mode) 'emmet-mode)
  (defvar emmet-mode-keymap (make-sparse-keymap))
  :config
  (setq emmet-move-cursor-between-quotes t)
  (map! :map emmet-mode-keymap
        :v "M-e" 'emmet-wrap-with-markup
        :i "M-e" 'emmet-expand-yas
        :i "M-E" 'emmet-expand-line))


;;
;; Project types
;;

(defvar bower-conf (make-hash-table :test 'equal))
(def-project-type! bower "bower"
  :modes (web-mode js-mode coffee-mode css-mode sass-mode pug-mode)
  :files ("bower.json")
  :when
  (lambda (&rest _)
    (let* ((project-path (doom/project-root))
           (hash (gethash project-path bower-conf))
           (package-file (f-expand "bower.json" project-path))
           deps)
      (awhen (and (not hash) (f-exists? package-file)
                  (ignore-errors (json-read-file package-file)))
        (puthash project-path it bower-conf)))
    t))

(def-project-type! angularjs "angular"
  :modes (nodejs-project-mode bower-project-mode)
  :when
  (lambda (&rest _)
    (let* ((project (doom/project-root))
           (bower (gethash project bower-conf))
           (npm (gethash project npm-conf))
           (deps (append (cdr-safe (assq 'dependencies bower))
                         (cdr-safe (assq 'dependencies npm))
                         (cdr-safe (assq 'devDependencies bower))
                         (cdr-safe (assq 'devDependencies npm)))))
      (assq 'angular deps))))

(def-project-type! jekyll ":{"
  :modes (web-mode js-mode coffee-mode css-mode haml-mode pug-mode)
  :match "/\\(\\(css\\|_\\(layouts\\|posts\\|sass\\)\\)/.+\\|.+.html\\)$"
  :files ("config.yml" "_layouts/")
  (add-hook! mode
    (when (eq major-mode 'web-mode)
      (web-mode-set-engine "django"))))

(def-project-type! wordpress "wp"
  :modes (php-mode web-mode css-mode haml-mode pug-mode)
  :match "/wp-\\(\\(content\\|admin\\|includes\\)/\\)?.+$"
  :files ("wp-config.php" "wp-content/"))

(provide 'module-web)
;;; module-web.el ends here
