;;; tools/magit/config.el -*- lexical-binding: t; -*-

(def-package! magit
  :defer t
  :init
  (load "magit-autoloads" nil t)
  :config
  (set! :popup "^\\*?magit" :ignore)

  (map! :map magit-repolist-mode-map
        :n "j" #'next-line
        :n "k" #'previous-line
        :n "s" #'magit-repolist-status)

  ;; (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)
  ;; (set! :popup "^.*magit" '((slot . -1) (side . right) (size . 80)) '((modeline . nil) (select . t)))
  ;; (set! :popup "^\\*magit.*popup\\*" '((slot . 0) (side . right)) '((modeline . nil) (select . t)))
  ;; (set! :popup "^.*magit-revision:.*" '((slot . 2) (side . right) (window-height . 0.6)) '((modeline . nil) (select . t)))
  ;; (set! :popup "^.*magit-diff:.*" '((slot . 2) (side . right) (window-height . 0.6)) '((modeline . nil) (select . nil)))

  (after! evil
    ;; Switch to emacs state only while in `magit-blame-mode', then back when
    ;; its done (since it's a minor-mode).
    (add-hook! 'magit-blame-mode-hook
      (evil-local-mode (if magit-blame-mode -1 +1)))))


(def-package! magit-blame
  :commands magit-blame
  :after git-timemachine)


(def-package! magithub
  :commands (magithub-clone magithub-feature-autoinject)
  :after magit
  :preface
  (setq magithub-dir (concat doom-etc-dir "magithub/"))
  :init
  (setq magithub-clone-default-directory "~/"
        magithub-preferred-remote-method 'clone_url)
  :config
  (load "magithub-autoloads" nil t)
  (magithub-feature-autoinject t))


(def-package! evil-magit
  :when (featurep! :feature evil)
  :after magit
  :config
  (setq evil-magit-state 'normal))
