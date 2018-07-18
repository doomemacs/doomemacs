;;; tools/magit/config.el -*- lexical-binding: t; -*-

(defvar +magit-hub-enable-by-default nil
  "Whether or not to enable magithub features for all projects by default. Must
be set before `magithub' (and `magit') is loaded.")

(defvar +magit-hub-features
  '(pull-request-merge commit-browse completion)
  "What features to initialize when `magithub' is loaded. Set this to `t' to
load everything.")


;;
;; Plugins
;;

(def-package! magit
  :commands magit-file-delete
  :init
  (setq magit-auto-revert-mode nil)  ; we already use `global-auto-revert-mode'
  :config
  (setq magit-completing-read-function
        (if (featurep! :completion ivy)
            #'ivy-completing-read
          #'magit-builtin-completing-read)
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-diff-refine-hunk t  ; show word-granularity on selected hunk
        magit-display-buffer-function #'+magit-display-buffer-fullscreen
        magit-popup-display-buffer-action '((display-buffer-in-side-window)))

  (set-popup-rule! "^\\(?:\\*magit\\|magit:\\)" :ignore t)
  ;; so magit buffers can be switched to
  (add-hook 'magit-mode-hook #'doom|mark-buffer-as-real)
  ;; modeline isn't helpful in magit
  (add-hook! '(magit-mode-hook magit-popup-mode-hook)
    #'hide-mode-line-mode)
  ;; properly kill leftover magit buffers on quit
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'+magit/quit))


(def-package! magit-blame :after git-timemachine)


(def-package! magit-todos
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-require-colon nil)
  (define-key magit-todos-section-map "j" nil))


(def-package! magithub
  :after magit
  :preface
  (setq magithub-dir (concat doom-etc-dir "magithub/"))
  :init
  (setq magithub-clone-default-directory "~/"
        magithub-preferred-remote-method 'clone_url)
  :config
  (unless +magit-hub-enable-by-default
    ;; Disable magit by default. Can be enabled through magithub settings popup,
    ;; or setting `+magit-hub-enable-by-default'.
    (advice-add #'magithub-enabled-p :override #'+magit*hub-enabled-p)
    ;; I don't use `magithub-settings--simple' to redefine this because it
    ;; changes the order of settings. Obnoxious, but the alternative is even
    ;; more so.
    (advice-add #'magithub-settings--format-magithub.enabled
                :override #'+magit*hub-settings--format-magithub.enabled))
  (when +magit-hub-features
    (magithub-feature-autoinject +magit-hub-features)))


(def-package! magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))


(def-package! evil-magit
  :when (featurep! :feature evil +everywhere)
  :after magit
  :init
  (setq evil-magit-state 'normal
        evil-magit-use-z-for-folds t)
  :config
  (define-key! magit-mode-map
    (kbd "M-1") nil
    (kbd "M-2") nil
    (kbd "M-3") nil
    (kbd "M-4") nil)
  (evil-define-key* '(normal visual) magit-mode-map
    "zz" #'evil-scroll-line-to-center)
  (after! git-rebase
    (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
      (setcar (assoc (car key) evil-magit-rebase-commands-w-descriptions)
              (cdr key)))
    (evil-define-key* evil-magit-state git-rebase-mode-map
      "gj" #'git-rebase-move-line-down
      "gk" #'git-rebase-move-line-up))
  (map! :map (magit-mode-map magit-blame-read-only-mode-map)
        doom-leader-key nil))
