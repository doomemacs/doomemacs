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
  :config
  (setq magit-completing-read-function
        (if (featurep! :completion ivy)
            #'ivy-completing-read
          #'magit-builtin-completing-read)
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-diff-refine-hunk t ;; Show word-granularity on the currently selected hunk
        magit-display-buffer-function #'+magit-display-buffer-fullscreen)

  (set! :popup "^\\(?:\\*magit\\|magit:\\)" :ignore)
  ;; Consider magit buffers real (so they can switched to)
  (add-hook 'magit-mode-hook #'doom|mark-buffer-as-real)
  ;; no mode-line in magit popups
  (add-hook! '(magit-mode-hook magit-popup-mode-hook)
    #'hide-mode-line-mode)
  ;; Clean up after magit by properly killing buffers
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'+magit/quit))


(def-package! magit-blame :after git-timemachine)


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
  (magithub-feature-autoinject +magit-hub-features))


(def-package! magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))


(def-package! evil-magit
  :when (featurep! :feature evil +everywhere)
  :after magit
  :init (setq evil-magit-state 'normal)
  :config
  (map! :map (magit-mode-map magit-blame-read-only-mode-map)
        doom-leader-key nil))
