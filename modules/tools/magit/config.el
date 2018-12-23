;;; tools/magit/config.el -*- lexical-binding: t; -*-

(defvar +magit-hub-enable-by-default nil
  "Whether or not to enable magithub features for all projects by default. Must
be set before `magithub' (and `magit') is loaded.")

(defvar +magit-hub-features t
  "What features to initialize when `magithub' is loaded. Set this to `t' to
load everything, and nil to load nothing. See `magithub-feature-list' to see
what features are available.")


;;
;; Packages

(def-package! magit
  :commands magit-file-delete
  :defer-incrementally (dash f s with-editor git-commit package)
  :init
  (setq magit-auto-revert-mode nil)  ; we already use `global-auto-revert-mode'
  :config
  (setq magit-completing-read-function
        (if (featurep! :completion ivy)
            #'ivy-completing-read
          #'magit-builtin-completing-read)
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-diff-refine-hunk t  ; show word-granularity on selected hunk
        magit-display-buffer-function #'+magit-display-buffer
        magit-popup-display-buffer-action '((+magit-display-popup-buffer)))

  (set-popup-rule! "^\\(?:\\*magit\\|magit:\\)" :ignore t)

  (magit-define-popup-option 'magit-rebase-popup
    ?S "Sign using gpg" "--gpg-sign=" #'magit-read-gpg-secret-key)

  ;; so magit buffers can be switched to (except for process buffers)
  (defun +magit-buffer-p (buf)
    (with-current-buffer buf
      (and (derived-mode-p 'magit-mode)
           (not (eq major-mode 'magit-process-mode)))))
  (add-to-list 'doom-real-buffer-functions #'+magit-buffer-p nil #'eq)

  ;; modeline isn't helpful in magit
  (add-hook! '(magit-mode-hook magit-popup-mode-hook)
    #'hide-mode-line-mode)

  ;; properly kill leftover magit buffers on quit
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'+magit/quit))


(def-package! magit-todos
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-require-colon nil)
  (define-key magit-todos-section-map "j" nil)
  (advice-add #'magit-todos-mode :around #'doom*shut-up))


(def-package! magithub
  :after magit
  :preface
  ;; Magithub is not well-behaved, so this needs to be set early
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
  (unmap! magit-mode-map "M-1" "M-2" "M-3" "M-4") ; replaced by z1, z2, z3, etc
  (evil-define-key* '(normal visual) magit-mode-map
    "zz" #'evil-scroll-line-to-center
    "%"  #'magit-gitflow-popup)
  (after! git-rebase
    (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
      (setcar (assoc (car key) evil-magit-rebase-commands-w-descriptions)
              (cdr key)))
    (evil-define-key* evil-magit-state git-rebase-mode-map
      "gj" #'git-rebase-move-line-down
      "gk" #'git-rebase-move-line-up)))
