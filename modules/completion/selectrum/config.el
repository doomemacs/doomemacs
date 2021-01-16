;;; completion/selectrum/config.el -*- lexical-binding: t; -*-

(use-package! selectrum
  :hook (doom-first-input . selectrum-mode)
  :config
  (setq selectrum-extend-current-candidate-highlight t
        selectrum-fix-minibuffer-height t)
  (unless (featurep! +orderless)
    (setq completion-styles '(substring partial-completion))))

(when (featurep! +prescient)
  (use-package! selectrum-prescient
    :after selectrum
    :hook ((doom-first-input . selectrum-prescient-mode)
           (doom-first-input . prescient-persist-mode))))

(use-package! orderless
  :when (featurep! +orderless)
  :config
  (setq completion-styles '(orderless)))

(use-package! consult
  :defer t
  :init
  (fset 'multi-occur #'consult-multi-occur)
  (define-key!
    [remap apropos] #'consult-apropos
    [remap goto-line] #'consult-goto-line
    [remap imenu] #'consult-imenu
    [remap switch-to-buffer] #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame
    [remap man] #'consult-man
    [remap yank-pop] #'consult-yank-pop
    [remap locate] #'consult-locate
    [remap load-theme] #'consult-theme
    [remap recentf-open-files] #'consult-recent-file)
  :config
  (setq consult-project-root-function #'projectile-project-root))

(use-package! consult-flycheck
  :when (featurep! :checkers syntax)
  :after (consult flycheck))

(use-package! embark
  :init
  (define-key!
    "C-S-a" #'embark-act)
  (define-key! selectrum-minibuffer-map
    "C-c C-o" #'embark-export
    "C-c C-c" #'embark-act-noexit))

(use-package! marginalia
  :after selectrum
  :hook (doom-first-input . marginalia-mode)
  :init
  (setq-default marginalia-annotators '(marginalia-annotators-heavy)))

(use-package! embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))
