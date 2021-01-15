;;; completion/selectrum/config.el -*- lexical-binding: t; -*-

(use-package! selectrum
  :hook (doom-first-input . selectrum-mode))

(when (featurep! +prescient)
  (use-package! selectrum-prescient
    :after selectrum
    :hook ((doom-first-input . selectrum-prescient-mode)
           (doom-first-input . prescient-persist-mode))))

(use-package! consult
  :defer t
  :init
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
    [remap load-theme] #'consult-theme))

(use-package! consult-flycheck)

(use-package! embark
  :init
  (define-key!
    "C-S-a" #'embark-act))

(use-package! marginalia
  :after selectrum
  :hook (doom-first-input . marginalia-mode)
  :init
  (setq-default marginalia-annotators '(marginalia-annotators-heavy)))

(use-package! embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))
