;;; completion/vertico/config.el -*- lexical-binding: t; -*-

(use-package! vertico
  :hook (doom-first-input . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  (setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))
  ;; cleans up path when moving directories with shadowed paths syntax,
  ;; e.g. cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook  #'vertico-directory-tidy)
  (map! :map vertico-map
        [backspace] #'+vertico/backward-updir))

(use-package! orderless
  :defer t
  :after-call doom-first-input-hook
  :config
  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in find-file etc.
        completion-category-overrides '((file (styles . (orderless partial-completion))))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")
  ;; otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package! consult
  :defer t
  :init
  (advice-add #'multi-occur :override #'consult-multi-occur)
  (define-key!
    [remap apropos]                       #'consult-apropos
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap man]                           #'consult-man
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop
    [remap persp-switch-to-buffer]        #'+vertico/switch-workspace-buffer)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  (recentf-mode)
  (setq consult-project-root-function #'doom-project-root
        consult-narrow-key "<"
        consult-line-numbers-widen t)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   +default/search-project +default/search-project-for-symbol-at-point
   +default/search-other-project +vertico/search-symbol-at-point
   +default/search-cwd +default/search-other-cwd
   +default/search-notes-for-symbol-at-point
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (list (kbd "C-SPC") (kbd "C-M-j") (kbd "C-M-k")))
  (consult-customize
   consult-theme
   :preview-key
   (list (kbd "C-SPC") (kbd "C-M-j") (kbd "C-M-k")
         :debounce 0.5 'any))
  (after! org
    (defvar +vertico--consult-org-source
      `(:name     "Org"
        :narrow   ?o
        :hidden t
        :category buffer
        :state    ,#'consult--buffer-state
        :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))
    (add-to-list 'consult-buffer-sources '+vertico--consult-org-source 'append)))

(use-package! consult-flycheck
  :when (featurep! :checkers syntax)
  :after (consult flycheck))

(use-package! embark
  :defer t
  :init
  (map! "C-;"               #'embark-act  ; to be moved to :config default if accepted
        :map minibuffer-local-map
        "C-;"               #'embark-act
        "C-c C-;"           #'embark-export
        :desc "Export to writable buffer"
        "C-c C-e"           #'+vertico/embark-export-write
        :leader
        :desc "Actions" "a" #'embark-act) ; to be moved to :config default if accepted
  (define-key!
    [remap describe-bindings] #'embark-bindings)
  :config
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
  ;; add the package! target finder before the file target finder,
  ;; so we don't get a false positive match.
  (let ((pos (or (cl-position
                  'embark-target-file-at-point
                  embark-target-finders)
                 (length embark-target-finders))))
    (cl-callf2
        cons
        '+vertico--embark-target-package
        (nthcdr pos embark-target-finders)))
  (setq embark-package-map (make-sparse-keymap))
  (map!
   :map embark-file-map
   :desc "Open target with sudo" "s" #'doom/sudo-find-file
   :desc "Open in new workspace" "TAB" #'+vertico-embark-open-in-new-workspace
   :map embark-package-map
   "h" #'doom/help-packages
   "b" #'doom/bump-package
   "c" #'doom/help-package-config
   "u" #'doom/help-package-homepage))

(use-package! marginalia
  :hook (doom-first-input . marginalia-mode)
  :init
  (map! :map minibuffer-local-map
        :desc "Cycle marginalia views"
        "M-A" #'marginalia-cycle)
  :config
  (when (featurep! +icons)
    (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))
  (nconc marginalia-command-categories
         '((persp-switch-to-buffer . buffer)
           (projectile-find-file . project-file)
           (doom/describe-active-minor-mode . minor-mode)
           (flycheck-error-list-set-filter . builtin))))

(use-package! embark-consult
  :after (embark consult)
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(use-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))
