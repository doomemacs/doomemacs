;;; completion/ivy/config.el -*- lexical-binding: t; -*-

(defvar +ivy-buffer-icons nil
  "If non-nil, show buffer mode icons in `ivy-switch-buffer' and the like.")

(defvar +ivy-task-tags
  '(("TODO"  . warning)
    ("FIXME" . error))
  "An alist of tags for `+ivy/tasks' to include in its search, whose CDR is the
face to render it with.")

(defmacro +ivy-do-action! (action)
  "Returns an interactive lambda that sets the current ivy action and
immediately runs it on the current candidate (ending the ivy session)."
  `(lambda ()
     (interactive)
     (ivy-set-action ,action)
     (setq ivy-exit 'done)
     (exit-minibuffer)))


;;
;; Packages
;;

(def-package! ivy
  :defer 1
  :after-call pre-command-hook
  :config
  (setq ivy-height 15
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        smex-completion-method 'ivy
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; highlight til EOL
        ivy-format-function #'ivy-format-function-line
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function nil
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t)

  (after! magit     (setq magit-completing-read-function #'ivy-completing-read))
  (after! yasnippet (add-to-list 'yas-prompt-functions #'+ivy-yas-prompt nil #'eq))

  (map! [remap switch-to-buffer]       #'ivy-switch-buffer
        [remap persp-switch-to-buffer] #'+ivy/switch-workspace-buffer
        [remap imenu-anywhere]         #'ivy-imenu-anywhere)

  (ivy-mode +1))


;; Show more buffer information in switch-buffer commands
(def-package! ivy-rich
  :after ivy
  :config
  (dolist (cmd '(ivy-switch-buffer +ivy/switch-workspace-buffer
                 counsel-projectile-switch-to-buffer))
    (ivy-set-display-transformer cmd '+ivy-buffer-transformer)))


(def-package! counsel
  :commands counsel-describe-face
  :init
  (map! [remap apropos]                  #'counsel-apropos
        [remap bookmark-jump]            #'counsel-bookmark
        [remap describe-face]            #'counsel-describe-face
        [remap describe-function]        #'counsel-describe-function
        [remap describe-variable]        #'counsel-describe-variable
        [remap execute-extended-command] #'counsel-M-x
        [remap find-file]                #'counsel-find-file
        [remap find-library]             #'counsel-find-library
        [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
        [remap imenu]                    #'counsel-imenu
        [remap recentf-open-files]       #'counsel-recentf
        [remap org-capture]              #'counsel-org-capture
        [remap swiper]                   #'counsel-grep-or-swiper)
  :config
  (set! :popup "^\\*ivy-occur" '((size . 0.35)) '((transient . 0) (quit)))

  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        ;; Add smart-casing and compressed archive searching (-zS) to default
        ;; command arguments:
        counsel-rg-base-command "rg -zS --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag -zS --nocolor --nogroup %s"
        counsel-pt-base-command "pt -zS --nocolor --nogroup -e %s")

  ;; Dim recentf entries that are not in the current project.
  (ivy-set-display-transformer #'counsel-recentf #'+ivy-recentf-transformer)

  ;; Configure `counsel-rg', `counsel-ag' & `counsel-pt'
  (dolist (cmd '(counsel-ag counsel-rg counsel-pt))
    (ivy-add-actions
     cmd
     '(("O" +ivy-git-grep-other-window-action "open in other window")))))


(def-package! counsel-projectile
  :commands (counsel-projectile-find-file counsel-projectile-find-dir counsel-projectile-switch-to-buffer
             counsel-projectile-grep counsel-projectile-ag counsel-projectile-switch-project)
  :init
  (map! [remap projectile-find-file]        #'counsel-projectile-find-file
        [remap projectile-find-dir]         #'counsel-projectile-find-dir
        [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
        [remap projectile-grep]             #'counsel-projectile-grep
        [remap projectile-ag]               #'counsel-projectile-ag
        [remap projectile-switch-project]   #'counsel-projectile-switch-project)
  :config
  ;; Highlight entries that have been visited
  (ivy-set-display-transformer #'counsel-projectile-find-file #'+ivy-projectile-find-file-transformer))


;; Used by `counsel-M-x'
(after! smex
  (setq smex-save-file (concat doom-cache-dir "/smex-items"))
  (smex-initialize))


(def-package! ivy-hydra
  :commands (+ivy@coo/body ivy-dispatching-done-hydra)
  :init
  (map! :after ivy
        :map ivy-minibuffer-map
        "C-o" #'+ivy@coo/body
        "M-o" #'ivy-dispatching-done-hydra)
  :config
  (defhydra +ivy@coo (:hint nil :color pink)
    "
 Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
 _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
 ^↨^ _h_ ^+^ _l_ ^↕^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
 _G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
"
    ;; arrows
    ("j" ivy-next-line)
    ("k" ivy-previous-line)
    ("l" ivy-alt-done)
    ("h" ivy-backward-delete-char)
    ("g" ivy-beginning-of-buffer)
    ("G" ivy-end-of-buffer)
    ("d" ivy-scroll-up-command)
    ("u" ivy-scroll-down-command)
    ("e" ivy-scroll-down-command)
    ;; actions
    ("q" keyboard-escape-quit :exit t)
    ("C-g" keyboard-escape-quit :exit t)
    ("<escape>" keyboard-escape-quit :exit t)
    ("C-o" nil)
    ("i" nil)
    ("TAB" ivy-alt-done :exit nil)
    ("C-j" ivy-alt-done :exit nil)
    ("RET" ivy-done :exit t)
    ("C-m" ivy-done :exit t)
    ("C-SPC" ivy-call-and-recenter :exit nil)
    ("f" ivy-call)
    ("c" ivy-toggle-calling)
    ("m" ivy-toggle-fuzzy)
    (">" ivy-minibuffer-grow)
    ("<" ivy-minibuffer-shrink)
    ("w" ivy-prev-action)
    ("s" ivy-next-action)
    ("a" ivy-read-action)
    ("t" (setq truncate-lines (not truncate-lines)))
    ("C" ivy-toggle-case-fold)
    ("o" ivy-occur :exit t)))


(def-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


(def-package! ivy-posframe
  :when (and EMACS26+ (featurep! +childframe))
  :hook (ivy-mode . ivy-posframe-enable)
  :preface
  ;; This function searches the entire `obarray' just to populate
  ;; `ivy-display-functions-props'. There are 15k entries in mine! This is
  ;; wasteful, so...
  (advice-add #'ivy-posframe-setup :override #'ignore)
  :config
  (setq ivy-height 16
        ivy-fixed-height-minibuffer nil
        ivy-posframe-parameters `((min-width . 90)
                                  (min-height . ,ivy-height)
                                  (internal-border-width . 10)))

  ;; ... let's do it manually
  (dolist (fn (list 'ivy-posframe-display-at-frame-bottom-left
                    'ivy-posframe-display-at-frame-center
                    'ivy-posframe-display-at-point
                    'ivy-posframe-display-at-frame-bottom-window-center
                    'ivy-posframe-display
                    'ivy-posframe-display-at-window-bottom-left
                    'ivy-posframe-display-at-window-center
                    '+ivy-display-at-frame-center-near-bottom))
    (map-put ivy-display-functions-props fn '(:cleanup ivy-posframe-cleanup)))

  (map-put ivy-display-functions-alist 't '+ivy-display-at-frame-center-near-bottom)

  ;; posframe doesn't work well with async sources
  (dolist (fn '(swiper counsel-rg counsel-ag counsel-pt counsel-grep counsel-git-grep))
    (map-put ivy-display-functions-alist fn nil)))


(def-package! flx
  :when (featurep! +fuzzy)
  :defer t  ; is loaded by ivy
  :init
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (counsel-pt . ivy--regex-plus)
          (counsel-grep-or-swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))
        ivy-initial-inputs-alist nil))
