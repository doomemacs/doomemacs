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
  :init
  (add-hook 'doom-init-hook #'ivy-mode)
  :config
  (setq ivy-height 12
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
        ivy-virtual-abbreviate 'full)

  (after! magit     (setq magit-completing-read-function #'ivy-completing-read))
  (after! yasnippet (push #'+ivy-yas-prompt yas-prompt-functions))

  (map! [remap switch-to-buffer]       #'ivy-switch-buffer
        [remap persp-switch-to-buffer] #'+ivy/switch-workspace-buffer
        [remap imenu-anywhere]         #'ivy-imenu-anywhere)

  (nconc ivy-sort-functions-alist
         '((persp-kill-buffer   . nil)
           (persp-remove-buffer . nil)
           (persp-add-buffer    . nil)
           (persp-switch        . nil)
           (persp-window-switch . nil)
           (persp-frame-switch  . nil)
           (+workspace/switch-to . nil)
           (+workspace/delete . nil))))


;; Show more buffer information in switch-buffer commands
(def-package! ivy-rich
  :after ivy
  :config
  (dolist (cmd '(ivy-switch-buffer +ivy/switch-workspace-buffer
                 counsel-projectile-switch-to-buffer))
    (ivy-set-display-transformer cmd '+ivy-buffer-transformer)))


(def-package! swiper :commands (swiper swiper-all))


(def-package! counsel
  :commands (counsel-ag counsel-rg counsel-pt counsel-apropos counsel-bookmark
             counsel-describe-function counsel-describe-variable
             counsel-describe-face counsel-M-x counsel-file-jump
             counsel-find-file counsel-find-library counsel-info-lookup-symbol
             counsel-imenu counsel-recentf counsel-yank-pop
             counsel-descbinds counsel-org-capture)
  :init
  (map! [remap apropos]                  #'counsel-apropos
        [remap bookmark-jump]            #'counsel-bookmark
        [remap describe-face]            #'counsel-describe-face
        [remap describe-function]        #'counsel-describe-function
        [remap describe-variable]        #'counsel-describe-variable
        [remap execute-extended-command] #'counsel-M-x
        [remap find-file]                #'counsel-find-file
        [remap find-library]             #'counsel-find-library
        [remap yank-pop]                 #'counsel-yank-pop
        [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
        [remap imenu]                    #'counsel-imenu
        [remap recentf-open-files]       #'counsel-recentf
        [remap org-capture]              #'counsel-org-capture)
  :config
  (set! :popup "^\\*ivy-occur" '((size . 0.35)) '((transient . 0) (quit)))

  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  ;; Dim recentf entries that are not in the current project.
  (ivy-set-display-transformer #'counsel-recentf #'+ivy-recentf-transformer)

  ;; Configure `counsel-rg', `counsel-ag' & `counsel-pt'
  (dolist (cmd '(counsel-ag counsel-rg counsel-pt))
    (ivy-add-actions
     cmd
     '(("O" +ivy-git-grep-other-window-action "open in other window"))))

  ;; Removes character limit from `counsel-ag-function'
  ;;
  ;; This may need to be updated frequently, to meet changes upstream
  ;; counsel-ag, counsel-rg and counsel-pt all use this function
  (advice-add #'counsel-ag-function :override #'+ivy*counsel-ag-function))


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
(def-package! smex
  :commands (smex smex-major-mode-commands)
  :config
  (setq smex-save-file (concat doom-cache-dir "/smex-items"))
  (smex-initialize))


(def-package! ivy-hydra
  :commands (+ivy@coo/body ivy-dispatching-done-hydra)
  :init
  (map! :map ivy-minibuffer-map
        "C-o" #'+ivy@coo/body
        "M-o" #'ivy-dispatching-done-hydra)
  :config
  (def-hydra! +ivy@coo (:hint nil :color pink)
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
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :config (setq wgrep-auto-save-buffer t))
