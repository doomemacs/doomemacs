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
  (add-hook 'doom-post-init-hook #'ivy-mode)
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
        ivy-magic-slash-non-match-action nil)

  (after! magit     (setq magit-completing-read-function #'ivy-completing-read))
  (after! yasnippet (push #'+ivy-yas-prompt yas-prompt-functions))

  (map! [remap apropos]                   #'counsel-apropos
        [remap describe-face]             #'counsel-describe-face
        [remap find-file]                 #'counsel-find-file
        [remap switch-to-buffer]          #'ivy-switch-buffer
        [remap persp-switch-to-buffer]    #'+ivy/switch-workspace-buffer
        [remap recentf-open-files]        #'counsel-recentf
        [remap imenu]                     #'counsel-imenu
        [remap bookmark-jump]             #'counsel-bookmark
        [remap projectile-find-file]      #'counsel-projectile-find-file
        [remap imenu-anywhere]            #'ivy-imenu-anywhere
        [remap execute-extended-command]  #'counsel-M-x
        [remap describe-face]             #'counsel-describe-face)

  ;; Show more buffer information in switch-buffer commands
  (ivy-set-display-transformer #'ivy-switch-buffer #'+ivy-buffer-transformer)
  (ivy-set-display-transformer #'ivy-switch-buffer-other-window #'+ivy-buffer-transformer)
  (ivy-set-display-transformer #'+ivy/switch-workspace-buffer #'+ivy-buffer-transformer)
  (ivy-set-display-transformer #'counsel-recentf #'abbreviate-file-name)

  (nconc ivy-sort-functions-alist
         '((persp-kill-buffer   . nil)
           (persp-remove-buffer . nil)
           (persp-add-buffer    . nil)
           (persp-switch        . nil)
           (persp-window-switch . nil)
           (persp-frame-switch  . nil)
           (+workspace/switch-to . nil)
           (+workspace/delete . nil))))


(def-package! swiper :commands (swiper swiper-all))


(def-package! counsel
  :requires ivy
  :config
  (require 'counsel-projectile)
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  ;; Configure `counsel-rg', `counsel-ag' & `counsel-pt'
  (set! :popup 'ivy-occur-grep-mode :size (+ 2 ivy-height) :regexp t :autokill t)
  (dolist (cmd '(counsel-ag counsel-rg counsel-pt))
    (ivy-add-actions
     cmd
     '(("O" +ivy-git-grep-other-window-action "open in other window"))))

  ;; 1. Remove character limit from `counsel-ag-function'
  ;; 2. This may need to be updated frequently, to meet changes upstream
  ;; 3. counsel-ag, counsel-rg and counsel-pt all use this function
  (advice-add #'counsel-ag-function :override #'+ivy*counsel-ag-function))


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
