;;; completion/ivy/packages.el

(defvar +ivy-task-tags '(("TODO"  . warning)
                         ("FIXME" . error))
  "An alist of tags for `+ivy/tasks' to include in its search, whose CDR is the
face to render it with.")

(defmacro +ivy-do-action! (action)
  "A factory function that returns an interactive lamba that sets the current
ivy action and immediately runs it on the current candidate (ending the ivy
session)."
  `(lambda ()
     (interactive)
     (ivy-set-action ,action)
     (setq ivy-exit 'done)
     (exit-minibuffer)))


;;
;; Packages
;;

(def-package! ivy :demand t
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

  (after! magit      (setq magit-completing-read-function #'ivy-completing-read))
  (after! yasnippet  (push #'+ivy-yas-prompt yas-prompt-functions))

  (ivy-mode +1)

  (map! :map ivy-minibuffer-map
        [escape] #'keyboard-escape-quit
        "M-v" #'yank
        "M-z" #'undo
        "C-r" #'evil-paste-from-register
        "C-k" #'ivy-previous-line
        "C-j" #'ivy-next-line
        "C-l" #'ivy-alt-done
        "C-w" #'+ivy/backward-kill-word
        "C-u" #'doom/minibuffer-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word)

  (map! :map ivy-mode-map
        [remap describe-face]             #'counsel-describe-face
        [remap find-file]                 #'counsel-find-file
        [remap switch-to-buffer]          #'+ivy/switch-buffer
        [remap persp-switch-to-buffer]    #'+ivy/switch-workspace-buffer
        [remap recentf]                   #'counsel-recentf
        [remap imenu]                     #'counsel-imenu
        [remap bookmark-jump]             #'counsel-bookmark
        [remap projectile-switch-project] #'counsel-projectile-switch-project
        [remap projectile-find-file]      #'counsel-projectile-find-file
        [remap imenu-anywhere]            #'ivy-imenu-anywhere
        [remap execute-extended-command]  #'counsel-M-x
        [remap describe-function]         #'counsel-describe-function
        [remap describe-variable]         #'counsel-describe-variable
        [remap describe-face]             #'counsel-describe-face)

  (when (featurep! :feature workspaces)
    (nconc ivy-sort-functions-alist
           '((persp-kill-buffer   . nil)
             (persp-remove-buffer . nil)
             (persp-add-buffer    . nil)
             (persp-switch        . nil)
             (persp-window-switch . nil)
             (persp-frame-switch  . nil)
             (+workspace/switch-to . nil)
             (+workspace/delete . nil)))))


(def-package! swiper :commands (swiper swiper-all))


(def-package! counsel
  :after ivy
  :config
  (require 'counsel-projectile)

  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  ;; Configure `counsel-rg', `counsel-ag' & `counsel-pt'
  (set! :popup 'ivy-occur-grep-mode :size (+ 2 ivy-height) :regexp t :autokill t)

  (dolist (cmd '(counsel-ag counsel-rg counsel-pt))
    (ivy-add-actions
     cmd
     '(("O" +ivy-git-grep-other-window-action "open in other window"))))

  (map! :map counsel-ag-map
        [backtab] #'+ivy/wgrep-occur  ; search/replace on results
        "C-SPC"   #'counsel-git-grep-recenter   ; preview
        "M-RET"   (+ivy-do-action! #'+ivy-git-grep-other-window-action))

  (advice-add #'counsel-ag-function :override #'+ivy*counsel-ag-function))


;; Used by `counsel-M-x'
(def-package! smex
  :commands (smex smex-major-mode-commands)
  :config
  (setq smex-save-file (concat doom-cache-dir "/smex-items"))
  (smex-initialize))

