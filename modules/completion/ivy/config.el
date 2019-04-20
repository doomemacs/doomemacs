;;; completion/ivy/config.el -*- lexical-binding: t; -*-

(defvar +ivy-buffer-icons nil
  "If non-nil, show buffer mode icons in `ivy-switch-buffer' and the like.")

(defvar +ivy-buffer-preview nil
  "If non-nil, preview buffers while switching, à la `counsel-switch-buffer'.

When nil, don't preview anything.
When non-nil, preview non-virtual buffers.
When 'everything, also preview virtual buffers")

(defvar +ivy-task-tags
  '(("TODO"  . warning)
    ("FIXME" . error))
  "An alist of tags for `+ivy/tasks' to include in its search, whose CDR is the
face to render it with.")

(defvar +ivy-project-search-engines '(rg ag pt)
  "What search tools for `+ivy/project-search' (and `+ivy-file-search' when no
ENGINE is specified) to try, and in what order.

To disable a particular tool, remove it from this list. To prioritize a tool
over others, move it to the front of the list. Later duplicates in this list are
silently ignored.

If you want to already use git-grep or grep, set this to nil.")

(defvar +ivy-buffer-unreal-face 'font-lock-comment-face
  "The face for unreal buffers in `ivy-switch-to-buffer'.")

(defmacro +ivy-do-action! (action)
  "Returns an interactive lambda that sets the current ivy action and
immediately runs it on the current candidate (ending the ivy session)."
  `(lambda ()
     (interactive)
     (ivy-set-action ,action)
     (setq ivy-exit 'done)
     (exit-minibuffer)))


;;
;;; Packages

(def-package! ivy
  :defer 1
  :after-call pre-command-hook
  :config
  (setq ivy-height 15
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
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

  (after! yasnippet
    (add-to-list 'yas-prompt-functions #'+ivy-yas-prompt nil #'eq))

  (define-key! ivy-mode-map
    [remap switch-to-buffer]              #'+ivy/switch-buffer
    [remap switch-to-buffer-other-window] #'+ivy/switch-buffer-other-window
    [remap persp-switch-to-buffer]        #'+ivy/switch-workspace-buffer
    [remap imenu-anywhere]                #'ivy-imenu-anywhere)

  (ivy-mode +1)

  (def-package! ivy-hydra
    :commands (ivy-dispatching-done-hydra ivy--matcher-desc ivy-hydra/body)
    :init
    (define-key! ivy-minibuffer-map
      "C-o" #'ivy-dispatching-done-hydra
      "M-o" #'hydra-ivy/body)
    :config
    ;; ivy-hydra rebinds this, so we have to do so again
    (define-key ivy-minibuffer-map (kbd "M-o") #'hydra-ivy/body)))


(def-package! ivy-rich
  :hook (ivy-mode . ivy-rich-mode)
  :config
  (when +ivy-buffer-icons
    (cl-pushnew '(+ivy-rich-buffer-icon (:width 2 :align right))
                (cadr (plist-get ivy-rich-display-transformers-list
                                 'ivy-switch-buffer)))
    (after! counsel-projectile
      (setq ivy-rich-display-transformers-list
            (plist-put ivy-rich-display-transformers-list
                       'counsel-projectile-switch-project
                       '(:columns
                         (((lambda (_) (all-the-icons-octicon "file-directory"))
                           (:width 2 :align right))
                          (ivy-rich-candidate)))))
      (setq ivy-rich-display-transformers-list
            (plist-put ivy-rich-display-transformers-list
                       'counsel-projectile-find-file
                       '(:columns
                         ((all-the-icons-icon-for-file (:width 2 :align right))
                          (ivy-rich-candidate)))))))

  ;; Remove built-in coloring of buffer list; we do our own
  (setq ivy-switch-buffer-faces-alist nil)
  (ivy-set-display-transformer 'internal-complete-buffer nil)

  ;; Highlight buffers differently based on whether they're in the same project
  ;; as the current project or not.
  (let* ((plist (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))
         (switch-buffer-alist (assq 'ivy-rich-candidate (plist-get plist :columns))))
    (when switch-buffer-alist
      (setcar switch-buffer-alist '+ivy-rich-buffer-name)))

  ;; Allow these transformers to apply to more switch-buffer commands
  (let ((ivy-switch-buffer-transformer (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)))
    (dolist (cmd '(+ivy--switch-buffer counsel-projectile-switch-to-buffer))
      (setq ivy-rich-display-transformers-list
            (plist-put ivy-rich-display-transformers-list
                       cmd ivy-switch-buffer-transformer)))))


(def-package! counsel
  :commands counsel-describe-face
  :init
  (map! [remap apropos]                  #'counsel-apropos
        [remap bookmark-jump]            #'counsel-bookmark
        [remap describe-face]            #'counsel-faces
        [remap describe-function]        #'counsel-describe-function
        [remap describe-variable]        #'counsel-describe-variable
        [remap describe-bindings]        #'counsel-descbinds
        [remap set-variable]             #'counsel-set-variable
        [remap execute-extended-command] #'counsel-M-x
        [remap find-file]                #'counsel-find-file
        [remap find-library]             #'counsel-find-library
        [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
        [remap imenu]                    #'counsel-imenu
        [remap recentf-open-files]       #'counsel-recentf
        [remap org-capture]              #'counsel-org-capture
        [remap swiper]                   #'counsel-grep-or-swiper
        [remap evil-ex-registers]        #'counsel-evil-registers
        [remap yank-pop]                 #'counsel-yank-pop)
  :config
  (set-popup-rule! "^\\*ivy-occur" :size 0.35 :ttl 0 :quit nil)

  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable
        ;; Add smart-casing (-S) to default command arguments:
        counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag -S --nocolor --nogroup %s"
        counsel-pt-base-command "pt -S --nocolor --nogroup -e %s")

  (add-to-list 'swiper-font-lock-exclude #'+doom-dashboard-mode nil #'eq)

  ;; Factories
  (defun +ivy-action-reloading (cmd)
    (lambda (x)
      (funcall cmd x)
      (ivy--reset-state ivy-last)))

  (defun +ivy-action-given-file (cmd prompt)
    (lambda (source)
      (let* ((enable-recursive-minibuffers t)
             (target (read-file-name (format "%s %s to:" prompt source))))
        (funcall cmd source target 1))))

  ;; Configure `counsel-find-file'
  (ivy-add-actions
   'counsel-find-file
   `(("b" counsel-find-file-cd-bookmark-action "cd bookmark")
     ("s" counsel-find-file-as-root "open as root")
     ("m" counsel-find-file-mkdir-action "mkdir")
     ("c" ,(+ivy-action-given-file #'copy-file "Copy file") "copy file")
     ("d" ,(+ivy-action-reloading #'+ivy-confirm-delete-file) "delete")
     ("r" (lambda (path) (rename-file path (read-string "New name: "))) "rename")
     ("R" ,(+ivy-action-reloading (+ivy-action-given-file #'rename-file "Move")) "move")
     ("f" find-file-other-window "other window")
     ("F" find-file-other-frame "other frame")
     ("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory)))) "insert relative path")
     ("P" (lambda (path) (with-ivy-window (insert path))) "insert absolute path")
     ("l" (lambda (path) "Insert org-link with relative path"
            (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory))))) "insert org-link (rel. path)")
     ("L" (lambda (path) "Insert org-link with absolute path"
            (with-ivy-window (insert (format "[[%s]]" path)))) "insert org-link (abs. path)")))

  (ivy-add-actions
   'counsel-ag ; also applies to `counsel-rg' & `counsel-pt'
   '(("O" +ivy-git-grep-other-window-action "open in other window"))))


(def-package! counsel-projectile
  :commands (counsel-projectile-find-file counsel-projectile-find-dir counsel-projectile-switch-to-buffer
                                          counsel-projectile-grep counsel-projectile-ag counsel-projectile-switch-project)
  :init
  (map! [remap projectile-find-file]        #'+ivy/projectile-find-file
        [remap projectile-find-dir]         #'counsel-projectile-find-dir
        [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
        [remap projectile-grep]             #'counsel-projectile-grep
        [remap projectile-ag]               #'counsel-projectile-ag
        [remap projectile-switch-project]   #'counsel-projectile-switch-project)
  :config
  ;; no highlighting visited files; slows down the filtering
  (ivy-set-display-transformer #'counsel-projectile-find-file nil))


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
  (setq ivy-fixed-height-minibuffer nil
        ivy-posframe-parameters
        `((min-width . 90)
          (min-height . ,ivy-height)
          (internal-border-width . 10)))

  ;; ... let's do it manually instead
  (unless (assq 'ivy-posframe-display-at-frame-bottom-left ivy-display-functions-props)
    (dolist (fn (list 'ivy-posframe-display-at-frame-bottom-left
                      'ivy-posframe-display-at-frame-center
                      'ivy-posframe-display-at-point
                      'ivy-posframe-display-at-frame-bottom-window-center
                      'ivy-posframe-display
                      'ivy-posframe-display-at-window-bottom-left
                      'ivy-posframe-display-at-window-center
                      '+ivy-display-at-frame-center-near-bottom))
      (push (cons fn '(:cleanup ivy-posframe-cleanup)) ivy-display-functions-props)))
  ;; default to posframe display function
  (setf (alist-get t ivy-display-functions-alist) #'+ivy-display-at-frame-center-near-bottom)

  ;; Fix #1017: stop session persistence from restoring a broken posframe
  (defun +workspace|delete-all-posframes (&rest _) (posframe-delete-all))
  (add-hook 'persp-after-load-state-functions #'+workspace|delete-all-posframes)

  ;; posframe doesn't work well with async sources
  (dolist (fn '(swiper counsel-ag counsel-grep counsel-git-grep))
    (setf (alist-get fn ivy-display-functions-alist) #'ivy-display-function-fallback)))


(def-package! flx
  :when (featurep! +fuzzy)
  :defer t  ; is loaded by ivy
  :init
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (counsel-grep . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          (t . ivy--regex-fuzzy))
        ivy-initial-inputs-alist nil))


;; Used by `counsel-M-x'
(setq amx-save-file (concat doom-cache-dir "amx-items"))


;;
;; Evil key fixes

(map! :when (featurep! :feature evil +everywhere)
      :after ivy
      :map (ivy-occur-mode-map ivy-occur-grep-mode-map)
      :m "j"       #'ivy-occur-next-line
      :m "k"       #'ivy-occur-previous-line
      :m "h"       #'evil-backward-char
      :m "l"       #'evil-forward-char
      :m "g"       nil
      :m "gg"      #'evil-goto-first-line
      :map ivy-occur-mode-map
      :n [mouse-1] #'ivy-occur-click
      :n [return]  #'ivy-occur-press-and-switch
      :n "gf"      #'ivy-occur-press
      :n "ga"      #'ivy-occur-read-action
      :n "go"      #'ivy-occur-dispatch
      :n "gc"      #'ivy-occur-toggle-calling
      :n "gr"      #'ivy-occur-revert-buffer
      :n "q"       #'quit-window
      :map ivy-occur-grep-mode-map
      :v "j"       #'evil-next-line
      :v "k"       #'evil-previous-line
      :n "D"       #'ivy-occur-delete-candidate
      :n "C-d"     #'evil-scroll-down
      :n "d"       #'ivy-occur-delete-candidate
      :n "C-x C-q" #'ivy-wgrep-change-to-wgrep-mode
      :n "i"       #'ivy-wgrep-change-to-wgrep-mode
      :n "gd"      #'ivy-occur-delete-candidate
      :n [mouse-1] #'ivy-occur-click
      :n [return]  #'ivy-occur-press-and-switch
      :n "gf"      #'ivy-occur-press
      :n "gr"      #'ivy-occur-revert-buffer
      :n "ga"      #'ivy-occur-read-action
      :n "go"      #'ivy-occur-dispatch
      :n "gc"      #'ivy-occur-toggle-calling
      :n "q"       #'quit-window)
