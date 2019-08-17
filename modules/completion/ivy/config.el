;;; completion/ivy/config.el -*- lexical-binding: t; -*-

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

(defvar +ivy-edit-functions nil
  "A plist mapping ivy/counsel commands to commands that generate an editable
results buffer.")

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

(use-package! ivy
  :defer 1
  :after-call pre-command-hook
  :init
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (counsel-grep . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          ;; Ignore order for non-fuzzy searches by default
          (t . ivy--regex-ignore-order)))
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

  ;; REVIEW Move this somewhere else and perhaps generalize this so both
  ;; ivy/helm users can enjoy it.
  (defadvice! +ivy--counsel-file-jump-use-fd-rg-a (args)
    "Change `counsel-file-jump' to use fd or ripgrep, if they are available."
    :override #'counsel--find-return-list
    (cl-destructuring-bind (find-program . args)
        (cond ((executable-find "fd")
               (cons "fd" (list "-t" "f" "-E" ".git")))
              ((executable-find "rg")
               (cons "rg" (list "--files" "--hidden" "--no-messages")))
              ((cons find-program args)))
      (unless (listp args)
        (user-error "`counsel-file-jump-args' is a list now, please customize accordingly."))
      (counsel--call
       (cons find-program args)
       (lambda ()
         (goto-char (point-min))
         (let ((offset (if (member find-program '("fd" "rg")) 0 2))
               files)
           (while (< (point) (point-max))
             (push (buffer-substring
                    (+ offset (line-beginning-position)) (line-end-position)) files)
             (forward-line 1))
           (nreverse files))))))

  ;; Ensure a jump point is registered before jumping to new locations with ivy
  (defvar +ivy--origin nil)
  (defun +ivy--record-position-maybe-fn ()
    (with-ivy-window
      (setq +ivy--origin (point-marker))))
  (setq ivy-hooks-alist '((t . +ivy--record-position-maybe-fn)))

  (add-hook! 'minibuffer-exit-hook
    (defun +ivy--set-jump-point-maybe-h ()
      (with-demoted-errors "Ivy error: %s"
        (when (and (markerp +ivy--origin)
                   (not (equal (with-ivy-window (point-marker))
                               +ivy--origin)))
          (with-current-buffer (marker-buffer +ivy--origin)
            (better-jumper-set-jump +ivy--origin)))
        (setq +ivy--origin nil))))

  (after! yasnippet
    (add-to-list 'yas-prompt-functions #'+ivy-yas-prompt nil #'eq))

  (defadvice! +ivy--inhibit-in-evil-ex-a (orig-fn &rest args)
    "`ivy-completion-in-region' struggles with completing certain
evil-ex-specific constructs, so we disable it solely in evil-ex."
    :around #'evil-ex
    (let ((completion-in-region-function #'completion--in-region))
      (apply orig-fn args)))

  (define-key! ivy-mode-map
    [remap switch-to-buffer]              #'+ivy/switch-buffer
    [remap switch-to-buffer-other-window] #'+ivy/switch-buffer-other-window
    [remap persp-switch-to-buffer]        #'+ivy/switch-workspace-buffer)

  (define-key ivy-minibuffer-map (kbd "C-c C-e") #'+ivy/woccur)

  (ivy-mode +1)

  (use-package! ivy-hydra
    :commands (ivy-dispatching-done-hydra ivy--matcher-desc ivy-hydra/body)
    :init
    (define-key! ivy-minibuffer-map
      "C-o" #'ivy-dispatching-done-hydra
      "M-o" #'hydra-ivy/body)
    :config
    ;; ivy-hydra rebinds this, so we have to do so again
    (define-key ivy-minibuffer-map (kbd "M-o") #'hydra-ivy/body)))


(use-package! ivy-rich
  :after ivy
  :config
  (when (featurep! +icons)
    (cl-pushnew '(+ivy-rich-buffer-icon)
                (cadr (plist-get ivy-rich-display-transformers-list
                                 'ivy-switch-buffer))))

  ;; Include variable value in `counsel-describe-variable'
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'counsel-describe-variable
                   '(:columns
                     ((counsel-describe-variable-transformer (:width 40)) ; the original transformer
                      (+ivy-rich-describe-variable-transformer (:width 50))
                      (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))))

  ;; Remove built-in coloring of buffer list; we do our own
  (setq ivy-switch-buffer-faces-alist nil)
  (ivy-set-display-transformer 'internal-complete-buffer nil)

  ;; Highlight buffers differently based on whether they're in the same project
  ;; as the current project or not.
  (let* ((plist (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))
         (switch-buffer-alist (assq 'ivy-rich-candidate (plist-get plist :columns))))
    (when switch-buffer-alist
      (setcar switch-buffer-alist '+ivy-rich-buffer-name)))

  ;; Apply switch buffer transformers to `counsel-projectile-switch-to-buffer' as well
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'counsel-projectile-switch-to-buffer
                   (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)))

  ;; Reload ivy which so changes to `ivy-rich-display-transformers-list' work
  (ivy-rich-mode +1))


(use-package! all-the-icons-ivy
  :when (featurep! +icons)
  :after ivy
  :config
  ;; `all-the-icons-ivy' is incompatible with ivy-rich's switch-buffer
  ;; modifications, so we disable them and merge them ourselves
  (setq all-the-icons-ivy-buffer-commands nil)

  (all-the-icons-ivy-setup)
  (after! counsel-projectile
    (let ((all-the-icons-ivy-file-commands '(counsel-projectile
                                             counsel-projectile-find-file
                                             counsel-projectile-find-dir)))
      (all-the-icons-ivy-setup))))


(use-package! counsel
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

  ;; Record in jumplist when opening files via counsel-{ag,rg,pt,git-grep}
  (add-hook 'counsel-grep-post-action-hook #'better-jumper-set-jump)

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


(use-package! counsel-projectile
  :defer t
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


(use-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


(use-package! ivy-posframe
  :when (and EMACS26+ (featurep! +childframe))
  :hook (ivy-mode . ivy-posframe-mode)
  :config
  (setq ivy-fixed-height-minibuffer nil
        ivy-posframe-border-width 10
        ivy-posframe-parameters
        `((min-width . 90)
          (min-height . ,ivy-height)))

  ;; default to posframe display function
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'+ivy-display-at-frame-center-near-bottom-fn)

  ;; posframe doesn't work well with async sources
  (dolist (fn '(swiper counsel-ag counsel-grep counsel-git-grep))
    (setf (alist-get fn ivy-posframe-display-functions-alist)
          #'ivy-display-function-fallback)))


(use-package! flx
  :when (and (featurep! +fuzzy)
             (not (featurep! +prescient)))
  :defer t  ; is loaded by ivy
  :init
  (setf (alist-get 't ivy-re-builders-alist) #'ivy--regex-fuzzy)
  (setq ivy-initial-inputs-alist nil
        ivy-flx-limit 10000))


(use-package! ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode)
  :when (featurep! +prescient)
  :init
  (setq prescient-filter-method
        (if (featurep! +fuzzy)
            '(literal regexp initialism fuzzy)
          '(literal regexp initialism))
        ivy-prescient-enable-filtering nil  ; we do this ourselves
        ivy-prescient-retain-classic-highlighting t
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist
        '((counsel-ag . +ivy-prescient-non-fuzzy)
          (counsel-rg . +ivy-prescient-non-fuzzy)
          (counsel-grep . +ivy-prescient-non-fuzzy)
          (swiper . +ivy-prescient-non-fuzzy)
          (swiper-isearch . +ivy-prescient-non-fuzzy)
          (t . ivy-prescient-re-builder)))

  :config
  (defun +ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  ;; NOTE prescient config duplicated with `company'
  (setq prescient-save-file (concat doom-cache-dir "prescient-save.el"))
  (prescient-persist-mode +1))


;;;###package amx
(setq amx-save-file (concat doom-cache-dir "amx-items"))  ; used by `counsel-M-x'
