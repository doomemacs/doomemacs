;;; completion/ivy/config.el -*- lexical-binding: t; -*-

(defvar +ivy-buffer-preview nil
  "If non-nil, preview buffers while switching, à la `counsel-switch-buffer'.

When nil, don't preview anything.
When non-nil, preview non-virtual buffers.
When 'everything, also preview virtual buffers")

(defvar +ivy-buffer-unreal-face 'font-lock-comment-face
  "The face for unreal buffers in `ivy-switch-to-buffer'.")

(defvar +ivy-edit-functions nil
  "A plist mapping ivy/counsel commands to commands that generate an editable
results buffer.")


;;
;;; Packages

(use-package! ivy
  :after-call pre-command-hook
  :init
  (let ((standard-search-fn
         (if (featurep! +prescient)
             #'+ivy-prescient-non-fuzzy
           #'ivy--regex-plus))
        (alt-search-fn
         (if (featurep! +fuzzy)
             #'ivy--regex-fuzzy
           ;; Ignore order for non-fuzzy searches by default
           #'ivy--regex-ignore-order)))
    (setq ivy-re-builders-alist
          `((counsel-rg     . ,standard-search-fn)
            (swiper         . ,standard-search-fn)
            (swiper-isearch . ,standard-search-fn)
            (t . ,alt-search-fn))
          ivy-more-chars-alist
          '((counsel-rg . 1)
            (counsel-search . 2)
            (t . 3))))

  (define-key!
    [remap switch-to-buffer]              #'+ivy/switch-buffer
    [remap switch-to-buffer-other-window] #'+ivy/switch-buffer-other-window
    [remap persp-switch-to-buffer]        #'+ivy/switch-workspace-buffer
    [remap evil-show-jumps]               #'+ivy/jump-list)
  :config
  ;; Counsel changes a lot of ivy's state at startup; to control for that, we
  ;; need to load it as early as possible. Some packages (like `ivy-prescient')
  ;; require this.
  (require 'counsel nil t)

  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t)

  ;; Highlight each ivy candidate including the following newline, so that it
  ;; extends to the right edge of the window
  (setf (alist-get 't ivy-format-functions-alist)
        #'ivy-format-function-line)

  ;; Integrate `ivy' with `better-jumper'; ensure a jump point is registered
  ;; before jumping to new locations with ivy
  (setf (alist-get 't ivy-hooks-alist)
        (lambda ()
          (with-ivy-window
            (setq +ivy--origin (point-marker)))))

  (add-hook! 'minibuffer-exit-hook
    (defun +ivy--set-jump-point-maybe-h ()
      (and (markerp (bound-and-true-p +ivy--origin))
           (not (equal (ignore-errors (with-ivy-window (point-marker)))
                       +ivy--origin))
           (with-current-buffer (marker-buffer +ivy--origin)
             (better-jumper-set-jump +ivy--origin)))
      (setq +ivy--origin nil)))

  (after! yasnippet
    (add-hook 'yas-prompt-functions #'+ivy-yas-prompt))

  (defadvice! +ivy--inhibit-completion-in-region-a (orig-fn &rest args)
    "`ivy-completion-in-region' struggles with completing certain
evil-ex-specific constructs, so we disable it solely in evil-ex."
    :around #'evil-ex
    (let ((completion-in-region-function #'completion--in-region))
      (apply orig-fn args)))

  (define-key! ivy-minibuffer-map
    "C-c C-e" #'+ivy/woccur
    [remap doom/delete-backward-word] #'ivy-backward-kill-word
    "C-o" #'ivy-dispatching-done
    "M-o" #'hydra-ivy/body)

  (ivy-mode +1))


(use-package! ivy-rich
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)

  (when (featurep! +icons)
    (cl-pushnew '(+ivy-rich-buffer-icon)
                (cadr (plist-get ivy-rich-display-transformers-list
                                 'ivy-switch-buffer))))

  ;; Include variable value in `counsel-describe-variable'
  (plist-put! ivy-rich-display-transformers-list
              'counsel-describe-variable
              '(:columns
                ((counsel-describe-variable-transformer (:width 40)) ; the original transformer
                 (+ivy-rich-describe-variable-transformer (:width 50))
                 (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))))

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
  (plist-put! ivy-rich-display-transformers-list
              'counsel-projectile-switch-to-buffer
              (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))

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
  :defer t
  :init
  (define-key!
    [remap apropos]                  #'counsel-apropos
    [remap bookmark-jump]            #'counsel-bookmark
    [remap compile]                  #'+ivy/compile
    [remap describe-bindings]        #'counsel-descbinds
    [remap describe-face]            #'counsel-faces
    [remap describe-function]        #'counsel-describe-function
    [remap describe-variable]        #'counsel-describe-variable
    [remap evil-ex-registers]        #'counsel-evil-registers
    [remap evil-show-marks]          #'counsel-mark-ring
    [remap execute-extended-command] #'counsel-M-x
    [remap find-file]                #'counsel-find-file
    [remap find-library]             #'counsel-find-library
    [remap imenu]                    #'counsel-imenu
    [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
    [remap load-theme]               #'counsel-load-theme
    [remap locate]                   #'counsel-locate
    [remap org-set-tags-command]     #'counsel-org-tag
    [remap projectile-compile-project] #'+ivy/project-compile
    [remap recentf-open-files]       #'counsel-recentf
    [remap set-variable]             #'counsel-set-variable
    [remap swiper]                   #'counsel-grep-or-swiper
    [remap unicode-chars-list-chars] #'counsel-unicode-char
    [remap yank-pop]                 #'counsel-yank-pop)
  :config
  (set-popup-rule! "^\\*ivy-occur" :size 0.35 :ttl 0 :quit nil)

  ;; HACK Fix an issue where `counsel-projectile-find-file-action' would try to
  ;;      open a candidate in an occur buffer relative to the wrong buffer,
  ;;      causing it to fail to find the file we want.
  (defadvice! +ivy--run-from-ivy-directory-a (orig-fn &rest args)
    :around #'counsel-projectile-find-file-action
    (let ((default-directory (ivy-state-directory ivy-last)))
      (apply orig-fn args)))

  ;; Don't use ^ as initial input. Set this here because `counsel' defines more
  ;; of its own, on top of the defaults.
  (setq ivy-initial-inputs-alist nil)

  ;; Integrate with `helpful'
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)

  ;; Record in jumplist when opening files via counsel-{ag,rg,pt,git-grep}
  (add-hook 'counsel-grep-post-action-hook #'better-jumper-set-jump)
  (ivy-add-actions
   'counsel-rg ; also applies to `counsel-rg'
   '(("O" +ivy-git-grep-other-window-action "open in other window")))

  ;; Make `counsel-compile' projectile-aware (if you prefer it over
  ;; `+ivy/compile' and `+ivy/project-compile')
  (add-to-list 'counsel-compile-root-functions #'projectile-project-root)
  (after! savehist
    ;; Persist `counsel-compile' history
    (add-to-list 'savehist-additional-variables 'counsel-compile-history))

  ;; `counsel-imenu' -- no sorting for imenu. Sort it by appearance in page.
  (add-to-list 'ivy-sort-functions-alist '(counsel-imenu))

  ;; `counsel-locate'
  (when IS-MAC
    ;; Use spotlight on mac by default since it doesn't need any additional setup
    (setq counsel-locate-cmd #'counsel-locate-cmd-mdfind))

  ;; `swiper'
  ;; Don't mess with font-locking on the dashboard; it causes breakages
  (add-to-list 'swiper-font-lock-exclude #'+doom-dashboard-mode)

  ;; `counsel-find-file'
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  (dolist (fn '(counsel-rg counsel-find-file))
    (ivy-add-actions
     fn '(("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory))))
           "insert relative path")
          ("P" (lambda (path) (with-ivy-window (insert path)))
           "insert absolute path")
          ("l" (lambda (path) (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory)))))
           "insert relative org-link")
          ("L" (lambda (path) (with-ivy-window (insert (format "[[%s]]" path))))
           "Insert absolute org-link"))))

  (ivy-add-actions 'counsel-file-jump (plist-get ivy--actions-list 'counsel-find-file))

  ;; `counsel-search': use normal page for displaying results, so that we see
  ;; custom ddg themes (if one is set).
  (setf (nth 1 (alist-get 'ddg counsel-search-engines-alist))
        "https://duckduckgo.com/?q=")

  ;; REVIEW Move this somewhere else and perhaps generalize this so both
  ;;        ivy/helm users can enjoy it.
  (defadvice! +ivy--counsel-file-jump-use-fd-rg-a (args)
    "Change `counsel-file-jump' to use fd or ripgrep, if they are available."
    :override #'counsel--find-return-list
    (cl-destructuring-bind (find-program . args)
        (cond ((executable-find doom-projectile-fd-binary)
               (cons doom-projectile-fd-binary (list "-t" "f" "-E" ".git")))
              ((executable-find "rg")
               (split-string (format counsel-rg-base-command "--files --no-messages") " " t))
              ((cons find-program args)))
      (unless (listp args)
        (user-error "`counsel-file-jump-args' is a list now, please customize accordingly."))
      (counsel--call
       (cons find-program args)
       (lambda ()
         (goto-char (point-min))
         (let ((offset (if (member find-program (list "rg" doom-projectile-fd-binary)) 0 2))
               files)
           (while (< (point) (point-max))
             (push (buffer-substring
                    (+ offset (line-beginning-position)) (line-end-position)) files)
             (forward-line 1))
           (nreverse files)))))))


(use-package! counsel-projectile
  :defer t
  :init
  (define-key!
    [remap projectile-find-file]        #'+ivy/projectile-find-file
    [remap projectile-find-dir]         #'counsel-projectile-find-dir
    [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
    [remap projectile-grep]             #'counsel-projectile-grep
    [remap projectile-ag]               #'counsel-projectile-ag
    [remap projectile-switch-project]   #'counsel-projectile-switch-project)
  :config
  ;; A more sensible `counsel-projectile-find-file' that reverts to
  ;; `counsel-find-file' if invoked from $HOME, `counsel-file-jump' if invoked
  ;; from a non-project, `projectile-find-file' if in a big project (more than
  ;; `ivy-sort-max-size' files), or `counsel-projectile-find-file' otherwise.
  (setf (alist-get 'projectile-find-file counsel-projectile-key-bindings)
        #'+ivy/projectile-find-file)

  ;; no highlighting visited files; slows down the filtering
  (ivy-set-display-transformer #'counsel-projectile-find-file nil)

  (if (featurep! +prescient)
      (setq counsel-projectile-sort-files t)))


(use-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


(use-package! ivy-posframe
  :when (featurep! +childframe)
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

  ;; posframe doesn't work well with async sources (the posframe will
  ;; occasionally stop responding/redrawing), and causes violent resizing of the
  ;; posframe.
  (dolist (fn '(swiper counsel-rg counsel-grep counsel-git-grep))
    (setf (alist-get fn ivy-posframe-display-functions-alist)
          #'ivy-display-function-fallback)))


(use-package! flx
  :when (featurep! +fuzzy)
  :unless (featurep! +prescient)
  :defer t  ; is loaded by ivy
  :init (setq ivy-flx-limit 10000))


(use-package! ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode)
  :when (featurep! +prescient)
  :init
  (setq prescient-filter-method
        (if (featurep! +fuzzy)
            '(literal regexp initialism fuzzy)
          '(literal regexp initialism)))
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer counsel-grep
               counsel-git-grep counsel-ag counsel-rg counsel-imenu
               counsel-yank-pop counsel-recentf counsel-buffer-or-recentf)
        ivy-prescient-retain-classic-highlighting t)
  (defun +ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  ;; NOTE prescient config duplicated with `company'
  (setq prescient-save-file (concat doom-cache-dir "prescient-save.el"))
  (prescient-persist-mode +1))


;;;###package swiper
(setq swiper-action-recenter t)


;;;###package amx
(setq amx-save-file (concat doom-cache-dir "amx-items"))  ; used by `counsel-M-x'
