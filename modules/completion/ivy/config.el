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
  :hook (doom-first-input . ivy-mode)
  :init
  (let ((standard-search-fn
         (if (modulep! +prescient)
             #'+ivy-prescient-non-fuzzy
           #'ivy--regex-plus))
        (alt-search-fn
         (if (modulep! +fuzzy)
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

  ;; Fix #4886: otherwise our remaps are overwritten
  (setq ivy-mode-map (make-sparse-keymap))
  :config
  ;; The default sorter is much to slow and the default for `ivy-sort-max-size'
  ;; is way too big (30,000). Turn it down so big repos affect project
  ;; navigation less.
  (setq ivy-sort-max-size 7500)

  ;; Counsel changes a lot of ivy's state at startup; to control for that, we
  ;; need to load it as early as possible. Some packages (like `ivy-prescient')
  ;; require this.
  (require 'counsel nil t)

  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-read-action-format-function #'ivy-read-action-format-columns
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
        #'+ivy-format-function-line-or-arrow)

  ;; Integrate `ivy' with `better-jumper'; ensure a jump point is registered
  ;; before jumping to new locations with ivy
  (setf (alist-get 't ivy-hooks-alist)
        (lambda ()
          (with-ivy-window
            (setq +ivy--origin (point-marker)))))

  (add-hook! 'minibuffer-exit-hook
    (defun +ivy--set-jump-point-maybe-h ()
      (when (markerp (bound-and-true-p +ivy--origin))
        (unless (equal (ignore-errors (with-ivy-window (point-marker)))
                       +ivy--origin)
          (with-current-buffer (marker-buffer +ivy--origin)
            (better-jumper-set-jump +ivy--origin)))
        (set-marker +ivy--origin nil))
      (setq +ivy--origin nil)))

  (after! yasnippet
    (add-hook 'yas-prompt-functions #'+ivy-yas-prompt-fn))

  (define-key! ivy-minibuffer-map
    [remap doom/delete-backward-word] #'ivy-backward-kill-word
    "C-c C-e" #'+ivy/woccur
    "C-o" #'ivy-dispatching-done))


(use-package! ivy-rich
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)

  (defun ivy-rich-bookmark-filename-or-empty (candidate)
    (let ((filename (ivy-rich-bookmark-filename candidate)))
      (if (not filename) "" filename)))

  ;; Enahnce the appearance of a couple counsel commands
  (plist-put! ivy-rich-display-transformers-list
              'counsel-describe-variable
              '(:columns
                ((counsel-describe-variable-transformer (:width 40)) ; the original transformer
                 (+ivy-rich-describe-variable-transformer (:width 50)) ; display variable value
                 (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
              'counsel-M-x
              '(:columns
                ((counsel-M-x-transformer (:width 60))
                 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
              ;; Apply switch buffer transformers to `counsel-projectile-switch-to-buffer' as well
              'counsel-projectile-switch-to-buffer
              (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)
              'counsel-bookmark
              '(:columns
                ((ivy-rich-candidate (:width 0.5))
                 (ivy-rich-bookmark-filename-or-empty (:width 60)))))

  ;; Remove built-in coloring of buffer list; we do our own
  (setq ivy-switch-buffer-faces-alist nil)
  (ivy-set-display-transformer 'internal-complete-buffer nil)

  ;; Highlight buffers differently based on whether they're in the same project
  ;; as the current project or not.
  (when-let* ((plist (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))
              (switch-buffer-alist (assq 'ivy-rich-candidate (plist-get plist :columns))))
    (setcar switch-buffer-alist '+ivy-rich-buffer-name))

  (when (modulep! +icons)
    (nerd-icons-ivy-rich-mode +1))
  (ivy-rich-mode +1)
  (ivy-rich-project-root-cache-mode +1))


(use-package! nerd-icons-ivy-rich
  :when (modulep! +icons)
  :commands (nerd-icons-ivy-rich-mode)
  :after counsel-projectile)


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
    [remap describe-symbol]          #'counsel-describe-symbol
    [remap evil-show-registers]      #'counsel-evil-registers
    [remap evil-show-marks]          #'counsel-mark-ring
    [remap execute-extended-command] #'counsel-M-x
    [remap find-file]                #'counsel-find-file
    [remap find-library]             #'counsel-find-library
    [remap imenu]                    #'counsel-imenu
    [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
    [remap load-theme]               #'counsel-load-theme
    [remap locate]                   #'counsel-locate
    [remap org-goto]                 #'counsel-org-goto
    [remap org-set-tags-command]     #'counsel-org-tag
    [remap projectile-compile-project] #'+ivy/project-compile
    [remap recentf-open-files]       #'counsel-recentf
    [remap set-variable]             #'counsel-set-variable
    [remap swiper]                   #'counsel-grep-or-swiper
    [remap insert-char]              #'counsel-unicode-char
    [remap yank-pop]                 #'counsel-yank-pop)
  :config
  (set-popup-rule! "^\\*ivy-occur" :size 0.35 :ttl 0 :quit nil)

  ;; HACK Fix an issue where `counsel-projectile-find-file-action' would try to
  ;;      open a candidate in an occur buffer relative to the wrong buffer,
  ;;      causing it to fail to find the file we want.
  (defadvice! +ivy--run-from-ivy-directory-a (fn &rest args)
    :around #'counsel-projectile-find-file-action
    (let ((default-directory (ivy-state-directory ivy-last)))
      (apply fn args)))

  ;; Don't use ^ as initial input. Set this here because `counsel' defines more
  ;; of its own, on top of the defaults.
  (setq ivy-initial-inputs-alist nil)

  ;; REVIEW Counsel allows `counsel-rg-base-command' to be a string or list.
  ;;        This backwards compatibility complicates things for Doom. Simpler to
  ;;        just force it to always be a list.
  (when (stringp counsel-rg-base-command)
    (setq counsel-rg-base-command (split-string counsel-rg-base-command)))

  ;; REVIEW: See abo-abo/swiper#2339.
  (defadvice! +counsel-rg-suppress-error-code-a (fn &rest args)
    "Ripgrep returns a non-zero exit code if it encounters any trouble (e.g. you
don't have the needed permissions for a couple files/directories in a project).
Even if rg continues to produce workable results, that non-zero exit code causes
counsel-rg to discard the rest of the output to display an error.

This advice suppresses the error code, so you can still operate on whatever
workable results ripgrep produces, despite the error."
    :around #'counsel-rg
    (letf! (defun process-exit-status (proc)
             (let ((code (funcall process-exit-status proc)))
               (if (= code 2) 0 code)))
      (apply fn args)))

  ;; Decorate `doom/help-custom-variable' results the same way as
  ;; `counsel-describe-variable' (adds value and docstring columns).
  (ivy-configure 'doom/help-custom-variable :parent 'counsel-describe-variable)

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
  (when (featurep :system 'macos)
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
        (cond ((when-let (fd (executable-find (or doom-fd-executable "fd") t))
                 (append (list fd "--hidden" "--type" "file" "--type" "symlink" "--follow" "--color=never")
                         (cl-loop for dir in projectile-globally-ignored-directories
                                  collect "--exclude"
                                  collect dir)
                         (if (featurep :system 'windows) '("--path-separator=/")))))
              ((executable-find "rg" t)
               (append (list "rg" "--hidden" "--files" "--follow" "--color=never" "--no-messages")
                       (cl-loop for dir in projectile-globally-ignored-directories
                                collect "--glob"
                                collect (concat "!" dir))
                       (if (featurep :system 'windows) '("--path-separator=/"))))
              ((cons find-program args)))
      (unless (listp args)
        (user-error "`counsel-file-jump-args' is a list now, please customize accordingly."))
      (counsel--call
       (cons find-program args)
       (lambda ()
         (goto-char (point-min))
         (let (files)
           (while (< (point) (point-max))
             (push (buffer-substring (line-beginning-position) (line-end-position))
                   files)
             (forward-line 1))
           (nreverse files)))))))


(use-package! counsel-projectile
  :after ivy-rich
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

  ;; HACK: Force `counsel-projectile-switch-project' to call
  ;;   `projectile-relevant-known-projects' and initialize the known projects
  ;;   list, because otherwise it's trying to read from the
  ;;   `projectile-known-projects' variable directly instead of calling the
  ;;   function of the same name.
  ;; REVIEW: This should be fixed upstream.
  (setq counsel-projectile-remove-current-project t)

  ;; no highlighting visited files; slows down the filtering
  (ivy-set-display-transformer #'counsel-projectile-find-file nil)

  (when (modulep! +prescient)
    (setq counsel-projectile-sort-files t)))


(use-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


(use-package! ivy-posframe
  :when (modulep! +childframe)
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
          #'ivy-display-function-fallback))

  (add-hook 'doom-after-reload-hook #'posframe-delete-all))


(use-package! flx
  :when (modulep! +fuzzy)
  :unless (modulep! +prescient)
  :defer t  ; is loaded by ivy
  :preface (when (or (modulep! -fuzzy)
                     (modulep! +prescient))
             (setq ivy--flx-featurep nil))
  :init (setq ivy-flx-limit 10000))

(use-package! ivy-avy
  :after ivy)

(use-package! ivy-prescient
  :when (modulep! +prescient)
  :hook (ivy-mode . ivy-prescient-mode)
  :hook (ivy-prescient-mode . prescient-persist-mode)
  :commands +ivy-prescient-non-fuzzy
  :init
  (setq prescient-filter-method
        (if (modulep! +fuzzy)
            '(literal regexp initialism fuzzy)
          '(literal regexp initialism)))
  :config
  ;; REVIEW Remove when radian-software/prescient.el#102 is resolved
  (add-to-list 'ivy-sort-functions-alist '(ivy-resume))
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer lsp-ivy-workspace-symbol
          ivy-resume ivy--restore-session counsel-grep counsel-git-grep
          counsel-rg counsel-ag counsel-ack counsel-fzf counsel-pt counsel-imenu
          counsel-yank-pop counsel-recentf counsel-buffer-or-recentf
          counsel-outline counsel-org-goto counsel-jq)
        ivy-prescient-retain-classic-highlighting t)
  (defun +ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  ;; NOTE prescient config duplicated with `company'
  (setq prescient-save-file (concat doom-cache-dir "prescient-save.el")))


;;;###package swiper
(setq swiper-action-recenter t)


;;;###package amx
(setq amx-save-file (concat doom-cache-dir "amx-items"))  ; used by `counsel-M-x'
