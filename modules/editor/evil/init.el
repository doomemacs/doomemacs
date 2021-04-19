;;; editor/evil/init.el -*- lexical-binding: t; -*-

(defvar evil-collection-key-blacklist)

;; We load evil-collection ourselves for these reasons:
;;
;; 1. To truly lazy load it. Some of its modules, like
;;    evil-collection-{elisp-mode,buff-menu} are loaded immediately, because
;;    Emacs loads their packages immediately, which pulls in all of
;;    evil-collection (and other packages with it, sometimes).
;; 2. This ensures a predictable load order, versus lazy loading using :defer or
;;    :after-call. This means users can use (after! org ...) and be sure that
;;    their changes will override evil-collection's.
;; 3. Ideally, we'd do away with evil-collection entirely. It changes too often,
;;    introduces breaking bugs too frequently, and I don't agree with all their
;;    design choices. Regardless, it does more good than trouble, so it may be
;;    here to stay.
;; 4. Adds `+evil-collection-disabled-list', to make it easier for users to
;;    disable modules, and to reduce the effort required to maintain our copy of
;;    `evil-collection-list' (now I can just copy it from time to time).

(when (and doom-interactive-p
           (not doom-reloading-p)
           (featurep! +everywhere))

  (setq evil-collection-company-use-tng (featurep! :completion company +tng)
        ;; must be set before evil/evil-collection is loaded
        evil-want-keybinding nil)

  (defvar +evil-collection-disabled-list
    '(anaconda-mode
      buff-menu
      comint
      company
      custom
      eldoc
      elisp-mode
      ert
      free-keys
      helm
      indent
      image
      kotlin-mode
      occur
      outline
      package-menu
      simple
      slime
      lispy)
    "A list of `evil-collection' modules to ignore. See the definition of this
variable for an explanation of the defaults (in comments). See
`evil-collection-mode-list' for a list of available options.")

  (defvar evil-collection-setup-minibuffer nil)

  ;; We do this ourselves, and better.
  (defvar evil-collection-want-unimpaired-p nil)
  ;; Doom binds goto-reference on gD and goto-assignments on gA ourselves
  (defvar evil-collection-want-find-usages-bindings-p nil)

  ;; We handle loading evil-collection ourselves
  (defvar evil-collection--supported-modes nil)

  ;; This has to be defined here since evil-collection doesn't autoload its own.
  ;; It must be updated whenever evil-collection updates theirs. Here's an easy
  ;; way to update it:
  ;;
  ;; (with-current-buffer
  ;;     (url-retrieve-synchronously "https://raw.githubusercontent.com/emacs-evil/evil-collection/master/evil-collection.el" t t)
  ;;   (goto-char (point-min))
  ;;   (when (re-search-forward "^(defvar evil-collection--supported-modes\n[^(]+")
  ;;     (let ((list (sexp-at-point)))
  ;;       ;; Fixes
  ;;       (when (assq 'pdf list)
  ;;         (setf (alist-get 'pdf list) '(pdf-tools)))
  ;;       (let ((diff (cl-set-difference evil-collection-mode-list list :test #'equal)))
  ;;         (list (- (length list) (length evil-collection-mode-list))
  ;;               diff)
  ;;         (message "diff: %s" diff)
  ;;         (kill-new (prin1-to-string list))))))

  (defvar evil-collection-mode-list
    `(2048-game
      ag
      alchemist
      anaconda-mode
      apropos
      arc-mode
      auto-package-update
      bm
      bookmark
      (buff-menu "buff-menu")
      calc
      calendar
      cider
      cmake-mode
      comint
      company
      compile
      consult
      (custom cus-edit)
      cus-theme
      daemons
      dashboard
      deadgrep
      debbugs
      debug
      dictionary
      diff-mode
      dired
      dired-sidebar
      disk-usage
      doc-view
      docker
      ebib
      edbi
      edebug
      ediff
      eglot
      explain-pause-mode
      elfeed
      elisp-mode
      elisp-refs
      elisp-slime-nav
      emms
      epa
      ert
      eshell
      eval-sexp-fu
      evil-mc
      eww
      finder
      flycheck
      flymake
      free-keys
      geiser
      ggtags
      git-timemachine
      gnus
      go-mode
      grep
      guix
      hackernews
      helm
      help
      helpful
      hg-histedit
      hungry-delete
      ibuffer
      image
      image-dired
      image+
      imenu
      imenu-list
      (indent "indent")
      indium
      info
      ivy
      js2-mode
      leetcode
      lispy
      log-edit
      log-view
      lsp-ui-imenu
      lua-mode
      kotlin-mode
      macrostep
      man
      magit
      magit-todos
      monky
      mu4e
      mu4e-conversation
      neotree
      newsticker
      notmuch
      nov
      (occur replace)
      omnisharp
      org-present
      osx-dictionary
      outline
      p4
      (package-menu package)
      pass
      (pdf pdf-tools)
      popup
      proced
      prodigy
      profiler
      python
      quickrun
      racer
      racket-describe
      realgud
      reftex
      restclient
      rg
      ripgrep
      rjsx-mode
      robe
      rtags
      ruby-mode
      sh-script
      simple
      slime
      sly
      speedbar
      tablist
      tar-mode
      (term term ansi-term multi-term)
      tetris
      ,@(if EMACS27+ '(thread))
      tide
      timer-list
      transmission
      trashed
      typescript-mode
      vc-annotate
      vc-dir
      vc-git
      vdiff
      view
      vlf
      vterm
      w3m
      wdired
      wgrep
      which-key
      woman
      xref
      xwidget
      youtube-dl
      zmusic
      (ztree ztree-diff)))

  (defun +evil-collection-init (module &optional disabled-list)
    "Initialize evil-collection-MODULE.

Unlike `evil-collection-init', this respects `+evil-collection-disabled-list',
and complains if a module is loaded too early (during startup)."
    (unless (memq (or (car-safe module) module) disabled-list)
      (doom-log "Initialized evil-collection-%s %s"
                (or (car-safe module) module)
                (if doom-init-time "" "(too early!)"))
      (with-demoted-errors "evil-collection error: %s"
        (evil-collection-init (list module)))))

  (defadvice! +evil-collection-disable-blacklist-a (orig-fn)
    :around #'evil-collection-vterm-toggle-send-escape  ; allow binding to ESC
    (let (evil-collection-key-blacklist)
      (funcall-interactively orig-fn)))

  ;; These modes belong to packages that Emacs always loads at startup, causing
  ;; evil-collection and it's co-packages to all load immediately. We avoid this
  ;; by loading them after evil-collection has first loaded...
  (with-eval-after-load 'evil-collection
    ;; Don't let evil-collection interfere with certain keys
    (setq evil-collection-key-blacklist
          (append (list doom-leader-key doom-localleader-key
                        doom-leader-alt-key)
                  (when (featurep! :tools lookup)
                    '("gd" "gf" "K"))
                  (when (featurep! :tools eval)
                    '("gr" "gR"))
                  '("[" "]" "gz" "<escape>")))

    (evil-define-key* 'normal process-menu-mode-map
      "q" #'kill-current-buffer
      "d" #'process-menu-delete-process)

    (mapc #'+evil-collection-init '(comint custom)))

  ;; ...or on first invokation of their associated major/minor modes.
  (after! evil
    ;; Emacs loads these two packages immediately, at startup, which needlessly
    ;; convolutes load order for evil-collection-help.
    (defer-feature! help help-mode)
    (defer-feature! help-mode help-mode)

    (add-transient-hook! 'Buffer-menu-mode
      (+evil-collection-init '(buff-menu "buff-menu")))
    (add-transient-hook! 'image-mode
      (+evil-collection-init 'image))
    (add-transient-hook! 'emacs-lisp-mode
      (+evil-collection-init 'elisp-mode))
    (add-transient-hook! 'occur-mode
      (+evil-collection-init '(occur replace)))
    (add-transient-hook! 'indent-rigidly
      (+evil-collection-init '(indent "indent")))
    (add-transient-hook! 'minibuffer-setup-hook
      (when evil-collection-setup-minibuffer
        (+evil-collection-init 'minibuffer)
        (evil-collection-minibuffer-insert)))
    (add-transient-hook! 'process-menu-mode
      (+evil-collection-init '(process-menu simple)))
    (add-transient-hook! 'tabulated-list-mode
      (+evil-collection-init 'tabulated-list))
    (when EMACS27+
      (add-transient-hook! 'tab-bar-mode
        (+evil-collection-init 'tab-bar)))

    ;; HACK Do this ourselves because evil-collection break's `eval-after-load'
    ;;      load order by loading their target plugin before applying keys. This
    ;;      makes it hard for end-users to overwrite these keybinds with a
    ;;      simple `after!' or `with-eval-after-load'.
    (dolist (mode evil-collection-mode-list)
      (dolist (req (or (cdr-safe mode) (list mode)))
        (with-eval-after-load req
          (+evil-collection-init mode +evil-collection-disabled-list))))))
