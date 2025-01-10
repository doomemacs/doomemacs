;;; editor/evil/init.el -*- lexical-binding: t; -*-
;;;###if (modulep! +everywhere)

(defvar evil-collection-key-blacklist)

;; must be set before evil/evil-collection is loaded
(defvar evil-want-keybinding nil)

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

(unless (or noninteractive (doom-context-p 'reload))

  (setq evil-collection-company-use-tng (modulep! :completion company +tng))

  (defvar +evil-collection-disabled-list
    '(anaconda-mode
      buff-menu
      calc
      comint
      company
      custom
      eldoc
      elisp-mode
      ert
      free-keys
      helm
      help
      image
      indent
      kmacro
      kotlin-mode
      lispy
      outline
      replace
      shortdoc
      simple
      slime
      tab-bar)
    "A list of `evil-collection' modules to ignore. See the definition of this
variable for an explanation of the defaults (in comments). See
`evil-collection-mode-list' for a list of available options.")

  (defvar evil-collection-setup-minibuffer nil)

  ;; We do this ourselves, and better.
  (defvar evil-collection-want-unimpaired-p nil)
  ;; Doom binds goto-reference on gD and goto-assignments on gA ourselves
  (defvar evil-collection-want-find-usages-bindings-p nil)
  ;; Reduces keybind conflicts between outline-mode and org-mode (which is
  ;; derived from outline-mode).
  (defvar evil-collection-outline-enable-in-minor-mode-p nil)

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
      atomic-chrome
      auto-package-update
      beginend
      bluetooth
      bm
      bookmark
      (buff-menu "buff-menu")
      bufler
      calc
      calendar
      cider
      citre
      cmake-mode
      color-rg
      comint
      company
      compile
      consult
      corfu
      crdt
      (csv "csv-mode")
      (custom cus-edit)
      cus-theme
      dape
      dashboard
      daemons
      deadgrep
      debbugs
      debug
      devdocs
      dictionary
      diff-hl
      diff-mode
      dired
      dired-sidebar
      disk-usage
      distel
      doc-view
      docker
      eat
      ebib
      ebuku
      edbi
      edebug
      ediff
      eglot
      elpaca
      ement
      explain-pause-mode
      eldoc
      elfeed
      elisp-mode
      elisp-refs
      elisp-slime-nav
      embark
      emms
      ,@(if (> emacs-major-version 28) '(emoji))
      epa
      ert
      eshell
      eval-sexp-fu
      evil-mc
      eww
      fanyi
      finder
      flycheck
      flymake
      forge
      free-keys
      geiser
      ggtags
      git-timemachine
      gited
      gnus
      go-mode
      gptel
      grep
      guix
      hackernews
      helm
      help
      helpful
      hg-histedit
      hungry-delete
      hyrolo
      ibuffer
      (image image-mode)
      image-dired
      image+
      imenu
      imenu-list
      (indent "indent")
      indium
      info
      ivy
      js2-mode
      ,@(if (>= emacs-major-version 30) '(kmacro))
      leetcode
      lispy
      lms
      log-edit
      log-view
      lsp-ui-imenu
      lua-mode
      kotlin-mode
      macrostep
      man
      (magit magit-repos magit-submodule)
      magit-repos
      magit-section
      magit-todos
      markdown-mode
      monky
      mpc
      mpdel
      mu4e
      mu4e-conversation
      neotree
      newsticker
      notmuch
      nov
      omnisharp
      org
      org-present
      org-roam
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
      p-search
      python
      quickrun
      racer
      racket-describe
      realgud
      reftex
      replace
      restclient
      rg
      ripgrep
      rjsx-mode
      robe
      rtags
      ruby-mode
      scheme
      scroll-lock
      selectrum
      sh-script
      ,@(if (> emacs-major-version 27) '(shortdoc))
      simple
      simple-mpc
      slime
      sly
      smerge-mode
      snake
      so-long
      speedbar
      tab-bar
      tablist
      tar-mode
      telega
      (term term ansi-term multi-term)
      tetris
      thread
      tide
      timer-list
      transmission
      trashed
      tuareg
      typescript-mode
      vc-annotate
      vc-dir
      vc-git
      vdiff
      vertico
      view
      vlf
      vterm
      vundo
      w3m
      wdired
      wgrep
      which-key
      with-editor
      woman
      xref
      xwidget
      yaml-mode
      youtube-dl
      zmusic
      (ztree ztree-diff)))

  (defun +evil-collection-init (module &optional disabled-list)
    "Initialize evil-collection-MODULE.

Unlike `evil-collection-init', this respects `+evil-collection-disabled-list',
and complains if a module is loaded too early (during startup)."
    (unless (memq (or (car-safe module) module) disabled-list)
      (doom-log "editor:evil: loading evil-collection-%s %s"
                (or (car-safe module) module)
                (if after-init-time "" "(too early!)"))
      (with-demoted-errors "evil-collection error: %s"
        (evil-collection-init (list module)))))

  (defadvice! +evil-collection-disable-blacklist-a (fn)
    :around #'evil-collection-vterm-toggle-send-escape  ; allow binding to ESC
    (let (evil-collection-key-blacklist)
      (funcall-interactively fn)))

  ;; These modes belong to packages that Emacs always loads at startup, causing
  ;; evil-collection and it's co-packages to all load immediately. We avoid this
  ;; by loading them after evil-collection has first loaded...
  (with-eval-after-load 'evil-collection
    ;; Don't let evil-collection interfere with certain keys
    (setq evil-collection-key-blacklist
          (append (list doom-leader-key doom-localleader-key
                        doom-leader-alt-key)
                  evil-collection-key-blacklist
                  (when (modulep! :tools lookup)
                    '("gd" "gf" "K"))
                  (when (modulep! :tools eval)
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
    (add-transient-hook! 'help-mode
      (+evil-collection-init 'help))
    (add-transient-hook! 'Buffer-menu-mode
      (+evil-collection-init '(buff-menu "buff-menu")))
    (add-transient-hook! 'calc-mode
      (+evil-collection-init 'calc))
    (add-transient-hook! 'image-mode
      (+evil-collection-init 'image))
    (add-transient-hook! 'emacs-lisp-mode
      (+evil-collection-init 'elisp-mode))
    (add-transient-hook! 'occur-mode
      (+evil-collection-init 'replace))
    (add-transient-hook! 'indent-rigidly
      (+evil-collection-init '(indent "indent")))
    (when (>= emacs-major-version 30)
      (add-transient-hook! 'kmacro-menu-mode
        (+evil-collection-init 'kmacro)))
    (add-transient-hook! 'minibuffer-setup-hook
      (when evil-collection-setup-minibuffer
        (+evil-collection-init 'minibuffer)
        (evil-collection-minibuffer-insert)))
    (add-transient-hook! 'process-menu-mode
      (+evil-collection-init '(process-menu simple)))
    (add-transient-hook! 'shortdoc-mode
      (+evil-collection-init 'shortdoc))
    (add-transient-hook! 'tabulated-list-mode
      (+evil-collection-init 'tabulated-list))
    (add-transient-hook! 'tab-bar-mode
      (+evil-collection-init 'tab-bar))

    ;; HACK Do this ourselves because evil-collection break's `eval-after-load'
    ;;      load order by loading their target plugin before applying keys. This
    ;;      makes it hard for end-users to overwrite these keybinds with a
    ;;      simple `after!' or `with-eval-after-load'.
    (dolist (mode evil-collection-mode-list)
      (dolist (req (or (cdr-safe mode) (list mode)))
        (with-eval-after-load req
          (+evil-collection-init mode +evil-collection-disabled-list))))))
