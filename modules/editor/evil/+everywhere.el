;;; editor/evil/+everywhere.el -*- lexical-binding: t; -*-

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
    help
    helm
    image
    kotlin-mode
    occur
    package-menu
    ruby-mode
    simple
    slime
    lispy)
  "A list of `evil-collection' modules to ignore. See the definition of this
variable for an explanation of the defaults (in comments). See
`evil-collection-mode-list' for a list of available options.")

(defvar evil-collection-setup-minibuffer nil)

;; We do this ourselves, and better.
(defvar evil-collection-want-unimpaired-p nil)

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
;;       (kill-new (prin1-to-string list)))))

(defvar evil-collection-mode-list
  `(2048-game
    ag
    alchemist
    anaconda-mode
    apropos
    arc-mode
    bookmark
    (buff-menu "buff-menu")
    calc
    calendar
    cider
    cmake-mode
    comint
    company
    compile
    (custom cus-edit)
    cus-theme
    daemons
    deadgrep
    debbugs
    debug
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
    imenu-list
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
    ,@(if evil-collection-setup-minibuffer '(minibuffer))
    monky
    mu4e
    mu4e-conversation
    neotree
    notmuch
    nov
    (occur replace)
    omnisharp
    outline
    p4
    (package-menu package)
    pass
    (pdf pdf-tools)
    popup
    proced
    process-menu
    prodigy
    profiler
    python
    quickrun
    racer
    realgud
    reftex
    restclient
    rjsx-mode
    robe
    rtags
    ruby-mode
    simple
    slime
    sly
    tablist
    tar-mode
    (term term ansi-term multi-term)
    tetris
    tide
    transmission
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
    youtube-dl
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


;;
;;; Bootstrap

;; These modes belong to packages that Emacs always loads at startup, causing
;; evil-collection to load immediately. We avoid this by loading them after
;; evil-collection has first loaded...
(with-eval-after-load 'evil-collection
  (mapc #'+evil-collection-init '(comint custom help)))

;; ...or on first invokation of their associated major/minor modes.
(add-transient-hook! 'Buffer-menu-mode
  (+evil-collection-init '(buff-menu "buff-menu")))
(add-transient-hook! 'image-mode
  (+evil-collection-init 'image))
(add-transient-hook! 'emacs-lisp-mode
  (+evil-collection-init 'elisp-mode))
(add-transient-hook! 'occur-mode
  (+evil-collection-init 'occur))

(evil-define-key* 'normal process-menu-mode-map
  "q" #'kill-current-buffer
  "d" #'process-menu-delete-process)

;; Don't overwrite the leader keys
(setq evil-collection-key-blacklist
      (list doom-leader-key doom-localleader-key
            doom-leader-alt-key doom-localleader-alt-key))

;; HACK Do this ourselves because evil-collection break's `eval-after-load' load
;;      order by loading their target plugin before applying keys. It'd be too
;;      much work to accommodate this eveywhere we want to bind our own evil
;;      keybinds.
(dolist (mode evil-collection-mode-list)
  (dolist (req (or (cdr-safe mode) (list mode)))
    (with-eval-after-load req
      (+evil-collection-init mode +evil-collection-disabled-list))))
