;;; lang/org/contrib/roam2.el -*- lexical-binding: t; -*-
;;;###if (featurep! +roam2)

(defvar +org-roam-open-buffer-on-find-file t
  "If non-nil, open the org-roam buffer when opening an org roam file.")

(defvar +org-roam-link-to-org-use-id 'create-if-interactive
  "`org-roam-directory' local value for `org-id-link-to-org-use-id'.
It's not recommended to set this to nil in order for other parts
of org-mode to properly utilize ID links.")


;;
;;; Packages

(use-package! org-roam
  :hook (org-load . +org-init-roam-h)
  :preface
  ;; Set this to nil so we can later detect if the user has set custom values
  ;; for these variables. If not, default values will be set in the :config
  ;; section.
  (defvar org-roam-directory nil)
  (defvar org-roam-db-location nil)

  :init
  (doom-load-packages-incrementally
   '(ansi-color dash f rx seq magit-section emacsql emacsql-sqlite))

  ;; Don't display warning message dedicated for v1 users. Need to be set early.
  (setq org-roam-v2-ack t)

  (defadvice! +org-roam-suppress-sqlite-build-a (fn &rest args)
    "Suppress automatic building of sqlite3 binary when loading `org-roam'.
This is a blocking operation that can take a while to complete
and better be deferred when there will be an actual demand for
the database. See `+org-init-roam-h' for the launch process."
    :around #'emacsql-sqlite-ensure-binary
    (if (not (boundp 'org-roam-db-version))
        (apply fn args)
      (advice-remove #'emacsql-sqlite-ensure-binary #'+org-roam-suppress-sqlite-build-a)
      nil))

  :config
  (defun +org-init-roam-h ()
    "Setup `org-roam' but don't immediately initialize its database.
Instead, initialize it when it will be actually needed."
    (letf! ((#'org-roam-db-sync #'ignore))
      (org-roam-setup))
    (defadvice! +org-roam-try-init-db-a (&rest _)
      "Try to initialize org-roam database at the last possible safe moment.
In case of failure, fail gracefully."
      :before #'org-roam-db-query
      (message "Initializing org-roam database...")
      (let ((run-cleanup-p t))
        (unwind-protect
            ;; Try to build the binary if it doesn't exist. In case of failure
            ;; this will error, run the cleanup and exit, and in case of success
            ;; this will return nil and sync the database.
            (setq run-cleanup-p (emacsql-sqlite-ensure-binary))
          (when run-cleanup-p
            (setq org-roam--sqlite-available-p nil)
            (org-roam-teardown)
            (message (concat "EmacSQL failied to build SQLite binary for org-roam; "
                             "see *Compile-Log* buffer for details.\n"
                             "To try reinitialize org-roam, run \"M-x org-roam-setup\"")))))
      (advice-remove 'org-roam-db-query #'+org-roam-try-init-db-a)
      (org-roam-db-sync)))

  (setq org-roam-directory
        (thread-first (or org-roam-directory "roam")
          (expand-file-name org-directory)
          (file-truename)
          (file-name-as-directory))
        org-roam-db-location
        (or org-roam-db-location
            (concat doom-etc-dir "org-roam.db"))
        org-roam-node-display-template
        "${doom-hierarchy:*} ${doom-tags:45}"
        org-roam-completion-everywhere t
        org-roam-mode-section-functions
        #'(org-roam-backlinks-section
           org-roam-reflinks-section))

  (setq-hook! 'org-roam-find-file-hook
    org-id-link-to-org-use-id +org-roam-link-to-org-use-id)

  ;; Normally, the org-roam buffer doesn't open until you explicitly call
  ;; `org-roam'. If `+org-roam-open-buffer-on-find-file' is non-nil, the
  ;; org-roam buffer will be opened for you whenever you visit a file in
  ;; `org-roam-directory'.
  (add-hook! 'org-roam-find-file-hook :append
    (defun +org-roam-open-with-buffer-maybe-h ()
      (and +org-roam-open-buffer-on-find-file
           (not org-roam-capture--node) ;; don't proc for roam capture buffers
           (not org-capture-mode) ;; don't proc for normal capture buffers
           (not (eq 'visible (org-roam-buffer--visibility)))
           (org-roam-buffer-toggle))))

  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))

  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)

  (map! (:map org-mode-map
         :localleader
         :prefix ("m" . "org-roam")
         "D" #'org-roam-demote-entire-buffer
         "f" #'org-roam-node-find
         "F" #'org-roam-ref-find
         "g" #'org-roam-graph
         "i" #'org-roam-node-insert
         "I" #'org-id-get-create
         "m" #'org-roam-buffer-toggle
         "M" #'org-roam-buffer-display-dedicated
         "n" #'org-roam-capture
         "r" #'org-roam-refile
         "R" #'org-roam-link-replace-all
         (:prefix ("d" . "by date")
          :desc "Goto previous note" "b" #'org-roam-dailies-goto-previous-note
          :desc "Goto date"          "d" #'org-roam-dailies-goto-date
          :desc "Capture date"       "D" #'org-roam-dailies-capture-date
          :desc "Goto next note"     "f" #'org-roam-dailies-goto-next-note
          :desc "Goto tomorrow"      "m" #'org-roam-dailies-goto-tomorrow
          :desc "Capture tomorrow"   "M" #'org-roam-dailies-capture-tomorrow
          :desc "Capture today"      "n" #'org-roam-dailies-capture-today
          :desc "Goto today"         "t" #'org-roam-dailies-goto-today
          :desc "Capture today"      "T" #'org-roam-dailies-capture-today
          :desc "Goto yesterday"     "y" #'org-roam-dailies-goto-yesterday
          :desc "Capture yesterday"  "Y" #'org-roam-dailies-capture-yesterday
          :desc "Find directory"     "-" #'org-roam-dailies-find-directory)
         (:prefix ("o" . "node properties")
          "a" #'org-roam-alias-add
          "A" #'org-roam-alias-remove
          "t" #'org-roam-tag-add
          "T" #'org-roam-tag-remove
          "r" #'org-roam-ref-add
          "R" #'org-roam-ref-remove)))

  (when (featurep! :editor evil +everywhere)
    (add-hook! 'org-roam-mode-hook
      (defun +org-roam-detach-magit-section-mode-map-h ()
        "Detach `magit-section-mode-map' from `org-roam-mode-map'.
Inheriting its keymaps introduces a lot of conflicts in
`org-roam-mode' based buffers, where Evil and leader keybindings
will become completely overridden. This is because `magit-section'
uses 'keymap text-property to attach section-unique keymaps, which
has a higher level of precedence than `emulation-mode-map-alists'.

Note: We do this each time through the hook, because otherwise
sections seems to ignore the detachment."
        (set-keymap-parent org-roam-mode-map nil)))

    (map! :map org-roam-mode-map
          :nv "]"       #'magit-section-forward-sibling
          :nv "["       #'magit-section-backward-sibling
          :nv "gj"      #'magit-section-forward-sibling
          :nv "gk"      #'magit-section-backward-sibling
          :nv "gr"      #'revert-buffer
          :nv "gR"      #'revert-buffer
          :nv "z1"      #'magit-section-show-level-1
          :nv "z2"      #'magit-section-show-level-2
          :nv "z3"      #'magit-section-show-level-3
          :nv "z4"      #'magit-section-show-level-4
          :nv "za"      #'magit-section-toggle
          :nv "zc"      #'magit-section-hide
          :nv "zC"      #'magit-section-hide-children
          :nv "zo"      #'magit-section-show
          :nv "zO"      #'magit-section-show-children
          :nv "zr"      #'magit-section-show-level-4-all
          :nv "C-j"     #'magit-section-forward
          :nv "C-k"     #'magit-section-backward
          :g  "M-p"     #'magit-section-backward-sibling
          :g  "M-n"     #'magit-section-forward-sibling
          :g  [tab]     #'magit-section-toggle
          :g  [C-tab]   #'magit-section-cycle
          :g  [backtab] #'magit-section-cycle-global)))


;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package! org-roam-protocol
  :after org-protocol)
