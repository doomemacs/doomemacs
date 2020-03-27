;;; lang/org/config.el -*- lexical-binding: t; -*-

(defvar +org-babel-mode-alist
  '((cpp . C)
    (C++ . C)
    (D . C)
    (sh . shell)
    (bash . shell)
    (matlab . octave)
    (amm . ammonite))
  "An alist mapping languages to babel libraries. This is necessary for babel
libraries (ob-*.el) that don't match the name of the language.

For example, with (fish . shell) will cause #+BEGIN_SRC fish to load ob-shell.el
when executed.")

(defvar +org-babel-load-functions ()
  "A list of functions executed to load the current executing src block. They
take one argument (the language specified in the src block, as a string). Stops
at the first function to return non-nil.")

(defvar +org-capture-todo-file "todo.org"
  "Default target for todo entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-changelog-file "changelog.org"
  "Default target for changelog entries.

Is relative to `org-directory' unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-notes-file "notes.org"
  "Default target for storing notes.

Used as a fall back file for org-capture.el, for templates that do not specify a
target file.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-journal-file "journal.org"
  "Default target for storing timestamped journal entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-projects-file "projects.org"
  "Default, centralized target for org-capture templates.")

(defvar +org-initial-fold-level 2
  "The initial fold level of org files when no #+STARTUP options for it.")

(defvar +org-habit-graph-padding 2
  "The padding added to the end of the consistency graph")

(defvar +org-habit-min-width 30
  "Hides the consistency graph if the `org-habit-graph-column' is less than this value")

(defvar +org-habit-graph-window-ratio 0.3
  "The ratio of the consistency graphs relative to the window width")


;;
;;; `org-load' hooks

(defun +org-init-agenda-h ()
  (unless org-agenda-files
    (setq org-agenda-files (list org-directory)))
  (setq-default
   ;; Don't monopolize the whole frame just for the agenda
   org-agenda-window-setup 'current-window
   org-agenda-inhibit-startup t
   org-agenda-skip-unavailable-files t
   ;; Move the agenda to show the previous 3 days and the next 7 days for a bit
   ;; better context instead of just the current week which is a bit confusing
   ;; on, for example, a sunday
   org-agenda-span 10
   org-agenda-start-on-weekday nil
   org-agenda-start-day "-3d"))


(defun +org-init-appearance-h ()
  "Configures the UI for `org-mode'."
  (setq org-indirect-buffer-display 'current-window
        org-eldoc-breadcrumb-separator " → "
        org-enforce-todo-dependencies t
        org-entities-user
        '(("flat"  "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-footnote-auto-label 'plain
        org-hide-leading-stars t
        org-hide-leading-stars-before-indent-mode t
        org-image-actual-width nil
        org-list-description-max-indent 4
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        org-startup-indented t
        org-tags-column 0
        org-use-sub-superscripts '{})

  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3))
        ;; Without this, completers like ivy/helm are only given the first level of
        ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
        ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
        ;; target! e.g. FILE/Tasks/heading/subheading
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  ;; Fontify latex blocks and entities, but not natively -- that's too slow
  (setq org-highlight-latex-and-related '(latex script entities))
  (plist-put! org-format-latex-options
              :scale 1.5         ; larger previews
              :foreground 'auto  ; match the theme foreground
              :background 'auto) ; ... and its background

  ;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
  ;;      underlying faces like the `org-todo' face does, so we define our own
  ;;      intermediary faces that extend from org-todo.
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) ""))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; An ongoing project that cannot be completed in one step
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something is holding up this task; or it is paused
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")) ; Task was completed
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)))

  (defadvice! +org-display-link-in-eldoc-a (orig-fn &rest args)
    "Display full link in minibuffer when cursor/mouse is over it."
    :around #'org-eldoc-documentation-function
    (or (when-let (link (org-element-property :raw-link (org-element-context)))
          (format "Link: %s" link))
        (apply orig-fn args)))

  ;; Automatic indent detection in org files is meaningless
  (add-to-list 'doom-detect-indentation-excluded-modes 'org-mode)

  (set-pretty-symbols! 'org-mode
    :name "#+NAME:"
    :src_block "#+BEGIN_SRC"
    :src_block_end "#+END_SRC"))


(defun +org-init-babel-h ()
  (setq org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t     ; we do this ourselves
        ;; You don't need my permission (just be careful, mkay?)
        org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function nil
        ;; Show src buffer in popup, and don't monopolize the frame
        org-src-window-setup 'other-window
        ;; Our :lang common-lisp module uses sly, so...
        org-babel-lisp-eval-fn #'sly-eval)

  ;; Add convenience lang alias for markdown blocks
  (add-to-list 'org-src-lang-modes '("md" . markdown))

  ;; I prefer C-c C-c over C-c ' (more consistent)
  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  (defadvice! +org-fix-newline-and-indent-in-src-blocks-a ()
    "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
    :after #'org-return-indent
    (when (org-in-src-block-p t)
      (org-babel-do-in-edit-buffer
       (call-interactively #'indent-for-tab-command))))

  ;; Refresh inline images after executing src blocks (useful for plantuml or
  ;; ipython, where the result could be an image)
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

  ;; Fix 'require(...).print is not a function' error from `ob-js' when
  ;; executing JS src blocks
  (setq org-babel-js-function-wrapper "console.log(require('util').inspect(function(){\n%s\n}()));"))


(defun +org-init-babel-lazy-loader-h ()
  "Load babel libraries lazily when babel blocks are executed."
  (defun +org--babel-lazy-load (lang)
    (cl-check-type lang symbol)
    (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
        (require (intern (format "ob-%s" lang)) nil t)
        (require lang nil t)))

  (defadvice! +org--src-lazy-load-library-a (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    :before #'org-src--get-lang-mode
    (or (cdr (assoc lang org-src-lang-modes))
        (+org--babel-lazy-load lang)))

  ;; This also works for tangling and exporting
  (defadvice! +org--babel-lazy-load-library-a (info)
    "Load babel libraries lazily when babel blocks are executed."
    :after-while #'org-babel-confirm-evaluate
    (let* ((lang (nth 0 info))
           (lang (cond ((symbolp lang) lang)
                       ((stringp lang) (intern lang))))
           (lang (or (cdr (assq lang +org-babel-mode-alist))
                     lang)))
      (when (and lang
                 (not (cdr (assq lang org-babel-load-languages)))
                 (+org--babel-lazy-load lang))
        (when (assq :async (nth 2 info))
          ;; ob-async has its own agenda for lazy loading packages (in the
          ;; child process), so we only need to make sure it's loaded.
          (require 'ob-async nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))
      t))

  (advice-add #'org-babel-do-load-languages :override #'ignore))


(defun +org-init-capture-defaults-h ()
  "Sets up some reasonable defaults, as well as two `org-capture' workflows that
I like:

1. The traditional way: invoking `org-capture' directly, via SPC X, or through
   the :cap ex command.
2. Through a org-capture popup frame that is invoked from outside Emacs (the
   ~/.emacs.d/bin/org-capture script). This can be invoked from qutebrowser,
   vimperator, dmenu or a global keybinding."
  (setq org-default-notes-file
        (expand-file-name +org-capture-notes-file org-directory)
        +org-capture-journal-file
        (expand-file-name +org-capture-journal-file org-directory)
        org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* [ ] %?\n%i\n%a" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {org-directory}/{+org-capture-projects-file} and store
          ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
          ;; support `:parents' to specify what headings to put them under, e.g.
          ;; :parents ("Projects")
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +org-capture-central-project-todo-file)
           "* TODO %?\n %i\n %a"
           :heading "Tasks"
           :prepend nil)
          ("on" "Project notes" entry
           (function +org-capture-central-project-notes-file)
           "* %U %?\n %i\n %a"
           :heading "Notes"
           :prepend t)
          ("oc" "Project changelog" entry
           (function +org-capture-central-project-changelog-file)
           "* %U %?\n %i\n %a"
           :heading "Changelog"
           :prepend t)))

  ;; Kill capture buffers by default (unless they've been visited)
  (after! org-capture
    (org-capture-put :kill-buffer t))

  ;; HACK Doom doesn't support `customize'. Best not to advertise it as an
  ;;      option in `org-capture's menu.
  (defadvice! +org--remove-customize-option-a (orig-fn table title &optional prompt specials)
    :around #'org-mks
    (funcall orig-fn table title prompt (remove '("C" "Customize org-capture-templates") specials)))

  (defadvice! +org--capture-expand-variable-file-a (file)
    "If a variable is used for a file path in `org-capture-template', it is used
as is, and expanded relative to `default-directory'. This changes it to be
relative to `org-directory', unless it is an absolute path."
    :filter-args #'org-capture-expand-file
    (if (and (symbolp file) (boundp file))
        (expand-file-name (symbol-value file) org-directory)
      file))

  (defadvice! +org--prevent-save-prompts-when-refiling-a (&rest _)
    "Fix #462: when refiling from org-capture, Emacs prompts to kill the
underlying, modified buffer. This fixes that."
    :after 'org-refile
    (when (bound-and-true-p org-capture-is-refiling)
      (org-save-all-org-buffers)))

  (add-hook! 'org-capture-mode-hook
    (defun +org-show-target-in-capture-header-h ()
      (setq header-line-format
            (format "%s%s%s"
                    (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
                                'face 'font-lock-string-face)
                    org-eldoc-breadcrumb-separator
                    header-line-format))))

  (when (featurep! :editor evil)
    (add-hook 'org-capture-mode-hook #'evil-insert-state)))


(defun +org-init-capture-frame-h ()
  (add-hook 'org-capture-after-finalize-hook #'+org-capture-cleanup-frame-h)

  (when (featurep! :ui doom-dashboard)
    (add-hook '+doom-dashboard-inhibit-functions #'+org-capture-frame-p)))


(defun +org-init-attachments-h ()
  "Sets up org's attachment system."
  ;; Centralized attachments directory
  (setq org-attach-id-dir (doom-path org-directory org-attach-id-dir)
        ;; Store a link to attachments when they are attached
        org-attach-store-link-p t
        ;; Inherit attachment properties from parent nodes
        org-attach-use-inheritance t)
  (after! projectile
    (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir)))


(defun +org-init-custom-links-h ()
  ;; Highlight broken file links
  (org-link-set-parameters
   "file"
   :face (lambda (path)
           (if (or (file-remote-p path)
                   (file-exists-p path))
               'org-link
             'error)))

  ;; Add custom link types
  (pushnew! org-link-abbrev-alist
            '("github"      . "https://github.com/%s")
            '("youtube"     . "https://youtube.com/watch?v=%s")
            '("google"      . "https://google.com/search?q=")
            '("gimages"     . "https://google.com/images?q=%s")
            '("gmap"        . "https://maps.google.com/maps?q=%s")
            '("duckduckgo"  . "https://duckduckgo.com/?q=%s")
            '("wolfram"     . "https://wolframalpha.com/input/?i=%s")
            '("doom-repo"   . "https://github.com/hlissner/doom-emacs/%s"))

  (+org-def-link "org" org-directory)
  (+org-def-link "doom" doom-emacs-dir)
  (+org-def-link "doom-docs" doom-docs-dir)
  (+org-def-link "doom-modules" doom-modules-dir)

  ;; Allow inline image previews of http(s)? urls or data uris
  (org-link-set-parameters "http"  :image-data-fun #'+org-image-link)
  (org-link-set-parameters "https" :image-data-fun #'+org-image-link)
  (org-link-set-parameters "img"   :image-data-fun #'+org-inline-data-image)

  ;; Add support for youtube links + previews
  (require 'org-yt nil t))


(defun +org-init-export-h ()
  "TODO"
  (setq org-export-with-smart-quotes t
        org-html-validation-link nil)

  (when (featurep! :lang markdown)
    (add-to-list 'org-export-backends 'md))

  (use-package! ox-hugo
    :when (featurep! +hugo)
    :after ox)

  (use-package! ox-pandoc
    :when (featurep! +pandoc)
    :when (executable-find "pandoc")
    :after ox
    :init
    (add-to-list 'org-export-backends 'pandoc)
    (setq org-pandoc-options
          '((standalone . t)
            (mathjax . t)
            (variable . "revealjs-url=https://revealjs.com")))))


(defun +org-init-habit-h ()
  "TODO"
  (add-hook! 'org-agenda-mode-hook
    (defun +org-habit-resize-graph-h ()
      "Right align and resize the consistency graphs based on
`+org-habit-graph-window-ratio'"
      (require 'org-habit)
      (let* ((total-days (float (+ org-habit-preceding-days org-habit-following-days)))
             (preceding-days-ratio (/ org-habit-preceding-days total-days))
             (graph-width (floor (* (window-width) +org-habit-graph-window-ratio)))
             (preceding-days (floor (* graph-width preceding-days-ratio)))
             (following-days (- graph-width preceding-days))
             (graph-column (- (window-width) (+ preceding-days following-days)))
             (graph-column-adjusted (if (> graph-column +org-habit-min-width)
                                        (- graph-column +org-habit-graph-padding)
                                      nil)))
        (setq-local org-habit-preceding-days preceding-days)
        (setq-local org-habit-following-days following-days)
        (setq-local org-habit-graph-column graph-column-adjusted)))))


(defun +org-init-hacks-h ()
  "Getting org to behave."
  ;; Don't open separate windows
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  ;; Open directory links in Emacs
  (add-to-list 'org-file-apps '(directory . emacs))

  ;; When you create a sparse tree and `org-indent-mode' is enabled, the
  ;; highlighting destroys the invisibility added by `org-indent-mode'.
  ;; Therefore, don't highlight when creating a sparse tree.
  (setq org-highlight-sparse-tree-matches nil)

  (add-hook! 'org-follow-link-hook
    (defun +org-delayed-recenter-h ()
      "`recenter', but after a tiny delay. Necessary to prevent certain race
conditions where a window's buffer hasn't changed at the time this hook is run."
      (run-at-time 0.1 nil #'recenter)))

  (defadvice! +org--strip-properties-from-outline-a (orig-fn path &optional width prefix separator)
    "Remove link syntax and fix variable height text (e.g. org headings) in the
eldoc string."
    :around #'org-format-outline-path
    (funcall orig-fn
             (cl-loop for part in path
                      ;; Remove full link syntax
                      for fixedpart = (replace-regexp-in-string org-link-any-re "\\4" part)
                      for n from 0
                      for face = (nth (% n org-n-level-faces) org-level-faces)
                      collect
                      (org-add-props fixedpart
                          nil 'face `(:foreground ,(face-foreground face nil t) :weight bold)))
             width prefix separator))

  (defun +org--restart-mode-h ()
    "Restart `org-mode', but only once."
    (quiet! (org-mode-restart))
    (delq! (current-buffer) org-agenda-new-buffers)
    (remove-hook 'doom-switch-buffer-hook #'+org--restart-mode-h
                 'local))

  (add-hook! 'org-agenda-finalize-hook
    (defun +org-exclude-agenda-buffers-from-workspace-h ()
      "Prevent temporarily-opened agenda buffers from being associated with the
current workspace (and clean them up)."
      (when (and org-agenda-new-buffers (bound-and-true-p persp-mode))
        (unless org-agenda-sticky
          (let (persp-autokill-buffer-on-remove)
            (persp-remove-buffer org-agenda-new-buffers
                                 (get-current-persp)
                                 nil)))
        (dolist (buffer org-agenda-new-buffers)
          (with-current-buffer buffer
            ;; HACK Org agenda opens temporary agenda incomplete org-mode
            ;;      buffers. These are great for extracting agenda information
            ;;      from, but what if the user tries to visit one of these
            ;;      buffers? Then we remove it from the to-be-cleaned queue and
            ;;      restart `org-mode' so they can grow up to be full-fledged
            ;;      org-mode buffers.
            (add-hook 'doom-switch-buffer-hook #'+org--restart-mode-h
                      nil 'local))))))

  (defadvice! +org--exclude-agenda-buffers-from-recentf-a (orig-fn file)
    "Prevent temporarily opened agenda buffers from polluting recentf."
    :around #'org-get-agenda-file-buffer
    (let ((recentf-exclude (list (lambda (_file) t))))
      (funcall orig-fn file)))

  ;; HACK With https://code.orgmode.org/bzg/org-mode/commit/48da60f4, inline
  ;;      image previews broke for users with imagemagick support built in. This
  ;;      reverses the problem, but should be removed once it is addressed
  ;;      upstream (if ever).
  (defadvice! +org--fix-inline-images-for-imagemagick-users-a (orig-fn &rest args)
    :around #'org-display-inline-images
    (cl-letf* ((old-create-image (symbol-function #'create-image))
               ((symbol-function #'create-image)
                (lambda (file-or-data &optional type data-p &rest props)
                  (let ((type (if (plist-get props :width) type)))
                    (apply old-create-image file-or-data type data-p props)))))
      (apply orig-fn args))))


(defun +org-init-keybinds-h ()
  "Sets up org-mode and evil keybindings. Tries to fix the idiosyncrasies
between the two."
  (add-hook 'doom-escape-hook #'+org-remove-occur-highlights-h)

  ;; C-a & C-e act like `doom/backward-to-bol-or-indent' and
  ;; `doom/forward-to-last-non-comment-or-eol', but with more org awareness.
  (setq org-special-ctrl-a/e t)

  (setq org-M-RET-may-split-line nil
        ;; insert new headings after current subtree rather than inside it
        org-insert-heading-respect-content t)

  (add-hook! 'org-tab-first-hook
             #'+org-yas-expand-maybe-h
             #'+org-indent-maybe-h)

  (add-hook 'doom-delete-backward-functions
            #'+org-delete-backward-char-and-realign-table-maybe-h)

  (map! :map org-mode-map
        "C-c C-S-l"  #'+org/remove-link
        "C-c C-i"    #'org-toggle-inline-images
        ;; textmate-esque newline insertion
        [C-return]   #'+org/insert-item-below
        [C-S-return] #'+org/insert-item-above
        [C-M-return] #'org-insert-subheading
        (:when IS-MAC
          [s-return]   #'+org/insert-item-below
          [s-S-return] #'+org/insert-item-above
          [s-M-return] #'org-insert-subheading)
        ;; Org-aware C-a/C-e
        [remap doom/backward-to-bol-or-indent]          #'org-beginning-of-line
        [remap doom/forward-to-last-non-comment-or-eol] #'org-end-of-line

        :localleader
        "'" #'org-edit-special
        "," #'org-switchb
        "." #'org-goto
        (:when (featurep! :completion ivy)
          "." #'counsel-org-goto
          "/" #'counsel-org-goto-all)
        (:when (featurep! :completion helm)
          "." #'helm-org-in-buffer-headings
          "/" #'helm-org-agenda-files-headings)
        "A" #'org-archive-subtree
        "d" #'org-deadline
        "e" #'org-export-dispatch
        "f" #'org-footnote-new
        "h" #'org-toggle-heading
        "i" #'org-toggle-item
        "I" #'org-toggle-inline-images
        "n" #'org-store-link
        "o" #'org-set-property
        "p" #'org-priority
        "q" #'org-set-tags-command
        "s" #'org-schedule
        "t" #'org-todo
        "T" #'org-todo-list
        (:prefix ("a" . "attachments")
          "a" #'org-attach
          "d" #'org-attach-delete-one
          "D" #'org-attach-delete-all
          "f" #'+org/find-file-in-attachments
          "l" #'+org/attach-file-and-insert-link
          "n" #'org-attach-new
          "o" #'org-attach-open
          "O" #'org-attach-open-in-emacs
          "r" #'org-attach-reveal
          "R" #'org-attach-reveal-in-emacs
          "u" #'org-attach-url
          "s" #'org-attach-set-directory
          "S" #'org-attach-sync
          (:when (featurep! +dragndrop)
            "c" #'org-download-screenshot
            "y" #'org-download-yank))
        (:prefix ("b" . "tables")
          "-" #'org-table-insert-hline
          "a" #'org-table-align
          "c" #'org-table-create-or-convert-from-region
          "e" #'org-table-edit-field
          "h" #'org-table-field-info
          (:when (featurep! +gnuplot)
            "p" #'org-plot/gnuplot))
        (:prefix ("c" . "clock")
          "c" #'org-clock-in
          "C" #'org-clock-out
          "d" #'org-clock-mark-default-task
          "e" #'org-clock-modify-effort-estimate
          "E" #'org-set-effort
          "l" #'org-clock-in-last
          "g" #'org-clock-goto
          "G" (λ! (org-clock-goto 'select))
          "r" #'org-clock-report
          "x" #'org-clock-cancel
          "=" #'org-clock-timestamps-up
          "-" #'org-clock-timestamps-down)
        (:prefix ("g" . "goto")
          "g" #'org-goto
          (:when (featurep! :completion ivy)
            "g" #'counsel-org-goto
            "G" #'counsel-org-goto-all)
          (:when (featurep! :completion helm)
            "g" #'helm-org-in-buffer-headings
            "G" #'helm-org-agenda-files-headings)
          "c" #'org-clock-goto
          "C" (λ! (org-clock-goto 'select))
          "i" #'org-id-goto
          "r" #'org-refile-goto-last-stored
          "v" #'+org/goto-visible
          "x" #'org-capture-goto-last-stored)
        (:prefix ("l" . "links")
          "c" #'org-cliplink
          "l" #'org-insert-link
          "L" #'org-insert-all-links
          "s" #'org-store-link
          "S" #'org-insert-last-stored-link
          "i" #'org-id-store-link
          "d" #'+org/remove-link)
        (:prefix ("r" . "refile")
          "." #'+org/refile-to-current-file
          "c" #'+org/refile-to-running-clock
          "l" #'+org/refile-to-last-location
          "f" #'+org/refile-to-file
          "o" #'+org/refile-to-other-window
          "O" #'+org/refile-to-other-buffer
          "v" #'+org/refile-to-visible
          "r" #'org-refile)) ; to all `org-refile-targets'

  (map! :after org-agenda
        :map org-agenda-mode-map
        :m "C-SPC" #'org-agenda-show-and-scroll-up
        :localleader
        "d" #'org-agenda-deadline
        (:prefix ("c" . "clock")
          "c" #'org-agenda-clock-in
          "C" #'org-agenda-clock-out
          "g" #'org-agenda-clock-goto
          "r" #'org-agenda-clockreport-mode
          "s" #'org-agenda-show-clocking-issues
          "x" #'org-agenda-clock-cancel)
        "q" #'org-agenda-set-tags
        "r" #'org-agenda-refile
        "s" #'org-agenda-schedule
        "t" #'org-agenda-todo))


(defun +org-init-popup-rules-h ()
  (set-popup-rules!
    '(("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
      ("^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)"
       :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
      ("^\\*Org \\(?:Select\\|Attach\\)" :slot -1 :vslot -2 :ttl 0 :size 0.25)
      ("^\\*Org Agenda"    :ignore t)
      ("^\\*Org Src"       :size 0.4  :quit nil :select t :autosave t :modeline t :ttl nil)
      ("^CAPTURE-.*\\.org$" :size 0.25 :quit nil :select t :autosave t))))


(defun +org-init-protocol-lazy-loader-h ()
  "Brings lazy-loaded support for org-protocol, so external programs (like
browsers) can invoke specialized behavior from Emacs. Normally you'd simply
require `org-protocol' and use it, but the package loads all of org for no
compelling reason, so..."
  (defadvice! +org--server-visit-files-a (args)
    "Advise `server-visit-flist' to invoke `org-protocol' lazily."
    :filter-args #'server-visit-files
    (cl-destructuring-bind (files proc &optional nowait) args
      (catch 'greedy
        (let ((flist (reverse files)))
          (dolist (var flist)
            (when (string-match-p ":/+" (car var))
              (require 'server)
              (require 'org-protocol)
              ;; `\' to `/' on windows
              (let ((fname (org-protocol-check-filename-for-protocol
                            (expand-file-name (car var))
                            (member var flist)
                            proc)))
                (cond ((eq fname t) ; greedy? We need the t return value.
                       (setq files nil)
                       (throw 'greedy t))
                      ((stringp fname) ; probably filename
                       (setcar var fname))
                      ((setq files (delq var files)))))))))
      (list files proc nowait)))

  ;; Disable built-in, clumsy advice
  (after! org-protocol
    (ad-disable-advice 'server-visit-files 'before 'org-protocol-detect-protocol-server)))


(defun +org-init-protocol-h ()
  ;; TODO org-board or better link grabbing support
  ;; TODO org-capture + org-protocol instead of bin/org-capture
  )


(defun +org-init-smartparens-h ()
  "TODO"
  (after! smartparens
    (defun +org-sp-point-in-checkbox-p (_id action _context)
      (and (eq action 'insert)
           (sp--looking-at-p "\\s-*]")))

    (defun +org-sp-point-at-bol-p (_id action _context)
      (and (eq action 'insert)
           (save-excursion
             (skip-chars-backward "*")
             (bolp))))

    (defun +org-sp-in-src-block-p (_id action _context)
      (and (eq action 'insert)
           (org-in-src-block-p)))

    ;; make delimiter auto-closing a little more conservative
    (sp-with-modes 'org-mode
      (sp-local-pair "*" "*" :unless '(:add sp-point-before-word-p sp-in-math-p +org-sp-point-at-bol-p +org-sp-in-src-block-p))
      (sp-local-pair "_" "_" :unless '(:add sp-point-before-word-p sp-in-math-p +org-sp-in-src-block-p))
      (sp-local-pair "/" "/" :unless '(:add sp-point-before-word-p sp-in-math-p +org-sp-point-in-checkbox-p +org-sp-in-src-block-p))
      (sp-local-pair "~" "~" :unless '(:add sp-point-before-word-p +org-sp-in-src-block-p))
      (sp-local-pair "=" "=" :unless '(:add sp-point-before-word-p sp-in-math-p +org-sp-in-src-block-p)))))


;;
;;; Packages

(use-package! toc-org ; auto-table of contents
  :hook (org-mode . toc-org-enable)
  :config (setq toc-org-hrefify-default "gh"))


(use-package! org-bullets ; "prettier" bullets
  :hook (org-mode . org-bullets-mode))


(use-package! org-crypt ; built-in
  :commands org-encrypt-entries
  :hook (org-reveal-start . org-decrypt-entry)
  :config
  (add-hook! 'org-mode-hook
    (add-hook 'before-save-hook 'org-encrypt-entries nil t))
  (add-to-list 'org-tags-exclude-from-inheritance "crypt")
  (setq org-crypt-key user-mail-address))


(use-package! org-clock ; built-in
  :commands org-clock-save
  :init
  (setq org-clock-persist-file (concat doom-etc-dir "org-clock-save.el"))
  (defadvice! +org--clock-load-a (&rest _)
    "Lazy load org-clock until its commands are used."
    :before '(org-clock-in
              org-clock-out
              org-clock-in-last
              org-clock-goto
              org-clock-cancel)
    (org-clock-load))
  :config
  (setq org-clock-persist 'history
        ;; Resume when clocking into task with open clock
        org-clock-in-resume t)
  (add-hook 'kill-emacs-hook #'org-clock-save))


(use-package! org-pdfview
  :when (featurep! :tools pdf)
  :commands org-pdfview-open
  :init
  (after! org
    (delete '("\\.pdf\\'" . default) org-file-apps)
    ;; org links to pdf files are opened in pdf-view-mode
    (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (_file link) (org-pdfview-open link))))
    ;; support for links to specific pages
    (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (_file link) (org-pdfview-open link))))))


(use-package! evil-org
  :when (featurep! :editor evil +everywhere)
  :hook (org-mode . evil-org-mode)
  :init
  (defvar evil-org-retain-visual-state-on-shift t)
  (defvar evil-org-special-o/O '(table-row))
  (defvar evil-org-use-additional-insert t)
  :config
  (evil-org-set-key-theme)
  (add-hook! 'org-tab-first-hook :append
             ;; Only fold the current tree, rather than recursively
             #'+org-cycle-only-current-subtree-h
             ;; Clear babel results if point is inside a src block
             #'+org-clear-babel-results-h)
  (map! :map evil-org-mode-map
        :ni [C-return]   #'+org/insert-item-below
        :ni [C-S-return] #'+org/insert-item-above
        ;; navigate table cells (from insert-mode)
        :i "C-l" (general-predicate-dispatch 'org-end-of-line
                   (org-at-table-p) 'org-table-next-field)
        :i "C-h" (general-predicate-dispatch 'org-beginning-of-line
                   (org-at-table-p) 'org-table-previous-field)
        :i "C-k" (general-predicate-dispatch 'org-up-element
                   (org-at-table-p) '+org/table-previous-row)
        :i "C-j" (general-predicate-dispatch 'org-down-element
                   (org-at-table-p) 'org-table-next-row)
        ;; moving/(de|pro)moting subtress & expanding tables (prepend/append columns/rows)
        :ni "C-S-l" #'org-shiftright
        :ni "C-S-h" #'org-shiftleft
        :ni "C-S-k" #'org-shiftup
        :ni "C-S-j" #'org-shiftdown
        ;; more intuitive RET keybinds
        :i [return] #'org-return-indent
        :i "RET"    #'org-return-indent
        :n [return] #'+org/dwim-at-point
        :n "RET"    #'+org/dwim-at-point
        ;; more vim-esque org motion keys (not covered by evil-org-mode)
        :m "]h"  #'org-forward-heading-same-level
        :m "[h"  #'org-backward-heading-same-level
        :m "]l"  #'org-next-link
        :m "[l"  #'org-previous-link
        :m "]c"  #'org-babel-next-src-block
        :m "[c"  #'org-babel-previous-src-block
        :n "gQ"  #'org-fill-paragraph
        :n "gr"  #'org-ctrl-c-ctrl-c
        :n "gR"  #'org-babel-execute-buffer
        ;; sensible vim-esque folding keybinds
        :n "za"  #'+org/toggle-fold
        :n "zA"  #'org-shifttab
        :n "zc"  #'+org/close-fold
        :n "zC"  #'outline-hide-subtree
        :n "zm"  #'+org/hide-next-fold-level
        :n "zn"  #'org-tree-to-indirect-buffer
        :n "zo"  #'+org/open-fold
        :n "zO"  #'outline-show-subtree
        :n "zr"  #'+org/show-next-fold-level
        :n "zR"  #'outline-show-all
        :n "zi"  #'org-toggle-inline-images

        :map org-read-date-minibuffer-local-map
        "C-h"   (λ! (org-eval-in-calendar '(calendar-backward-day 1)))
        "C-l"   (λ! (org-eval-in-calendar '(calendar-forward-day 1)))
        "C-k"   (λ! (org-eval-in-calendar '(calendar-backward-week 1)))
        "C-j"   (λ! (org-eval-in-calendar '(calendar-forward-week 1)))
        "C-S-h" (λ! (org-eval-in-calendar '(calendar-backward-month 1)))
        "C-S-l" (λ! (org-eval-in-calendar '(calendar-forward-month 1)))
        "C-S-k" (λ! (org-eval-in-calendar '(calendar-backward-year 1)))
        "C-S-j" (λ! (org-eval-in-calendar '(calendar-forward-year 1)))))


(use-package! evil-org-agenda
  :when (featurep! :editor evil +everywhere)
  :hook (org-agenda-mode . evil-org-agenda-mode)
  :config
  (evil-org-agenda-set-keys)
  (evil-define-key* 'motion evil-org-agenda-mode-map
    (kbd doom-leader-key) nil))


;;
;;; Bootstrap

(use-package! org
  :defer-incrementally
  calendar find-func format-spec org-macs org-compat org-faces org-entities
  org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
  org-capture
  :preface
  ;; Change org defaults (should be set before org loads)
  (setq org-directory "~/org/"
        org-attach-id-dir ".attach/"
        org-publish-timestamp-directory (concat doom-cache-dir "org-timestamps/")
        org-preview-latex-image-directory (concat doom-cache-dir "org-latex/"))

  (defvar org-modules
    '(;; ol-w3m
      ;; ol-bbdb
      ol-bibtex
      ;; ol-docview
      ;; ol-gnus
      ;; ol-info
      ;; ol-irc
      ;; ol-mhe
      ;; ol-rmail
      ;; ol-eww
      ))

  (add-hook 'org-mode-local-vars-hook #'eldoc-mode)

  (add-hook! 'org-mode-hook
             ;; `show-paren-mode' causes flickering with indent overlays made by
             ;; `org-indent-mode', so we turn off show-paren-mode altogether
             #'doom-disable-show-paren-mode-h
             ;; disable `show-trailing-whitespace'; shows a lot of false positives
             #'doom-disable-show-trailing-whitespace-h
             #'+org-enable-auto-reformat-tables-h
             #'+org-enable-auto-update-cookies-h
             #'+org-unfold-to-2nd-level-or-point-h)

  (add-hook! 'org-load-hook
             #'+org-init-appearance-h
             #'+org-init-agenda-h
             #'+org-init-attachments-h
             #'+org-init-babel-h
             #'+org-init-babel-lazy-loader-h
             #'+org-init-capture-defaults-h
             #'+org-init-capture-frame-h
             #'+org-init-custom-links-h
             #'+org-init-export-h
             #'+org-init-habit-h
             #'+org-init-hacks-h
             #'+org-init-keybinds-h
             #'+org-init-popup-rules-h
             #'+org-init-protocol-h
             #'+org-init-protocol-lazy-loader-h
             #'+org-init-smartparens-h)

  ;;; Custom org modules
  (if (featurep! +brain)     (load! "contrib/brain"))
  (if (featurep! +dragndrop) (load! "contrib/dragndrop"))
  (if (featurep! +ipython)   (load! "contrib/ipython"))
  (if (featurep! +journal)   (load! "contrib/journal"))
  (if (featurep! +jupyter)   (load! "contrib/jupyter"))
  (if (featurep! +pomodoro)  (load! "contrib/pomodoro"))
  (if (featurep! +present)   (load! "contrib/present"))

  ;; In case the user has eagerly loaded org from their configs
  (when (and (featurep 'org)
             (not doom-reloading-p)
             (not byte-compile-current-file))
    (message "`org' was already loaded by the time lang/org loaded, this may cause issues")
    (run-hooks 'org-load-hook))

  :config
  (setq org-archive-subtree-save-file-p t) ; save target buffer after archiving

  ;; Global ID state means we can have ID links anywhere. This is required for
  ;; `org-brain', however.
  (setq org-id-track-globally t
        org-id-locations-file (expand-file-name ".orgids" org-directory)
        org-id-locations-file-relative t)

  ;; HACK `org-id' doesn't check if `org-id-locations-file' exists or is
  ;;      writeable before trying to read/write to it.
  (defadvice! +org--fail-gracefully-a (&rest _)
    :before-while '(org-id-locations-save org-id-locations-load)
    (file-writable-p org-id-locations-file))

  (add-hook 'org-open-at-point-functions #'doom-set-jump-h))
