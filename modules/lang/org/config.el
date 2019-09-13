;;; lang/org/config.el -*- lexical-binding: t; -*-

;;
;;; `org-load' hooks

(defun +org-init-agenda-h ()
  (unless org-agenda-files
    (setq org-agenda-files (list org-directory)))
  (setq-default
   ;; Don't monopolize the whole frame just for the agenda
   org-agenda-window-setup 'current-window
   ;; Hide blocked tasks in the agenda view.
   org-agenda-dim-blocked-tasks 'invisible
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
  (setq-default
   org-indirect-buffer-display 'current-window
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
   '((?a . error)
     (?b . warning)
     (?c . success))
   org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3))
   org-startup-indented t
   org-todo-keywords
   '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)")
     (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
     (sequence "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "ABRT(c)"))
   org-todo-keyword-faces
   '(("[-]" :inherit (font-lock-constant-face bold))
     ("[?]" :inherit (warning bold))
     ("PROJ" :inherit (bold default))
     ("HOLD" :inherit (warning bold))
     ("ABRT" :inherit (error bold)))
   org-use-sub-superscripts '{}

   ;; Scale up LaTeX previews a bit (default is too small)
   org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; Show the full link destination in minibuffer when cursor/mouse is over it
  (advice-add #'org-eldoc-documentation-function :around #'+org-display-link-in-eldoc-a)

  ;; Don't do automatic indent detection in org files
  (add-to-list 'doom-detect-indentation-excluded-modes 'org-mode nil #'eq)

  ;; Previews are usually rendered with light backgrounds, so ensure their
  ;; background (and foreground) match the current theme.
  (defun +org-update-latex-preview-background-color-h ()
    (setq-default
     org-format-latex-options
     (plist-put org-format-latex-options
                :background
                (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                    'default)
                                :background nil t))))
  (add-hook 'doom-load-theme-hook #'+org-update-latex-preview-background-color-h)

  (set-pretty-symbols! 'org-mode
    :name "#+NAME:"
    :src_block "#+BEGIN_SRC"
    :src_block_end "#+END_SRC"))


(defun +org-init-babel-h ()
  (setq org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil  ; you don't need my permission
        ;; Show src buffer in popup, and don't monopolize the frame
        org-src-window-setup 'other-window)

  ;; I prefer C-c C-c over C-c ' (more consistent)
  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  ;; Use major-mode native TAB indentation in SRC blocks
  (advice-add #'org-return-indent :after #'+org-fix-newline-and-indent-in-src-blocks-a)

  ;; `org-babel-get-header' was removed from org in 9.0. Quite a few babel
  ;; plugins use it (like ob-spice), so until those plugins update, this
  ;; polyfill will do:
  (defun org-babel-get-header (params key &optional others)
    (cl-loop with fn = (if others #'not #'identity)
             for p in params
             if (funcall fn (eq (car p) key))
             collect p))

  ;; Fixes for various babel plugins
  (setq org-babel-js-function-wrapper "console.log(require('util').inspect(function(){\n%s\n}()));"))


(defun +org-init-babel-lazy-loader-h ()
  "Load babel libraries lazily when babel blocks are executed."
  (defvar +org-babel-mode-alist
    '((cpp . C)
      (C++ . C)
      (D . C)
      (sh . shell)
      (bash . shell)
      (matlab . octave))
    "An alist mapping languages to babel libraries. This is necessary for babel
libraries (ob-*.el) that don't match the name of the language.

For example, with (fish . shell) will cause #+BEGIN_SRC fish to load ob-shell.el
when executed.")

  (defvar +org-babel-load-functions ()
    "A list of functions executed to load the current executing src block. They
take one argument (the language specified in the src block, as a string). Stops
at the first function to return non-nil.")

  (defadvice! +org--src-lazy-load-library-a (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    :before #'org-src--get-lang-mode
    (or (cdr (assoc lang org-src-lang-modes))
        (fboundp (intern-soft (format "%s-mode" lang)))
        (require (intern-soft (format "ob-%s" lang)) nil t)))

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
                 (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
                     (require (intern (format "ob-%s" lang)) nil t)))
        (when (assq :async (nth 2 info))
          ;; ob-async has its own agenda for lazy loading packages (in the
          ;; child process), so we only need to make sure it's loaded.
          (require 'ob-async nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))
      t)))


(defun +org-init-capture-defaults-h ()
  "Sets up some reasonable defaults, as well as two `org-capture' workflows that
I like:

1. The traditional way: invoking `org-capture' directly, via SPC X, or through
   the :cap ex command.
2. Through a org-capture popup frame that is invoked from outside Emacs (the
   ~/.emacs.d/bin/org-capture script). This can be invoked from qutebrowser,
   vimperator, dmenu or a global keybinding."

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

  (setq org-default-notes-file
        (expand-file-name +org-capture-notes-file org-directory)
        org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t :kill-buffer t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("pn" "Project notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("pc" "Project changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-notes-file "Unreleased")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)))

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


(defun +org-init-centralized-attachments-h ()
  "I believe Org's native attachment system is over-complicated and litters
files with metadata I don't want. So I wrote my own, which:

+ Places attachments in a centralized location (`org-attach-directory' in
  `org-directory').
+ Adds attach:* link abbreviation for quick links to these files from anywhere.
+ Use `+org-attach/sync' to index all attachments in `org-directory' that use
  the attach:* abbreviation and delete orphaned ones that are no longer
  referenced.
+ This compliments the +dragndrop flag which provides drag'n'drop support for
  images (with preview) and media files.

Some commands of interest:
+ `org-download-screenshot'
+ `+org-attach/file'
+ `+org-attach/url'
+ `+org-attach/sync'"
  (setq org-attach-directory (doom-path org-directory org-attach-directory))

  ;; A shorter link to attachments
  (add-to-list 'org-link-abbrev-alist
               (cons "attach"
                     (abbreviate-file-name org-attach-directory)))

  (org-link-set-parameters
   "attach"
   :follow   (lambda (link) (find-file (doom-path org-attach-directory link)))
   :complete (lambda (&optional _arg)
               (+org--relpath (+org-link-read-file "attach" org-attach-directory)
                              org-attach-directory))
   :face     (lambda (link)
               (if (file-exists-p (expand-file-name link org-attach-directory))
                   'org-link
                 'error)))

  (after! projectile
    (add-to-list 'projectile-globally-ignored-directories org-attach-directory))

  (after! recentf
    (add-to-list 'recentf-exclude
                 (lambda (file) (file-in-directory-p file org-attach-directory)))))


(defun +org-init-centralized-exports-h ()
  "TODO"
  (defvar +org-enable-centralized-exports t
    "If non-nil, files exported from files in `org-directory' will be stored in
`+org-export-directory', rather than the same directory has the input file(s).")

  (defvar +org-export-directory ".export/"
    "Where to store exported files relative to `org-directory'. Can be an absolute
path too.")

  ;; I don't have any beef with org's built-in export system, but I do wish it
  ;; would export to a central directory (by default), rather than
  ;; `default-directory'. This is because all my org files are usually in one
  ;; place, and I want to be able to refer back to old exports if needed.
  (setq +org-export-directory (expand-file-name +org-export-directory org-directory))

  (defadvice! +org--export-output-file-name-a (args)
    "Return a centralized export location unless one is provided or the current
file isn't in `org-directory'."
    :filter-args #'org-export-output-file-name
    (when (and +org-enable-centralized-exports
               (not (nth 2 args))
               buffer-file-name
               (file-in-directory-p buffer-file-name org-directory))
      (cl-destructuring-bind (extension &optional subtreep _pubdir) args
        (let ((dir (expand-file-name +org-export-directory org-directory)))
          (unless (file-directory-p dir)
            (make-directory dir t))
          (setq args (list extension subtreep dir)))))
    args))


(defun +org-init-custom-links-h ()
  (defun +org--relpath (path root)
    (if (and buffer-file-name (file-in-directory-p buffer-file-name root))
        (file-relative-name path)
      path))

  (defun +org-def-link (key dir)
    (org-link-set-parameters
     key
     :complete (lambda () (+org--relpath (+org-link-read-file key dir) dir))
     :follow   (lambda (link) (find-file (expand-file-name link dir)))
     :face     (lambda (link)
                 (if (file-exists-p (expand-file-name link dir))
                     'org-link
                   'error))))

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
  (use-package! org-yt))


(defun +org-init-export-h ()
  (setq org-export-with-smart-quotes t)

  (when (featurep! :lang markdown)
    (add-to-list 'org-export-backends 'md))

  (use-package! ox-pandoc
    :when (and (featurep! +pandoc)
               (executable-find "pandoc"))
    :after ox
    :init
    (add-to-list 'org-export-backends 'pandoc)
    :config
    (setq org-pandoc-options
          '((standalone . t)
            (mathjax . t)
            (variable . "revealjs-url=https://cdn.jsdelivr.net/npm/reveal.js@3/")))))


(defun +org-init-habit-h ()
  "TODO"
  (defvar +org-habit-graph-padding 2
    "The padding added to the end of the consistency graph")

  (defvar +org-habit-min-width 30
    "Hides the consistency graph if the `org-habit-graph-column' is less than this value")

  (defvar +org-habit-graph-window-ratio 0.3
    "The ratio of the consistency graphs relative to the window width")

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
    (let ((result (funcall orig-fn path width prefix separator))
          (separator (or separator "/")))
      (string-join
       (cl-loop for part
                in (split-string (substring-no-properties result) separator)
                for n from 0
                for face = (nth (% n org-n-level-faces) org-level-faces)
                collect
                (org-add-props (replace-regexp-in-string org-any-link-re "\\4" part)
                    nil 'face `(:foreground ,(face-foreground face nil t) :weight bold)))
       separator)))

  (add-hook! 'org-agenda-finalize-hook
    (defun +org-exclude-agenda-buffers-from-workspace-h ()
      "Prevent temporarily-opened agenda buffers from being associated with the
current workspace."
      (when (and org-agenda-new-buffers (bound-and-true-p persp-mode))
        (let (persp-autokill-buffer-on-remove)
          (persp-remove-buffer org-agenda-new-buffers
                               (get-current-persp)
                               nil)))))

  (defadvice! +org--exclude-agenda-buffers-from-recentf-a (orig-fn file)
    "Prevent temporarily opened agenda buffers from polluting recentf."
    :around #'org-get-agenda-file-buffer
    (let ((recentf-exclude (list (lambda (_file) t))))
      (funcall orig-fn file))))


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
             #'+org-indent-maybe-h
             #'+org-yas-expand-maybe-h)

  (add-hook 'doom-delete-backward-functions
            #'+org-delete-backward-char-and-realign-table-maybe-h)

  (map! :map org-mode-map
        ;; textmate-esque newline insertion
        [C-return]   #'+org/insert-item-below
        [C-S-return] #'+org/insert-item-above
        "C-c C-S-l" #'+org/remove-link
        "C-c C-i"   #'org-toggle-inline-images
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
        "a" #'org-attach
        "d" #'org-deadline
        "f" #'org-footnote-new
        "h" #'org-toggle-heading
        "i" #'org-toggle-item
        "I" #'org-toggle-inline-images
        "l" #'org-insert-link
        "L" #'org-store-link
        "p" #'org-set-property
        "q" #'org-set-tags-command
        "r" #'org-refile
        "s" #'org-schedule
        "t" #'org-todo
        "T" #'org-todo-list
        (:prefix ("c" . "clock")
          "c" #'org-clock-in
          "C" #'org-clock-out
          "d" #'org-clock-mark-default-task
          "e" #'org-clock-modify-effort-estimate
          "l" #'org-clock-in-last
          "g" #'org-clock-goto
          "G" (λ! (org-clock-goto 'select))
          "x" #'org-clock-cancel
          "=" #'org-clock-timestamps-up
          "-" #'org-clock-timestamps-down)
        (:prefix ("e" . "export")
          :desc "to markdown"         "m" #'org-md-export-to-markdown
          :desc "to markdown & open"  "M" #'org-md-export-as-markdown
          :desc "to reveal.js"        "r" #'org-reveal-export-to-html
          :desc "to reveal.js & open" "R" #'org-reveal-export-to-html-and-browse
          (:prefix ("b" . "from beamer")
            :desc "to latex"            "l" #'org-beamer-export-to-latex
            :desc "to latex & open"     "L" #'org-beamer-export-as-latex
            :desc "as pdf"              "p" #'org-beamer-export-to-pdf))
        (:prefix ("g" . "goto")
          "g" #'org-goto
          (:when (featurep! :completion ivy)
            "g" #'counsel-org-goto
            "G" #'counsel-org-goto-all)
          "a" #'org-agenda-goto
          "A" #'org-agenda-clock-goto
          "c" #'org-clock-goto
          "C" (λ! (org-clock-goto 'select))
          "i" #'org-id-goto
          "r" #'org-refile-goto-last-stored
          "x" #'org-capture-goto-last-stored)
        (:prefix ("b" . "tables")
          "a" #'org-table-align
          "e" #'org-table-edit-field
          "h" #'org-table-field-info
          (:when (featurep! +gnuplot)
            "p" #'org-plot/gnuplot)
          (:prefix ("i" . "insert")
            "-" #'org-table-insert-hline
            "h" #'+org/table-insert-column-left
            "j" #'+org/table-insert-row-below
            "k" #'org-table-insert-row
            "l" #'+org/table-insert-column-right)
          (:prefix ("m" . "move")
            "h" #'org-table-move-column-left
            "j" #'org-table-move-row-down
            "k" #'org-table-move-row-up
            "l" #'org-table-move-column-right)
          (:prefix ("f" . "formula")
            "c" #'org-table-create
            "r" #'org-table-recalculate
            "e" #'org-table-edit-formulas
            "=" #'org-table-eval-formulas)))

  ;; HACK Fixes #1483: this messy hack fixes `org-agenda' or `evil-org-agenda'
  ;; overriding SPC, breaking the localleader
  (define-minor-mode org-agenda-localleader-mode "TODO"
    :keymap (make-sparse-keymap))
  (add-hook 'org-agenda-mode-hook #'org-agenda-localleader-mode)

  (map! :map org-agenda-localleader-mode-map
        :localleader
        "d" #'org-agenda-deadline
        "q" #'org-agenda-set-tags
        "r" #'org-agenda-refile
        "s" #'org-agenda-schedule
        "t" #'org-agenda-todo))


(defun +org-init-keybinds-for-evil-h (&rest args)
  "TODO"
  (when (featurep! :editor evil +everywhere)
    (use-package! evil-org
      :hook (org-mode . evil-org-mode)
      :init
      (defvar evil-org-key-theme '(navigation insert textobjects))
      (defvar evil-org-retain-visual-state-on-shift t)
      (defvar evil-org-special-o/O '(table-row))
      (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
      :config
      ;; change `evil-org-key-theme' instead
      (advice-add #'evil-org-set-key-theme :override #'ignore))

    (use-package! evil-org-agenda
      :after org-agenda
      :config (evil-org-agenda-set-keys))

    ;; Only fold the current tree, rather than recursively
    (add-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h t)

    ;; Fix o/O creating new list items in the middle of nested plain lists. Only
    ;; has an effect when `evil-org-special-o/O' has `item' in it (not the
    ;; default).
    (advice-add #'evil-org-open-below :around #'+org-evil-org-open-below-a)

    (map! :map outline-mode-map
          ;; Undo keybinds from `evil-collection-outline'
          :n "^" nil
          :n [backtab] nil
          :n "M-j" nil
          :n "M-k" nil
          :n "C-j" nil
          :n "C-k" nil
          :n "]" nil
          :n "[" nil

          :map evil-org-mode-map
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
          :ni "C-S-l" (general-predicate-dispatch 'org-shiftmetaright
                        (org-at-table-p) 'org-table-insert-column)
          :ni "C-S-h" (general-predicate-dispatch 'org-shiftmetaleft
                        (org-at-table-p) '+org/table-insert-column-left)
          :ni "C-S-k" (general-predicate-dispatch 'org-shiftmetaup
                        (org-at-table-p) 'org-table-insert-row)
          :ni "C-S-j" (general-predicate-dispatch 'org-shiftmetadown
                        (org-at-table-p) '+org/table-insert-row-below)
          ;; moving/(de|pro)moting single headings & shifting table rows/columns
          :ni "C-M-S-l" (general-predicate-dispatch 'org-metaright
                          (org-at-table-p) 'org-table-move-column-right)
          :ni "C-M-S-h" (general-predicate-dispatch 'org-metaleft
                          (org-at-table-p) 'org-table-move-column-left)
          :ni "C-M-S-k" (general-predicate-dispatch 'org-metaup
                          (org-at-table-p) 'org-table-move-row-up)
          :ni "C-M-S-j" (general-predicate-dispatch 'org-metadown
                          (org-at-table-p) 'org-table-move-row-down)
          ;; more intuitive RET keybinds
          :i [return] #'org-return-indent
          :i "RET"    #'org-return-indent
          :n [return] #'+org/dwim-at-point
          :n "RET"    #'+org/dwim-at-point
          ;; more vim-esque org motion keys (not covered by evil-org-mode)
          :m "]]"  (λ! (org-forward-heading-same-level nil) (org-beginning-of-line))
          :m "[["  (λ! (org-backward-heading-same-level nil) (org-beginning-of-line))
          :m "]h"  #'org-next-visible-heading
          :m "[h"  #'org-previous-visible-heading
          :m "]l"  #'org-next-link
          :m "[l"  #'org-previous-link
          :m "]c"  #'org-babel-next-src-block
          :m "[c"  #'org-babel-previous-src-block
          :m "^"   #'evil-org-beginning-of-line
          :m "0"   (λ! (let (visual-line-mode) (org-beginning-of-line)))
          :n "gQ"  #'org-fill-paragraph
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
          "C-S-j" (λ! (org-eval-in-calendar '(calendar-forward-year 1))))))


(defun +org-init-popup-rules-h ()
  (set-popup-rules!
    '(("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
      ("^\\*\\(?:Agenda Com\\|Calendar\\|Org \\(?:Export Dispatcher\\|Select\\)\\)"
       :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
      ("^\\*Org Agenda"    :ignore t)
      ("^\\*Org Src"       :size 0.4 :quit nil :select t :autosave t :modeline t :ttl nil)
      ("^CAPTURE.*\\.org$" :size 0.2 :quit nil :select t :autosave t))))


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
;;; Bootstrap

(use-package! org
  :defer-incrementally
  calendar find-func format-spec org-macs org-compat org-faces org-entities
  org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
  org-capture
  :preface
  ;; Change org defaults (should be set before org loads)
  (defvar org-directory "~/org/")
  (defvar org-attach-directory ".attach/")

  (setq org-clock-persist-file (concat doom-etc-dir "org-clock-save.el")
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

  (add-hook! 'org-mode-hook
             #'org-bullets-mode  ; "prettier" bullets
             #'toc-org-enable    ; auto-table of contents
             #'auto-fill-mode    ; hard line wrapping
             ;; `show-paren-mode' causes flickering with indentation margins made by
             ;; `org-indent-mode', so we turn off show-paren-mode altogether
             #'doom-disable-show-paren-mode-h
             ;; Shows a lot of false positives, so...
             #'doom-disable-show-trailing-whitespace-h

             #'+org-enable-auto-reformat-tables-h
             #'+org-enable-auto-update-cookies-h
             #'+org-unfold-to-2nd-level-or-point-h)

  (add-hook! 'org-load-hook
             #'+org-init-appearance-h
             #'+org-init-agenda-h
             #'+org-init-babel-h
             #'+org-init-babel-lazy-loader-h
             #'+org-init-capture-defaults-h
             #'+org-init-capture-frame-h
             #'+org-init-centralized-attachments-h
             #'+org-init-centralized-exports-h
             #'+org-init-custom-links-h
             #'+org-init-export-h
             #'+org-init-habit-h
             #'+org-init-hacks-h
             #'+org-init-keybinds-h
             #'+org-init-keybinds-for-evil-h ; will noop without :editor evil
             #'+org-init-popup-rules-h
             #'+org-init-protocol-h
             #'+org-init-protocol-lazy-loader-h
             #'+org-init-smartparens-h)

  ;; In case the user has eagerly loaded org from their configs
  (when (and (featurep 'org)
             (not doom-reloading-p))
    (message "`org' was already loaded by the time lang/org loaded, this may cause issues")
    (run-hooks 'org-load-hook))

  ;;; Custom org modules
  (if (featurep! +dragndrop) (load! "contrib/dragndrop"))
  (if (featurep! +ipython)   (load! "contrib/ipython"))
  (if (featurep! +present)   (load! "contrib/present"))

  :config
  (add-hook 'org-open-at-point-functions #'doom-set-jump-h)

  ;;; Packages
  (after! toc-org
    (setq toc-org-hrefify-default "gh")
    (defadvice! +org--unfold-toc-a (&rest _)
      :before #'toc-org-insert-toc
      (save-excursion
        (when (re-search-forward toc-org-toc-org-regexp (point-max) t)
          (+org/open-fold)))))

  (use-package! org-pdfview
    :when (featurep! :tools pdf)
    :commands (org-pdfview-open)
    :init
    (delete '("\\.pdf\\'" . default) org-file-apps)
    ;; org links to pdf files are opened in pdf-view-mode
    (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (_file link) (org-pdfview-open link))))
    ;; support for links to specific pages
    (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (_file link) (org-pdfview-open link)))))

  (use-package! org-crypt ; built-in
    :commands org-encrypt-entries
    :hook (org-reveal-start . org-decrypt-entry)
    :init
    (add-hook! 'org-mode-hook
      (add-hook 'before-save-hook 'org-encrypt-entries nil t))
    (add-to-list 'org-tags-exclude-from-inheritance "crypt")
    (setq org-crypt-key user-mail-address))

  (use-package! org-clock ; built-in
    :commands org-clock-save
    :init
    (setq org-clock-persist t)
    (defadvice! +org--clock-load-a (&rest _)
      "Lazy load org-clock until its commands are used."
      :before '(org-clock-in
                org-clock-out
                org-clock-in-last
                org-clock-goto
                org-clock-cancel)
      (org-clock-load))
    :config
    (add-hook 'kill-emacs-hook #'org-clock-save)))
