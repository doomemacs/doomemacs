;;; lang/org/config.el

;; A few things you can expect
;;   + `org-capture' in a popup frame (can be invoked from outside emacs too)
;;   + Exported files are put in a centralized location (see
;;     `org-export-directory')
;;   + Inline latex previews (requires latex and dvipng programs)
;;   + Inline code block execution for various languages
;;   + TODO A simpler attachment system (with auto-deleting support) and
;;     drag-and-drop for images and documents into org files
;;   + TODO Custom links for class notes
;;   + TODO An org-mode based CRM (including invoicing and pdf exporting) (see custom-crm)
;;   + TODO A tag-based file browser reminiscient of Evernote and Quiver (there's neotree too!)

(defvar +org-init-hook nil
  "TODO")

(add-hook 'org-load-hook #'+org|init)
(add-hook 'org-mode-hook #'+org|hook)

;; Custom variables
(defvar +org-dir (expand-file-name "~/work/org/")
  "The directory where org files are kept.")
(defvaralias 'org-directory '+org-dir)

(defvar +org-attachment-dir ".attach/"
  "Where to store attachments (relative to current org file).")

;; Ensure ELPA org is prioritized above built-in org.
(when-let (path (locate-library "org" nil doom--package-load-path))
  (push (file-name-directory path) load-path))

(load! +agenda)
(load! +attach)
(load! +capture)
(load! +export)
(load! +notebook)
(load! +babel)


;;
;; Config
;;

(defun +org|hook ()
  "Run everytime `org-mode' is enabled."
  (setq line-spacing 1)

  (visual-line-mode +1)
  (when (and (featurep 'evil) evil-mode)
    (evil-org-mode +1))

  (unless org-agenda-inhibit-startup
    ;; My version of the 'overview' #+STARTUP option: expand first-level
    ;; headings.
    (when (eq org-startup-folded t)
      (outline-hide-sublevels 2))

    ;; If saveplace places the point in a folded position, unfold it on load
    (when (outline-invisible-p)
      (ignore-errors
        (save-excursion
          (outline-previous-visible-heading 1)
          (org-show-subtree)))))

  (defun +org|realign-table-maybe ()
    "Auto-align table under cursor."
    (when (org-at-table-p)
      (org-table-align)))
  (add-hook 'evil-insert-state-exit-hook #'+org|realign-table-maybe nil t)

  (defun +org|update-cookies ()
    "Update counts in headlines (aka \"cookies\")."
    (when (and buffer-file-name (file-exists-p buffer-file-name))
      (org-update-statistics-cookies t)))

  (add-hook 'before-save-hook #'+org|update-cookies nil t)
  (add-hook 'evil-insert-state-exit-hook #'+org|update-cookies nil t))


(defun +org|init ()
  "Initializes org core."
  (define-minor-mode evil-org-mode
    "Evil-mode bindings for org-mode."
    :init-value nil
    :lighter " !"
    :keymap (make-sparse-keymap)
    :group 'evil-org)

  (define-minor-mode +org-pretty-mode
    "TODO"
    :init-value nil
    :lighter " *"
    :group 'evil-org
    (setq org-hide-emphasis-markers +org-pretty-mode)
    (org-toggle-pretty-entities)
    ;; In case the above un-align tables
    (org-table-map-tables 'org-table-align t))

  (setq-default
   org-export-coding-system 'utf-8
   org-todo-keywords '((sequence "[ ](t)" "[-](p)" "[?](m)" "|" "[X](d)")
                       (sequence "TODO(T)" "|" "DONE(D)")
                       (sequence "IDEA(i)" "NEXT(n)" "ACTIVE(a)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))

   ;; Appearance
   outline-blank-line t
   org-indent-mode-turns-on-hiding-stars t
   org-adapt-indentation nil
   org-cycle-separator-lines 1
   org-cycle-include-plain-lists t
   org-ellipsis " ... "
   org-entities-user '(("flat"  "\\flat" nil "" "" "266D" "♭")
                       ("sharp" "\\sharp" nil "" "" "266F" "♯"))
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-footnote-auto-label 'plain
   org-hide-emphasis-markers nil
   org-hide-leading-stars t
   org-hide-leading-stars-before-indent-mode t
   org-hidden-keywords nil
   org-image-actual-width nil
   org-indent-indentation-per-level 2
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts t
   org-startup-folded t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column 0
   org-use-sub-superscripts '{}

   ;; Behavior
   org-blank-before-new-entry '((heading . nil) (plain-list-item . auto))
   org-catch-invisible-edits 'show
   org-checkbox-hierarchical-statistics nil
   org-enforce-todo-checkbox-dependencies nil
   org-confirm-elisp-link-function nil
   org-default-priority ?C
   org-hierarchical-todo-statistics t
   org-loop-over-headlines-in-active-region t
   org-refile-use-outline-path t
   org-special-ctrl-a/e t

   ;; Sorting/refiling
   org-archive-location (concat +org-dir "/archived/%s::")
   org-refile-targets '((nil . (:maxlevel . 2))) ; display full path in refile completion

   ;; Latex
   org-highlight-latex-and-related '(latex)
   org-latex-create-formula-image-program 'dvipng
   org-latex-image-default-width ".9\\linewidth"
   org-latex-preview-ltxpng-directory (concat doom-cache-dir "/ltxpng/")
   org-latex-remove-logfiles nil
   org-startup-with-latex-preview nil
   ;; org-latex-packages-alist
   ;; '(("" "gauss" t)
   ;;   ("" "physics" t) TODO Install this)
   )

  ;; LaTeX previews are too small and usually render to light backgrounds, so
  ;; this enlargens them and ensures their background (and foreground) match the
  ;; current theme.
  (setq-default
   org-format-latex-options
   (plist-put org-format-latex-options :scale 1.5)
   org-format-latex-options
   (plist-put org-format-latex-options
              :background (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                              'default)
                                          :background nil t)))

  ;; Use ivy/helm if either is available
  (when (or (featurep! :completion ivy)
            (featurep! :completion helm))
    (setq-default org-completion-use-ido nil
                  org-outline-path-complete-in-steps nil))

  (let ((ext-regexp (regexp-opt '("GIF" "JPG" "JPEG" "SVG" "TIF" "TIFF" "BMP" "XPM"
                                  "gif" "jpg" "jpeg" "svg" "tif" "tiff" "bmp" "xpm"))))
    (setq iimage-mode-image-regex-alist
          `((,(concat "\\(`?file://\\|\\[\\[\\|<\\|`\\)?\\([-+./_0-9a-zA-Z]+\\."
                      ext-regexp "\\)\\(\\]\\]\\|>\\|'\\)?") . 2)
            (,(concat "<\\(http://.+\\." ext-regexp "\\)>") . 1))))

  ;;; Custom fontification
  ;; I like how org-mode fontifies checked TODOs and want this to extend to
  ;; checked checkbox items, so we remove the old checkbox highlight rule...
  (font-lock-remove-keywords
   'org-mode '(("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\(\\[[- X]\\]\\)"
                1 'org-checkbox prepend)))
  (font-lock-add-keywords
   'org-mode '(;; ...and replace it with my own
               ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[X\\][^\n]*\n\\)"
                1 'org-headline-done t)
               ("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\(\\[[- ]\\]\\)"
                1 'org-checkbox append)
               ;; Also highlight list bullets
               ("^ *\\([-+]\\|[0-9]+[).]\\) " 1 'org-list-dt append)
               ;; and separators
               ("^ *\\(-----+\\)$" 1 'org-meta-line)))

  ;; Enable gpg support
  (require 'epa-file)
  (epa-file-enable)
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key user-mail-address
        epa-file-encrypt-to user-mail-address)

  ;; smartparens config
  (sp-with-modes '(org-mode)
    (sp-local-pair "\\[" "\\]" :post-handlers '(("| " "SPC")))
    (sp-local-pair "\\(" "\\)" :post-handlers '(("| " "SPC")))
    (sp-local-pair "$$" "$$"   :post-handlers '((:add " | ")) :unless '(sp-point-at-bol-p))
    (sp-local-pair "{" nil))

  ;; The standard unicode characters are usually misaligned depending on the
  ;; font. This bugs me. Personally, the markdown #-marks for headlines are more
  ;; elegant, so use those.
  (def-package! org-bullets
    :commands org-bullets-mode
    :init (add-hook 'org-mode-hook 'org-bullets-mode)
    :config (setq org-bullets-bullet-list '("#")))

  ;; Keybinds
  (map! (:map org-mode-map
          "RET" nil
          "C-j" nil
          "C-k" nil
          :i [remap doom/inflate-space-maybe] #'org-self-insert-command
          :i "RET" #'org-return-indent)

        (:map evil-org-mode-map
          :n  "RET" #'+org/dwim-at-point
          ;;
          :ni "A-L" #'org-shiftmetaright
          :ni "A-H" #'org-shiftmetaleft
          :ni "A-K" #'org-shiftmetaup
          :ni "A-J" #'org-shiftmetadown
          ;; Expand tables (or shiftmeta move)
          :ni "C-S-l" #'+org/table-append-field-or-shift-right
          :ni "C-S-h" #'+org/table-prepend-field-or-shift-left
          :ni "C-S-k" #'+org/table-prepend-row-or-shift-up
          :ni "C-S-j" #'+org/table-append-row-or-shift-down
          ;; Navigate table cells
          :i  "C-L" #'+org/table-next-field
          :i  "C-H" #'+org/table-previous-field
          :i  "C-K" #'+org/table-previous-row
          :i  "C-J" #'+org/table-next-row

          :i  "C-e" #'org-end-of-line
          :i  "C-a" #'org-beginning-of-line

          :i  "<tab>"         #'+org/indent-or-next-field-or-yas-expand
          :i  [S-iso-lefttab] #'+org/dedent-or-prev-field ; for GNU Emacs
          :i  [(shift tab)]   #'+org/dedent-or-prev-field
          :i  [backtab]       #'+org/dedent-or-prev-field

          :n  "<tab>" #'+org/toggle-fold

          :nv "j"   #'evil-next-visual-line
          :nv "k"   #'evil-previous-visual-line
          :v  "<S-tab>" #'+snippets/expand-on-region

          :i  "M-a" (λ! (evil-visual-state) (org-mark-element))
          :n  "M-a" #'org-mark-element
          :v  "M-a" #'mark-whole-buffer

          :ni "<M-return>"   (λ! (+org/insert-item 'below))
          :ni "<S-M-return>" (λ! (+org/insert-item 'above))

          ;; Formatting shortcuts
          :i  "M-b" (λ! (+org-surround "*")) ; bold
          :i  "M-u" (λ! (+org-surround "_")) ; underline
          :i  "M-i" (λ! (+org-surround "/")) ; italics
          :i  "M-`" (λ! (+org-surround "+")) ; strikethrough

          :v  "M-b" "S*"
          :v  "M-u" "S_"
          :v  "M-i" "S/"
          :v  "M-`" "S+"

          (:localleader
           :n  "RET" #'org-archive-subtree
           :n  "SPC" #'+org/toggle-checkbox
           :n  "/"  #'org-sparse-tree
           :n  "="  #'org-align-all-tags
           :n  "?"  #'org-tags-view
           :n  "a"  #'org-agenda
           :n  "d"  #'org-time-stamp
           :n  "D"  #'org-deadline
           :n  "e"  #'org-edit-special
           :n  "E"  #'+org/edit-special-same-window
           :n  "n"  (λ! (if (buffer-narrowed-p) (widen) (org-narrow-to-subtree)))
           :n  "r"  #'org-refile
           :n  "R"  (λ! (org-metaleft) (org-archive-to-archive-sibling)) ; archive to parent sibling
           :n  "s"  #'org-schedule
           :n  "t"  (λ! (org-todo (if (org-entry-is-todo-p) 'none 'todo)))
           :v  "t"  (λ! (evil-ex-normal evil-visual-beginning evil-visual-end "\\t"))
           :n  "T"  #'org-todo
           :n  "v"  #'variable-pitch-mode
           :nv "l"  #'org-insert-link
           :nv "L"  #'org-store-link
           ;; :n  "w"  'writing-mode
           ;; :n  "x"  '+org/remove-link
           )

          ;; TODO Improve folding bindings
          :n  "za"  #'+org/toggle-fold
          :n  "zA"  #'org-shifttab
          :n  "zc"  #'outline-hide-subtree
          :n  "zC"  (λ! (outline-hide-sublevels 1))
          :n  "zd"  (lambda (&optional arg) (interactive "p") (outline-hide-sublevels (or arg 3)))
          :n  "zm"  (λ! (outline-hide-sublevels 1))
          :n  "zo"  #'outline-show-subtree
          :n  "zO"  #'outline-show-all
          :n  "zr"  #'outline-show-all

          :m  "]]"  (λ! (call-interactively #'org-forward-heading-same-level) (org-beginning-of-line))
          :m  "[["  (λ! (call-interactively #'org-backward-heading-same-level) (org-beginning-of-line))
          :m  "]l"  #'org-next-link
          :m  "[l"  #'org-previous-link

          :m  "gh"  #'outline-up-heading
          :m  "gj"  #'org-forward-heading-same-level
          :m  "gk"  #'org-backward-heading-same-level
          :m  "gl"  (λ! (call-interactively #'outline-next-visible-heading) (show-children))

          :n  "go"  #'org-open-at-point
          :n  "gO"  (λ! (let ((org-link-frame-setup (append '((file . find-file-other-window)) org-link-frame-setup))
                              (org-file-apps '(("\\.org$" . emacs)
                                               (t . "open \"%s\""))))
                          (call-interactively #'org-open-at-point)))

          :n  "gQ"  #'org-fill-paragraph
          :m  "$"   #'org-end-of-line
          :m  "^"   #'org-beginning-of-line
          :n  "<"   #'org-metaleft
          :n  ">"   #'org-metaright
          :v  "<"   (λ! (org-metaleft)  (evil-visual-restore))
          :v  ">"   (λ! (org-metaright) (evil-visual-restore))
          :n  "-"   #'org-cycle-list-bullet
          :m  "<tab>" #'org-cycle))

  ;; Initialize everything else
  (run-hooks '+org-init-hook)
  (+org|hacks))


(defun +org|hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (push '(file . find-file) org-link-frame-setup)

  ;; Let OS decide what to do with files when opened
  (setq org-file-apps
        `(("\\.org$" . emacs)
          (t . ,(cond (IS-MAC "open -R \"%s\"")
                      (IS-LINUX "xdg-open \"%s\"")))))

  ;; Remove highlights on ESC
  (defun +org*remove-occur-highlights (&rest args)
    (when (eq major-mode 'org-mode)
      (org-remove-occur-highlights)))
  (advice-add #'evil-force-normal-state :before #'+org*remove-occur-highlights)

  ;; Don't reset org-hide!
  (advice-add #'org-find-invisible-foreground :override #'ignore))

