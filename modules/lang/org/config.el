;;; lang/org/config.el -*- lexical-binding: t; -*-

;; Changed org defaults (should be set before org loads)
(defvar org-directory "~/org/")
(defvar org-modules
  '(org-w3m
    ;; org-bbdb
    org-bibtex
    org-docview
    ;; org-gnus
    org-info
    ;; org-irc
    ;; org-mhe
    ;; org-rmail
    ))


;;
;;; Packages

;; `toc-org'
(setq toc-org-hrefify-default "gh")
(defun +org*unfold-toc (&rest _)
  (save-excursion
    (when (re-search-forward toc-org-toc-org-regexp (point-max) t)
      (+org/open-fold))))
(advice-add #'toc-org-insert-toc :before #'+org*unfold-toc)

(def-package! evil-org
  :when (featurep! :editor evil +everywhere)
  :hook (org-mode . evil-org-mode)
  :init
  (defvar evil-org-key-theme '(navigation insert textobjects))
  (defvar evil-org-special-o/O '(table-row))
  (add-hook 'org-load-hook #'+org|setup-evil-keybinds)
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  :config
  (setq evil-org-retain-visual-state-on-shift t)
  ;; change `evil-org-key-theme' instead
  (advice-add #'evil-org-set-key-theme :override #'ignore)
  (def-package! evil-org-agenda
    :after org-agenda
    :config (evil-org-agenda-set-keys)))

(def-package! org-pdfview
  :when (featurep! :tools pdf)
  :commands (org-pdfview-open)
  :init
  (after! org
    (delete '("\\.pdf\\'" . default) org-file-apps)
    ;; org links to pdf files are opened in pdf-view-mode
    (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (_file link) (org-pdfview-open link))))
    ;; support for links to specific pages
    (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (_file link) (org-pdfview-open link))))))

(def-package! org-crypt ; built-in
  :commands org-encrypt-entries
  :hook (org-reveal-start . org-decrypt-entry)
  :init
  (add-hook! 'org-mode-hook
    (add-hook 'before-save-hook 'org-encrypt-entries nil t))
  :config
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key user-mail-address))

(def-package! org-clock ; built-in
  :commands org-clock-save
  :hook (org-mode . org-clock-load)
  :init
  (setq org-clock-persist 'history
        org-clock-persist-file (concat doom-etc-dir "org-clock-save.el"))
  :config
  (add-hook 'kill-emacs-hook #'org-clock-save))


;;
;;; `org-load' hooks

(defun +org|setup-agenda ()
  (unless org-agenda-files
    (setq org-agenda-files (list org-directory)))
  (setq-default
   org-agenda-dim-blocked-tasks nil
   org-agenda-inhibit-startup t
   org-agenda-skip-unavailable-files t
   ;; Move the agenda to show the previous 3 days and the next 7 days for a bit
   ;; better context instead of just the current week which is a bit confusing
   ;; on, for example, a sunday
   org-agenda-span 10
   org-agenda-start-on-weekday nil
   org-agenda-start-day "-3d"))


(defun +org|setup-custom-links ()
  "Set up custom org links."
  (setq org-link-abbrev-alist
        '(("github"      . "https://github.com/%s")
          ("youtube"     . "https://youtube.com/watch?v=%s")
          ("google"      . "https://google.com/search?q=")
          ("gimages"     . "https://google.com/images?q=%s")
          ("gmap"        . "https://maps.google.com/maps?q=%s")
          ("duckduckgo"  . "https://duckduckgo.com/?q=%s")
          ("wolfram"     . "https://wolframalpha.com/input/?i=%s")
          ("doom-repo"   . "https://github.com/hlissner/doom-emacs/%s")))

  (defun +org--relpath (path root)
    (if (and buffer-file-name (file-in-directory-p buffer-file-name root))
        (file-relative-name path)
      path))

  ;; highlight broken links
  (org-link-set-parameters
   "file"
   :face (lambda (path)
           (if (or (file-remote-p path)
                   (file-exists-p path))
               'org-link
             'error)))

  (defun +org-def-link (key dir)
    (org-link-set-parameters
     key
     :complete (lambda () (+org--relpath (+org-link-read-file key dir) dir))
     :follow   (lambda (link) (find-file (expand-file-name link dir)))
     :face     (lambda (link)
                 (if (file-exists-p (expand-file-name link dir))
                     'org-link
                   'error))))

  (+org-def-link "org" org-directory)
  (+org-def-link "doom" doom-emacs-dir)
  (+org-def-link "doom-docs" doom-docs-dir)
  (+org-def-link "doom-modules" doom-modules-dir)

  (def-package! org-yt
    :config
    (org-link-set-parameters "http"  :image-data-fun #'+org-image-link)
    (org-link-set-parameters "https" :image-data-fun #'+org-image-link)
    (org-link-set-parameters "img"   :image-data-fun #'+org-inline-data-image)))


(defun +org|setup-ui ()
  "Configures the UI for `org-mode'."
  (setq-default
   org-adapt-indentation nil
   org-cycle-include-plain-lists t
   org-eldoc-breadcrumb-separator " → "
   org-entities-user
   '(("flat"  "\\flat" nil "" "" "266D" "♭")
     ("sharp" "\\sharp" nil "" "" "266F" "♯"))
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-footnote-auto-label 'plain
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars t
   org-hide-leading-stars-before-indent-mode t
   org-image-actual-width nil
   org-indent-indentation-per-level 2
   org-indent-mode-turns-on-hiding-stars t
   org-list-description-max-indent 4
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts t
   org-priority-faces
   '((?a . error)
     (?b . warning)
     (?c . success))
   org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3))
   org-startup-folded t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column 0
   org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d)")
     (sequence "[ ](T)" "[-](p)" "[?](m)" "|" "[X](D)")
     (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
   org-todo-keyword-faces
   '(("[-]" :inherit (font-lock-constant-face bold))
     ("[?]" :inherit (warning bold))
     ("WAITING" :inherit bold)
     ("LATER" :inherit (warning bold)))
   org-use-sub-superscripts '{}

   ;; Scale up LaTeX previews a bit (default is too small)
   org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")
   org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  (advice-add #'org-eldoc-documentation-function :around #'+org*display-link-in-eldoc)

  ;; Don't do automatic indent detection in org files
  (add-to-list 'doom-detect-indentation-excluded-modes 'org-mode nil #'eq)

  ;; Previews are usually rendered with light backgrounds, so ensure their
  ;; background (and foreground) match the current theme.
  (defun +org|update-latex-preview-background-color ()
    (setq-default
     org-format-latex-options
     (plist-put org-format-latex-options
                :background
                (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                    'default)
                                :background nil t))))
  (add-hook 'doom-load-theme-hook #'+org|update-latex-preview-background-color))


(defun +org|setup-keybinds ()
  "Sets up org-mode and evil keybindings. Tries to fix the idiosyncrasies
between the two."
  (add-hook 'doom-escape-hook #'+org|remove-occur-highlights)

  ;; C-a & C-e act like `doom/backward-to-bol-or-indent' and
  ;; `doom/forward-to-last-non-comment-or-eol', but with more org awareness.
  (setq org-special-ctrl-a/e t)

  (setq org-M-RET-may-split-line nil
        ;; insert new headings after current subtree rather than inside it
        org-insert-heading-respect-content t)

  (add-hook! 'org-tab-first-hook #'(+org|indent-maybe +org|yas-expand-maybe))
  (add-hook 'doom-delete-backward-functions #'+org|delete-backward-char-and-realign-table-maybe)

  (map! :map org-mode-map
        ;; textmate-esque newline insertion
        [C-return]   (λ! (+org/insert-item 'below))
        [C-S-return] (λ! (+org/insert-item 'above))
        (:when IS-MAC
          [s-return]   (λ! (+org/insert-item 'below))
          [s-S-return] (λ! (+org/insert-item 'above)))
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
        "d" #'org-deadline
        "f" #'org-footnote-new
        "h" #'org-toggle-heading
        "i" #'org-toggle-item
        "I" #'org-toggle-inline-images
        "l" #'org-insert-link
        "L" #'org-store-link
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

  (map! :map org-agenda-mode-map
        :localleader
        "d" #'org-agenda-deadline
        "q" #'org-agenda-set-tags
        "r" #'org-agenda-refile
        "s" #'org-agenda-schedule
        "t" #'org-agenda-todo))

(defun +org|setup-evil-keybinds (&rest args)
  (unless args ; lookout for recursive requires
    (require 'evil-org))

  ;; Only fold the current tree, rather than recursively
  (add-hook 'org-tab-first-hook #'+org|cycle-only-current-subtree t)

  ;; Fix o/O creating new list items in the middle of nested plain lists. Only
  ;; has an effect when `evil-org-special-o/O' has `item' in it (not the
  ;; default).
  (advice-add #'evil-org-open-below :around #'+org*evil-org-open-below)

  ;; Undo keybinds in `evil-collection-outline'
  (map! :map outline-mode-map
        :n "^" nil
        :n [backtab] nil
        :n "M-j" nil
        :n "M-k" nil
        :n "C-j" nil
        :n "C-k" nil
        :n "]" nil
        :n "[" nil

        :map evil-org-mode-map
        :ni [C-return]   (λ! (+org/insert-item 'below))
        :ni [C-S-return] (λ! (+org/insert-item 'above))
        (:when IS-MAC
          :ni [s-return]   (λ! (+org/insert-item 'below))
          :ni [s-S-return] (λ! (+org/insert-item 'above)))
        ;; navigate table cells (from insert-mode)
        :i "C-l" (general-predicate-dispatch 'org-end-of-line
                   (org-at-table-p) 'org-table-next-field)
        :i "C-h" (general-predicate-dispatch 'org-beginning-of-line
                   (org-at-table-p) 'org-table-previous-field)
        :i "C-k" (general-predicate-dispatch 'org-up-element
                   (org-at-table-p) '+org/table-previous-row)
        :i "C-j" (general-predicate-dispatch 'org-down-element
                   (org-at-table-p) 'org-table-next-row)
        ;; expanding tables (prepend/append columns/rows)
        :ni "C-S-l" (general-predicate-dispatch 'org-shiftmetaright
                      (org-at-table-p) 'org-table-insert-column)
        :ni "C-S-h" (general-predicate-dispatch 'org-shiftmetaleft
                      (org-at-table-p) '+org/table-insert-column-left)
        :ni "C-S-k" (general-predicate-dispatch 'org-shiftmetaup
                      (org-at-table-p) 'org-table-insert-row)
        :ni "C-S-j" (general-predicate-dispatch 'org-shiftmetadown
                      (org-at-table-p) '+org/table-insert-row-below)
        ;; shifting table rows/columns
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


(defun +org|setup-hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  ;; Open directory links in Emacs
  (add-to-list 'org-file-apps '(directory . emacs))

  (defun +org|delayed-recenter ()
    "`recenter', but after a tiny delay. Necessary to prevent certain race
conditions where a window's buffer hasn't changed at the time this hook is run."
    (run-at-time 0.1 nil #'recenter))
  (add-hook 'org-follow-link-hook #'+org|delayed-recenter)

  ;; Fix variable height text (e.g. org headings) in the eldoc string
  (defun +org*strip-properties-from-outline (orig-fn path &optional width prefix separator)
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
  (advice-add #'org-format-outline-path :around #'+org*strip-properties-from-outline)

  ;; Prevent from temporarily-opened agenda buffers from being associated with
  ;; the current workspace, or being added to recentf. They haven't been opened
  ;; interactively, so shouldn't be treated as if they were.
  (defun +org|exclude-agenda-buffers-from-workspace ()
    (when (and org-agenda-new-buffers (bound-and-true-p persp-mode))
      (let (persp-autokill-buffer-on-remove)
        (persp-remove-buffer org-agenda-new-buffers
                             (get-current-persp)
                             nil))))
  (add-hook 'org-agenda-finalize-hook #'+org|exclude-agenda-buffers-from-workspace)

  (defun +org*exclude-agenda-buffers-from-recentf (orig-fn file)
    (let ((recentf-exclude (list (lambda (_file) t))))
      (funcall orig-fn file)))
  (advice-add #'org-get-agenda-file-buffer
              :around #'+org*exclude-agenda-buffers-from-recentf))


;;
;;; Bootstrap

(def-package! org
  :defer-incrementally
  (calendar find-func format-spec org-macs org-compat org-faces org-entities
   org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
   org-capture)
  :init
  (add-hook! 'org-mode-hook
    #'(org-bullets-mode           ; "prettier" bullets
       org-indent-mode            ; margin-based indentation
       toc-org-enable             ; auto-table of contents
       auto-fill-mode             ; line wrapping
       ;; `show-paren-mode' causes flickering with indentation margins made by
       ;; `org-indent-mode', so we turn off show-paren-mode altogether
       doom|disable-show-paren-mode
       ;; Shows a lot of false positives, so...
       doom|disable-show-trailing-whitespace

       +org|enable-auto-reformat-tables
       +org|enable-auto-update-cookies
       +org|unfold-to-2nd-level-or-point))

  :config
  (+org|setup-ui)
  (+org|setup-agenda)
  (+org|setup-keybinds)
  (+org|setup-hacks)
  (+org|setup-custom-links)

  (add-hook 'org-open-at-point-functions #'doom|set-jump)

  ;; Cross-module configuration
  (set-popup-rules!
    '(("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
      ("^\\*\\(?:Agenda Com\\|Calendar\\|Org \\(?:Export Dispatcher\\|Select\\)\\)"
       :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
      ("^\\*Org Agenda"    :size 0.35 :select t :ttl nil)
      ("^\\*Org Src"       :size 0.3 :quit nil :select t :autosave t :ttl nil)
      ("^CAPTURE.*\\.org$" :size 0.2 :quit nil :select t :autosave t)))

  (set-pretty-symbols! 'org-mode
    :name "#+NAME:"
    :src_block "#+BEGIN_SRC"
    :src_block_end "#+END_SRC")

  (after! smartparens
    (defun +org-sp-point-in-checkbox-p (_id action _context)
      (and (eq action 'insert)
           (sp--looking-at-p "\\s-*]")))

    (defun +org-sp-point-at-bol-p (_id action _context)
      (and (eq action 'insert)
           (eq (char-before) ?*)
           (sp--looking-back-p "^\\**" (line-beginning-position))))

    ;; make delimiter auto-closing a little more conservative
    (sp-with-modes 'org-mode
      (sp-local-pair "*" "*" :unless '(:add sp-point-before-word-p +org-sp-point-at-bol-p))
      (sp-local-pair "_" "_" :unless '(:add sp-point-before-word-p))
      (sp-local-pair "/" "/" :unless '(:add sp-point-before-word-p +org-sp-point-in-checkbox-p))
      (sp-local-pair "~" "~" :unless '(:add sp-point-before-word-p))
      (sp-local-pair "=" "=" :unless '(:add sp-point-before-word-p))))


  ;; Sub-modules
  (if (featurep! +attach)   (load! "+attach"))
  (if (featurep! +babel)    (load! "+babel"))
  (if (featurep! +capture)  (load! "+capture"))
  (if (featurep! +export)   (load! "+export"))
  (if (featurep! +habit)    (load! "+habit"))
  (if (featurep! +present)  (load! "+present"))
  (if (featurep! +protocol) (load! "+protocol")))
