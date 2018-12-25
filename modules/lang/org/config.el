;;; lang/org/config.el -*- lexical-binding: t; -*-

;; FIXME deprecated
(define-obsolete-variable-alias '+org-dir 'org-directory "2.1.0")

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

;; Sub-modules
(if (featurep! +attach)  (load! "+attach"))
(if (featurep! +babel)   (load! "+babel"))
(if (featurep! +capture) (load! "+capture"))
(if (featurep! +export)  (load! "+export"))
(if (featurep! +present) (load! "+present"))
;; TODO (if (featurep! +publish) (load! "+publish"))

(doom-load-packages-incrementally
 '(calendar find-func format-spec org-macs org-compat
   org-faces org-entities org-list org-pcomplete org-src
   org-footnote org-macro ob org org-agenda org-capture))


;;
;; Packages

;; `toc-org'
(setq toc-org-hrefify-default "org")

(def-package! evil-org
  :when (featurep! :feature evil +everywhere)
  :hook (org-mode . evil-org-mode)
  :init
  (defvar evil-org-key-theme '(navigation insert textobjects))
  (defvar evil-org-special-o/O '(table-row))
  (add-hook 'org-load-hook #'+org|setup-evil)
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  :config
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

(def-package! org-yt
  :after org
  :config
  (defun +org-inline-data-image (_protocol link _description)
    "Interpret LINK as base64-encoded image data."
    (base64-decode-string link))

  (defun +org-image-link (protocol link _description)
    "Interpret LINK as base64-encoded image data."
    (when (image-type-from-file-name link)
      (if-let* ((buf (url-retrieve-synchronously (concat protocol ":" link))))
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward "\r?\n\r?\n" nil t)
            (buffer-substring-no-properties (point) (point-max)))
        (message "Download of image \"%s\" failed" link)
        nil)))

  (org-link-set-parameters "http"  :image-data-fun #'+org-image-link)
  (org-link-set-parameters "https" :image-data-fun #'+org-image-link)
  (org-link-set-parameters "img"   :image-data-fun #'+org-inline-data-image))


;;
;; Bootstrap

(add-hook! 'org-load-hook
  #'(+org|setup-ui
     +org|setup-popup-rules
     +org|setup-agenda
     +org|setup-keybinds
     +org|setup-hacks
     +org|setup-pretty-code
     +org|setup-custom-links))

(add-hook! 'org-mode-hook
  #'(doom|disable-line-numbers  ; org doesn't really need em
     org-bullets-mode           ; "prettier" bullets
     org-indent-mode            ; margin-based indentation
     toc-org-enable             ; auto-table of contents
     auto-fill-mode             ; line wrapping
     ;; `show-paren-mode' causes flickering with indentation margins made by
     ;; `org-indent-mode', so we simply turn off show-paren-mode altogether."
     doom|disable-show-paren-mode

     +org|enable-auto-reformat-tables
     +org|enable-auto-update-cookies
     +org|smartparens-compatibility-config
     +org|unfold-to-2nd-level-or-point))


;;
;; `org-mode' hooks

(defun +org|unfold-to-2nd-level-or-point ()
  "My version of the 'overview' #+STARTUP option: expand first-level headings.
Expands the first level, but no further. If point was left somewhere deeper,
unfold to point on startup."
  (unless org-agenda-inhibit-startup
    (when (eq org-startup-folded t)
      (outline-hide-sublevels 2))
    (when (outline-invisible-p)
      (ignore-errors
        (save-excursion
          (outline-previous-visible-heading 1)
          (org-show-subtree))))))

(defun +org|smartparens-compatibility-config ()
  "Instruct `smartparens' not to impose itself in org-mode."
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
      (sp-local-pair "*" nil :unless '(:add sp-point-before-word-p +org-sp-point-at-bol-p))
      (sp-local-pair "_" nil :unless '(:add sp-point-before-word-p))
      (sp-local-pair "/" nil :unless '(:add sp-point-before-word-p +org-sp-point-in-checkbox-p))
      (sp-local-pair "~" nil :unless '(:add sp-point-before-word-p))
      (sp-local-pair "=" nil :unless '(:add sp-point-before-word-p)))))

(defun +org|enable-auto-reformat-tables ()
  "Realign tables & update formulas when exiting insert mode (`evil-mode')."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org|realign-table-maybe nil t)
    (add-hook 'evil-replace-state-exit-hook #'+org|realign-table-maybe nil t)
    (advice-add #'evil-replace :after #'+org*realign-table-maybe)))

(defun +org|enable-auto-update-cookies ()
  "Update statistics cookies when saving or exiting insert mode (`evil-mode')."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org|update-cookies nil t))
  (add-hook 'before-save-hook #'+org|update-cookies nil t))


;;
;; `org-load' hooks

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

(defun +org|setup-popup-rules ()
  "Defines popup rules for org-mode (does nothing if :ui popup is disabled)."
  (set-popup-rules!
    '(("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
      ("^\\*\\(?:Agenda Com\\|Calendar\\|Org \\(?:Export Dispatcher\\|Select\\)\\)"
       :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
      ("^\\*Org Agenda"    :size 0.35 :select t :ttl nil)
      ("^\\*Org Src"       :size 0.3 :quit nil :select t :autosave t :ttl nil)
      ("^CAPTURE.*\\.org$" :size 0.2 :quit nil :select t :autosave t))))

(defun +org|setup-pretty-code ()
  "Setup the default pretty symbols for"
  (set-pretty-symbols! 'org-mode
    :name "#+NAME:"
    :src_block "#+BEGIN_SRC"
    :src_block_end "#+END_SRC"))

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
  (+org-def-link "doom-modules" doom-modules-dir))

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
   '((sequence "[ ](t)" "[-](p)" "[?](m)" "|" "[X](d)")
     (sequence "TODO(T)" "|" "DONE(D)")
     (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
   org-todo-keyword-faces
   '(("[-]" :inherit font-lock-constant-face :weight bold)
     ("[?]" :inherit warning :weight bold)
     ("WAITING" :inherit default :weight bold)
     ("LATER" :inherit warning :weight bold))
   org-use-sub-superscripts '{}

   ;; Scale up LaTeX previews a bit (default is too small)
   org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")
   org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

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

  (define-key! org-mode-map
    (kbd "C-c C-S-l") #'+org/remove-link
    (kbd "C-c C-i")   #'org-toggle-inline-images
    [remap doom/backward-to-bol-or-indent]          #'org-beginning-of-line
    [remap doom/forward-to-last-non-comment-or-eol] #'org-end-of-line))

(defun +org|setup-evil (&rest args)
  ;; In case this hook is used in an advice on `evil-org-set-key-theme', this
  ;; prevents recursive requires.
  (unless args (require 'evil-org))

  (add-hook 'org-tab-first-hook #'+org|cycle-only-current-subtree t)
  (advice-add #'org-return-indent :after #'+org*fix-newline-and-indent-in-src-blocks)

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
        ;; dedent with shift-tab in insert mode
        :i [backtab] #'+org/dedent
        ;; navigate table cells (from insert-mode)
        :i "C-l" #'+org/table-next-field
        :i "C-h" #'+org/table-previous-field
        :i "C-k" #'+org/table-previous-row
        :i "C-j" #'+org/table-next-row
        ;; expand tables or move fields
        :ni "C-S-l" #'+org/table-append-field-or-shift-right
        :ni "C-S-h" #'+org/table-prepend-field-or-shift-left
        :ni "C-S-k" #'org-metaup
        :ni "C-S-j" #'org-metadown
        ;; more intuitive RET keybinds
        :i [return] #'org-return-indent
        :n [return] #'+org/dwim-at-point
        :nvi [C-return]   (λ! (+org/insert-item 'below))
        :nvi [C-S-return] (λ! (+org/insert-item 'above))
        (:when IS-MAC
          ;; textmate-esque newline insertion
          :nvi [s-return]    (λ! (+org/insert-item 'below))
          :nvi [S-s-return]  (λ! (+org/insert-item 'above)))
        ;; more vim-esque org motion keys (not covered by evil-org-mode)
        :m "]]"  (λ! (org-forward-heading-same-level nil) (org-beginning-of-line))
        :m "[["  (λ! (org-backward-heading-same-level nil) (org-beginning-of-line))
        :m "]h"  #'org-next-visible-heading
        :m "[h"  #'org-previous-visible-heading
        :m "]l"  #'org-next-link
        :m "[l"  #'org-previous-link
        :m "]s"  #'org-babel-next-src-block
        :m "[s"  #'org-babel-previous-src-block
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

        :map org-read-date-minibuffer-local-map
        "C-h"   (λ! (org-eval-in-calendar '(calendar-backward-day 1)))
        "C-l"   (λ! (org-eval-in-calendar '(calendar-forward-day 1)))
        "C-k"   (λ! (org-eval-in-calendar '(calendar-backward-week 1)))
        "C-j"   (λ! (org-eval-in-calendar '(calendar-forward-week 1)))
        "C-S-h" (λ! (org-eval-in-calendar '(calendar-backward-month 1)))
        "C-S-l" (λ! (org-eval-in-calendar '(calendar-forward-month 1)))
        "C-S-k" (λ! (org-eval-in-calendar '(calendar-backward-year 1)))
        "C-S-j" (λ! (org-eval-in-calendar '(calendar-forward-year 1)))

        :localleader
        :map org-mode-map
        "d" #'org-deadline
        "t" #'org-todo
        (:prefix ("c" . "clock")
          "c" #'org-clock-in
          "C" #'org-clock-out
          "g" #'org-clock-goto
          "G" (λ! (org-clock-goto 'select))
          "x" #'org-clock-cancel)))

(defun +org|setup-hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (setf (alist-get 'file org-link-frame-setup) #'find-file)

  (defun +org|delayed-recenter ()
    "`recenter', but after a tiny delay. Necessary to prevent certain race
conditions where a window's buffer hasn't changed at the time this hook is run."
    (run-at-time 0.1 nil #'recenter))
  (add-hook 'org-follow-link-hook #'+org|delayed-recenter)

  ;; Fix variable height org-level-N faces in the eldoc string
  (defun +org*format-outline-path (orig-fn path &optional width prefix separator)
    (let ((result (funcall orig-fn path width prefix separator))
          (separator (or separator "/")))
      (string-join
       (cl-loop for part
                in (split-string (substring-no-properties result) separator)
                for n from 0
                for face = (nth (% n org-n-level-faces) org-level-faces)
                collect (org-add-props part nil 'face `(:foreground ,(face-foreground face nil t) :weight bold)))
       separator)))
  (advice-add #'org-format-outline-path :around #'+org*format-outline-path)

  (setq org-file-apps
        `(("pdf" . default)
          ("\\.x?html?\\'" . default)
          ("/docs/" . emacs)
          (auto-mode . emacs)
          (directory . emacs)
          (t . ,(cond (IS-MAC "open -R \"%s\"")
                      (IS-LINUX "xdg-open \"%s\"")))))

  (defun +org|exclude-agenda-buffers-from-workspace ()
    (when org-agenda-new-buffers
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
;; Built-in libraries

(def-package! org-crypt ; built-in
  :commands org-encrypt-entries
  :hook (org-reveal-start . org-decrypt-entry)
  :init
  (add-hook! 'org-mode-hook
    (add-hook 'before-save-hook 'org-encrypt-entries nil t))
  :config
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key user-mail-address))

(def-package! org-clock
  :commands org-clock-save
  :hook (org-mode . org-clock-load)
  :defer-incrementally t
  :init
  (setq org-clock-persist 'history
        org-clock-persist-file (concat doom-etc-dir "org-clock-save.el"))
  :config
  (add-hook 'kill-emacs-hook #'org-clock-save))


;; In case org has already been loaded (or you're running `doom/reload')
(when (featurep 'org)
  (run-hooks 'org-load-hook))
