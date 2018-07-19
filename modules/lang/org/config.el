;;; lang/org/config.el -*- lexical-binding: t; -*-

;; FIXME deprecated
(define-obsolete-variable-alias '+org-dir 'org-directory "2.1.0")

(defvar org-directory "~/org/")

;; Sub-modules
(if (featurep! +attach)  (load! "+attach"))
(if (featurep! +babel)   (load! "+babel"))
(if (featurep! +capture) (load! "+capture"))
(if (featurep! +export)  (load! "+export"))
(if (featurep! +present) (load! "+present"))
;; TODO (if (featurep! +publish) (load! "+publish"))


;;
;; Plugins
;;

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


;;
;; Bootstrap
;;

(add-hook! 'org-load-hook
  #'(org-crypt-use-before-save-magic
     +org|setup-ui
     +org|setup-popup-rules
     +org|setup-agenda
     +org|setup-keybinds
     +org|setup-hacks
     +org|setup-pretty-code))

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
;;

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
;;

(defun +org|setup-agenda ()
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
      ("^\\*Org Src"       :size 0.3 :quit nil :select t)
      ("^CAPTURE.*\\.org$" :size 0.2 :quit nil :select t :autosave t))))

(defun +org|setup-pretty-code ()
  "Setup the default pretty symbols for"
  (set-pretty-symbols! 'org-mode
    :name "#+NAME:"
    :src_block "#+BEGIN_SRC"
    :src_block_end "#+END_SRC"))

(defun +org|setup-ui ()
  "Configures the UI for `org-mode'."
  (setq-default
   org-adapt-indentation nil
   org-cycle-include-plain-lists t
   org-cycle-separator-lines 1
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
   org-startup-folded t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column 0
   org-todo-keywords
   '((sequence "[ ](t)" "[-](p)" "[?](m)" "|" "[X](d)")
     (sequence "TODO(T)" "|" "DONE(D)")
     (sequence "NEXT(n)" "ACTIVE(a)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
   org-use-sub-superscripts '{}
   outline-blank-line t

   ;; Scale up LaTeX previews a bit (default is too small)
   org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")
   org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

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
  (add-hook 'doom-load-theme-hook #'+org|update-latex-preview-background-color)

  ;; Custom links
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
           (unless (file-remote-p path)
             (if (file-exists-p path) 'org-link 'error))))

  (eval-when-compile
    (defmacro def-org-file-link! (key dir)
      `(org-link-set-parameters
        ,key
        :complete (lambda () (+org--relpath (+org-link-read-file ,key ,dir) ,dir))
        :follow   (lambda (link) (find-file (expand-file-name link ,dir)))
        :face     (lambda (link)
                    (if (file-exists-p (expand-file-name link ,dir))
                        'org-link
                      'error)))))

  (def-org-file-link! "org" org-directory)
  (def-org-file-link! "doom" doom-emacs-dir)
  (def-org-file-link! "doom-docs" doom-docs-dir)
  (def-org-file-link! "doom-modules" doom-modules-dir))

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
  (evil-define-key* 'normal outline-mode-map
    "^" nil
    [backtab] nil
    "\M-j" nil "\M-k" nil
    "\C-j" nil "\C-k" nil
    "]" nil "[" nil)
  (evil-define-key* 'insert evil-org-mode-map
    ;; dedent with shift-tab in insert mode
    [backtab] #'+org/dedent
    ;; navigate table cells (from insert-mode)
    "\C-l" #'+org/table-next-field
    "\C-h" #'+org/table-previous-field
    "\C-k" #'+org/table-previous-row
    "\C-j" #'+org/table-next-row)
  ;; expand tables or move fields
  (evil-define-key* '(insert normal) evil-org-mode-map
    (kbd "C-S-l") #'+org/table-append-field-or-shift-right
    (kbd "C-S-h") #'+org/table-prepend-field-or-shift-left
    (kbd "C-S-k") #'org-metaup
    (kbd "C-S-j") #'org-metadown)
  ;; more intuitive RET keybinds
  (evil-define-key* 'insert evil-org-mode-map
    [return] #'org-return-indent)
  (evil-define-key* 'normal evil-org-mode-map
    [return] #'+org/dwim-at-point)
  (evil-define-key* '(insert normal) evil-org-mode-map
    [M-return]   (λ! (+org/insert-item 'below))
    [S-M-return] (λ! (+org/insert-item 'above)))
  ;; more vim-esque org motion keys
  (evil-define-key* 'motion evil-org-mode-map
    "]]"  (λ! (org-forward-heading-same-level nil) (org-beginning-of-line))
    "[["  (λ! (org-backward-heading-same-level nil) (org-beginning-of-line))
    "]h"  #'org-next-visible-heading
    "[h"  #'org-previous-visible-heading
    "]l"  #'org-next-link
    "[l"  #'org-previous-link
    "]s"  #'org-babel-next-src-block
    "[s"  #'org-babel-previous-src-block
    "^"   #'evil-org-beginning-of-line
    "0"   (λ! (let (visual-line-mode) (org-beginning-of-line))))
  (evil-define-key* 'normal evil-org-mode-map
    "gQ"  #'org-fill-paragraph
    ;; sensible vim-esque folding keybinds
    "za"  #'+org/toggle-fold
    "zA"  #'org-shifttab
    "zc"  #'+org/close-fold
    "zC"  #'outline-hide-subtree
    "zm"  #'+org/hide-next-fold-level
    "zo"  #'+org/open-fold
    "zO"  #'outline-show-subtree
    "zr"  #'+org/show-next-fold-level
    "zR"  #'outline-show-all)
  ;; <localleader>
  (map! :map evil-org-mode-map
        :localleader
        :n "d" #'org-deadline
        :n "t" #'org-todo
        (:desc "clock" :prefix "c"
          :n "c" #'org-clock-in
          :n "C" #'org-clock-out
          :n "g" #'org-clock-goto
          :n "G" (λ! (org-clock-goto 'select))
          :n "x" #'org-clock-cancel)))

(defun +org|setup-hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (setf (alist-get 'file org-link-frame-setup) #'find-file)

  ;; Fix variable height org-level-N faces in the eldoc string
  (defun +org*fix-font-size-variation-in-eldoc (orig-fn)
    (cl-letf (((symbol-function 'org-format-outline-path)
               (lambda (path &optional _width _prefix separator)
                 (string-join
                  (cl-loop with i = -1
                           for seg in (delq nil path)
                           for face = (nth (% (cl-incf i) org-n-level-faces) org-level-faces)
                           for spec = (face-all-attributes face)
                           collect (propertize (replace-regexp-in-string "[ \t]+\\'" "" seg)
                                               'face (if face `(:foreground ,(face-foreground face)))))
                  separator))))
      (funcall orig-fn)))
  (advice-add #'org-eldoc-get-breadcrumb :around #'+org*fix-font-size-variation-in-eldoc)

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

  (defun +org*exclude-agenda-buffers-from-recentf (orig-fn &rest args)
    (let ((recentf-exclude (list (lambda (_file) t))))
      (apply orig-fn args)))
  (advice-add #'org-get-agenda-file-buffer
              :around #'+org*exclude-agenda-buffers-from-recentf))


;;
;; Built-in libraries
;;

(def-package! org-crypt ; built-in
  :commands org-crypt-use-before-save-magic
  :config
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key user-mail-address))

(def-package! org-clock
  :commands org-clock-save
  :hook (org-mode . org-clock-load)
  :init
  (setq org-clock-persist 'history
        org-clock-persist-file (concat doom-etc-dir "org-clock-save.el"))
  :config
  (add-hook 'kill-emacs-hook #'org-clock-save))

;;
(when (featurep 'org)
  (run-hooks 'org-load-hook))
