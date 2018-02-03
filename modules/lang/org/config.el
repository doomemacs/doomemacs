;;; lang/org/config.el -*- lexical-binding: t; -*-

(defvar +org-dir (expand-file-name "~/work/org/")
  "The directory where org files are kept.")

;; Sub-modules
(if (featurep! +attach)  (load! +attach))
(if (featurep! +babel)   (load! +babel))
(if (featurep! +capture) (load! +capture))
(if (featurep! +export)  (load! +export))
(if (featurep! +present) (load! +present))
;; TODO (if (featurep! +publish) (load! +publish))


;;
;; Plugins
;;

(def-package! toc-org
  :commands toc-org-enable
  :config (setq toc-org-hrefify-default "org"))

(def-package! org-crypt ; built-in
  :commands org-crypt-use-before-save-magic
  :config
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key user-mail-address))

(def-package! org-bullets
  :commands org-bullets-mode)


;;
;; Bootstrap
;;

(add-hook! 'org-load-hook
  #'(org-crypt-use-before-save-magic
     +org|setup-ui
     +org|setup-agenda
     +org|setup-keybinds
     +org|setup-hacks))

(add-hook! 'org-mode-hook
  #'(doom|disable-line-numbers  ; org doesn't really need em
     org-bullets-mode           ; "prettier" bullets
     org-indent-mode            ; margin-based indentation
     toc-org-enable             ; auto-table of contents
     visual-line-mode           ; line wrapping

     +org|enable-auto-reformat-tables
     +org|enable-auto-update-cookies
     +org|smartparens-compatibility-config
     +org|unfold-to-2nd-level-or-point
     +org|show-paren-mode-compatibility
     ))

(after! org
  (defvaralias 'org-directory '+org-dir))

(when (featurep 'org)
  (run-hooks 'org-load-hook))


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
  (defun +org-sp-point-in-checkbox-p (_id action _context)
    (when (eq action 'insert)
      (sp--looking-at-p "\\s-*]")))

  ;; make delimiter auto-closing a little more conservative
  (after! smartparens
    (sp-with-modes 'org-mode
      (sp-local-pair "*" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-at-bol-p))
      (sp-local-pair "_" nil :unless '(sp-point-after-word-p sp-point-before-word-p))
      (sp-local-pair "/" nil :unless '(sp-point-after-word-p sp-point-before-word-p +org-sp-point-in-checkbox-p))
      (sp-local-pair "~" nil :unless '(sp-point-after-word-p sp-point-before-word-p))
      (sp-local-pair "=" nil :unless '(sp-point-after-word-p sp-point-before-word-p)))))

(defun +org|enable-auto-reformat-tables ()
  "Realign tables exiting insert mode (`evil-mode')."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org|realign-table-maybe nil t)))

(defun +org|enable-auto-update-cookies ()
  "Update statistics cookies when saving or exiting insert mode (`evil-mode')."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org|update-cookies nil t))
  (add-hook 'before-save-hook #'+org|update-cookies nil t))

(defun +org|show-paren-mode-compatibility ()
  "`show-paren-mode' causes flickering with indentation margins made by
`org-indent-mode', so we simply turn off show-paren-mode altogether."
  (set (make-local-variable 'show-paren-mode) nil))


;;
;; `org-load' hooks
;;

(defun +org|setup-agenda ()
  (setq-default
   org-agenda-dim-blocked-tasks nil
   org-agenda-files (ignore-errors (directory-files +org-dir t "\\.org$" t))
   org-agenda-inhibit-startup t
   org-agenda-skip-unavailable-files t))

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
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts t
   org-priority-faces
   `((?a . ,(face-foreground 'error))
     (?b . ,(face-foreground 'warning))
     (?c . ,(face-foreground 'success)))
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

   ;; LaTeX previews are too small and usually render to light backgrounds, so
   ;; this enlargens them and ensures their background (and foreground) match
   ;; the current theme.
   org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")
   org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
   org-format-latex-options
   (plist-put org-format-latex-options
              :background (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                              'default)
                                          :background nil t)))

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

  (defmacro def-org-file-link! (key dir)
    `(org-link-set-parameters
      ,key
      :complete (lambda () (+org--relpath (+org-link-read-file ,key ,dir) ,dir))
      :follow   (lambda (link) (find-file (expand-file-name link ,dir)))
      :face     (lambda (link)
                  (if (file-exists-p (expand-file-name link ,dir))
                      'org-link
                    '(:inherit (error underline))))))

  (def-org-file-link! "org" +org-dir)
  (def-org-file-link! "doom" doom-emacs-dir)
  (def-org-file-link! "doom-module" doom-modules-dir)
  (def-org-file-link! "doom-docs" doom-docs-dir)

  ;; Update UI when theme is changed
  (add-hook 'doom-init-theme-hook #'+org|setup-ui))

(defun +org|setup-keybinds ()
  "Sets up org-mode and evil keybindings. Tries to fix the idiosyncrasies
between the two."
  (map! :map org-mode-map
        "RET" #'org-return-indent
        "C-c C-S-l" #'+org/remove-link
        :n "C-c C-i" #'org-toggle-inline-images

        :n  "RET" #'+org/dwim-at-point

        ;; Navigate table cells (from insert-mode)
        :i  "C-l"   #'+org/table-next-field
        :i  "C-h"   #'+org/table-previous-field
        :i  "C-k"   #'+org/table-previous-row
        :i  "C-j"   #'+org/table-next-row
        ;; Expand tables (or shiftmeta move)
        :ni "C-S-l" #'+org/table-append-field-or-shift-right
        :ni "C-S-h" #'+org/table-prepend-field-or-shift-left
        :ni "C-S-k" #'org-metaup
        :ni "C-S-j" #'org-metadown

        :n  [tab]     #'+org/toggle-fold
        :i  [tab]     #'+org/indent-or-next-field-or-yas-expand
        :i  [backtab] #'+org/dedent-or-prev-field

        :ni [M-return]   (λ! (+org/insert-item 'below))
        :ni [S-M-return] (λ! (+org/insert-item 'above))

        ;; Fix vim motion keys
        :m  "]]"  (λ! (org-forward-heading-same-level nil) (org-beginning-of-line))
        :m  "[["  (λ! (org-backward-heading-same-level nil) (org-beginning-of-line))
        :m  "]l"  #'org-next-link
        :m  "[l"  #'org-previous-link
        :m  "$"   #'org-end-of-line
        :m  "^"   #'org-beginning-of-line
        :n  "gQ"  #'org-fill-paragraph
        :n  "<"   #'org-metaleft
        :n  ">"   #'org-metaright
        :v  "<"   (λ! (org-metaleft)  (evil-visual-restore))
        :v  ">"   (λ! (org-metaright) (evil-visual-restore))
        ;; Fix code-folding keybindings
        :n  "za"  #'+org/toggle-fold
        :n  "zA"  #'org-shifttab
        :n  "zc"  #'outline-hide-subtree
        :n  "zC"  (λ! (outline-hide-sublevels 1))
        :n  "zd"  (lambda (&optional arg) (interactive "p") (outline-hide-sublevels (or arg 3)))
        :n  "zm"  (λ! (outline-hide-sublevels 1))
        :n  "zo"  #'outline-show-subtree
        :n  "zO"  #'outline-show-all
        :n  "zr"  #'outline-show-all

        (:after org-agenda
          (:map org-agenda-mode-map
            :e "m"   #'org-agenda-month-view
            :e "C-j" #'org-agenda-next-item
            :e "C-k" #'org-agenda-previous-item
            :e "C-n" #'org-agenda-next-item
            :e "C-p" #'org-agenda-previous-item))))

(defun +org|setup-hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (map-put org-link-frame-setup 'file 'find-file)

  (defun +org|cleanup-agenda-files ()
    "Close leftover agenda buffers after they've been indexed by org-agenda."
    (cl-loop for file in org-agenda-files
             for buf = (get-file-buffer file)
             if (and file (not (get-buffer-window buf)))
             do (kill-buffer buf)))
  (add-hook 'org-agenda-finalize-hook #'+org|cleanup-agenda-files)

  ;; Let OS decide what to do with files when opened
  (setq org-file-apps
        `(("pdf" . default)
          ("\\.x?html?\\'" . default)
          (auto-mode . emacs)
          (directory . emacs)
          (t . ,(cond (IS-MAC "open -R \"%s\"")
                      (IS-LINUX "xdg-open \"%s\"")))))

  (defun +org|remove-occur-highlights ()
    "Remove org occur highlights on ESC in normal mode."
    (when (and (derived-mode-p 'org-mode)
               org-occur-highlights)
      (org-remove-occur-highlights)))
  (add-hook 'doom-escape-hook #'+org|remove-occur-highlights)

  (after! recentf
    ;; Don't clobber recentf with agenda files
    (defun +org-is-agenda-file (filename)
      (cl-find (file-truename filename) org-agenda-files
               :key #'file-truename
               :test #'equal))
    (push #'+org-is-agenda-file recentf-exclude)))
