;;; org/org/config.el -*- lexical-binding: t; -*-

;; Ensure ELPA org is prioritized above built-in org.
(when-let (path (locate-library "org" nil doom--package-load-path))
  (cl-pushnew (file-name-directory path) load-path :test #'equal))

;; Custom variables
(defvar +org-dir (expand-file-name "~/work/org/")
  "The directory where org files are kept.")
(defvaralias 'org-directory '+org-dir)

(add-hook 'org-load-hook #'+org|init)
(add-hook 'org-mode-hook #'+org|hook)


;;
;; Plugins
;;

(def-package! toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook #'toc-org-enable))

(def-package! org-crypt ; built-in
  :commands org-crypt-use-before-save-magic
  :init (add-hook 'org-load-hook #'org-crypt-use-before-save-magic)
  :config
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key user-mail-address
        epa-file-encrypt-to user-mail-address))

;; The standard unicode characters are usually misaligned depending on the font.
;; This bugs me. Personally, markdown #-marks for headlines are more elegant, so
;; we use those.
(def-package! org-bullets
  :commands org-bullets-mode
  :init (add-hook 'org-mode-hook #'org-bullets-mode)
  :config (setq org-bullets-bullet-list '("#")))


;;
;; Hooks & bootstraps
;;

(defun +org|hook ()
  "Run everytime `org-mode' is enabled."
  (setq line-spacing 1)
  (doom|disable-line-numbers)

  ;; show-paren-mode causes problems for org-indent-mode
  (make-local-variable 'show-paren-mode)
  (setq show-paren-mode nil)

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
          (org-show-subtree))))))

(defun +org|init ()
  "Run once, when org is first loaded."
  (define-minor-mode +org-evil-mode
    "Evil-mode bindings for org-mode."
    :init-value nil
    :lighter " !"
    :keymap (make-sparse-keymap)
    :group 'evil-org)

  (add-hook 'org-mode-hook #'visual-line-mode)
  (when (featurep! :feature evil)
    (add-hook 'org-mode-hook #'+org-evil-mode))

  (add-hook 'evil-insert-state-exit-hook #'+org|realign-table-maybe nil t)
  (add-hook 'evil-insert-state-exit-hook #'+org|update-cookies nil t)
  (add-hook 'before-save-hook #'+org|update-cookies nil t)

  (+org-init-ui)
  (+org-init-keybinds)
  (+org-hacks))

;;
(defun +org-init-ui ()
  "Configures the UI for `org-mode'."
  (setq-default
   org-adapt-indentation nil
   org-agenda-dim-blocked-tasks nil
   org-agenda-files (directory-files +org-dir t "\\.org$" t)
   org-agenda-inhibit-startup t
   org-agenda-skip-unavailable-files nil
   org-cycle-include-plain-lists t
   org-cycle-separator-lines 1
   ;; org-ellipsis "  "
   org-entities-user '(("flat"  "\\flat" nil "" "" "266D" "♭") ("sharp" "\\sharp" nil "" "" "266F" "♯"))
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
   org-startup-folded t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column 0
   org-use-sub-superscripts '{}
   outline-blank-line t

   ;; LaTeX previews are too small and usually render to light backgrounds, so
   ;; this enlargens them and ensures their background (and foreground) match the
   ;; current theme.
   org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
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

  ;; Custom fontification
  (defsubst +org--tag-face (n)
    (let ((kwd (match-string n)))
      (or (and (equal kwd "#") 'org-tag)
          (and (equal kwd "@") 'org-special-keyword))))

  (defun +org|init-custom-fontification ()
    "Correct (and improve) org-mode's font-lock keywords.

  1. Re-set `org-todo' & `org-headline-done' faces, to make them respect
     underlying faces.
  2. Fontify item bullets
  3. Fontify item checkboxes (and when they're marked done)
  4. Fontify dividers/separators (5+ dashes)
  5. Fontify #hashtags and @at-tags, for personal convenience"
    (let ((org-todo (format org-heading-keyword-regexp-format
                            org-todo-regexp))
          (org-done (format org-heading-keyword-regexp-format
                            (concat "\\(?:" (mapconcat #'regexp-quote org-done-keywords "\\|") "\\)"))))
      (setq
       org-font-lock-extra-keywords
       (append (org-delete-all
                `(("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                   (0 (org-get-checkbox-statistics-face) t))
                  (,org-todo (2 (org-get-todo-face 2) t))
                  (,org-done (2 'org-headline-done t)))
                org-font-lock-extra-keywords)
               `((,org-todo (2 (org-get-todo-face 2) prepend))
                 (,org-done (2 'org-headline-done prepend))
                 ;; Make checkbox statistic cookies respect underlying faces
                 ("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                  (0 (org-get-checkbox-statistics-face) prepend))
                 ;; I like how org-mode fontifies checked TODOs and want this to extend to
                 ;; checked checkbox items:
                 ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
                  1 'org-headline-done prepend)
                 ;; make plain list bullets stand out
                 ("^ *\\([-+]\\|[0-9]+[).]\\) " 1 'org-list-dt append)
                 ;; and separators/dividers
                 ("^ *\\(-----+\\)$" 1 'org-meta-line)
                 ;; custom #hashtags & @at-tags for another level of organization
                 ("\\s-\\(\\([#@]\\)[^ \n.,]+\\)" 1 (+org--tag-face 2)))))))
  (add-hook 'org-font-lock-set-keywords-hook #'+org|init-custom-fontification))

(defun +org-init-keybinds ()
  "Sets up org-mode and evil keybindings. Tries to fix the idiosyncrasies
between the two."
  (map! (:map org-mode-map "RET" #'org-return-indent)

        (:map +org-evil-mode-map
          :n "RET" #'+org/dwim-at-point

          ;; Navigate table cells (from insert-mode)
          :i "C-L" #'+org/table-next-field
          :i "C-H" #'+org/table-previous-field
          :i "C-K" #'+org/table-previous-row
          :i "C-J" #'+org/table-next-row
          ;; Expand tables (or shiftmeta move)
          :ni "C-S-l" #'+org/table-append-field-or-shift-right
          :ni "C-S-h" #'+org/table-prepend-field-or-shift-left
          :ni "C-S-k" #'+org/table-prepend-row-or-shift-up
          :ni "C-S-j" #'+org/table-append-row-or-shift-down

          :n  [tab]     #'+org/toggle-fold
          :i  [tab]     #'+org/indent-or-next-field-or-yas-expand
          :i  [backtab] #'+org/dedent-or-prev-field

          :ni [M-return]   (λ! (+org/insert-item 'below))
          :ni [S-M-return] (λ! (+org/insert-item 'above))

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
          :m  "<tab>" #'org-cycle

          ;; Fix code-folding keybindings
          :n  "za"  #'+org/toggle-fold
          :n  "zA"  #'org-shifttab
          :n  "zc"  #'outline-hide-subtree
          :n  "zC"  (λ! (outline-hide-sublevels 1))
          :n  "zd"  (lambda (&optional arg) (interactive "p") (outline-hide-sublevels (or arg 3)))
          :n  "zm"  (λ! (outline-hide-sublevels 1))
          :n  "zo"  #'outline-show-subtree
          :n  "zO"  #'outline-show-all
          :n  "zr"  #'outline-show-all)

        (:after org-agenda
          (:map org-agenda-mode-map
            :e "<escape>" #'org-agenda-Quit
            :e "m"   #'org-agenda-month-view
            :e "C-j" #'org-agenda-next-item
            :e "C-k" #'org-agenda-previous-item
            :e "C-n" #'org-agenda-next-item
            :e "C-p" #'org-agenda-previous-item))))

;;
(defun +org-hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (cl-pushnew '(file . find-file) org-link-frame-setup)

  ;; Let OS decide what to do with files when opened
  (setq org-file-apps
        `(("\\.org$" . emacs)
          (t . ,(cond (IS-MAC "open -R \"%s\"")
                      (IS-LINUX "xdg-open \"%s\"")))))

  ;; Remove highlights on ESC
  (defun +org|remove-occur-highlights ()
    (when (derived-mode-p 'org-mode)
      (org-remove-occur-highlights)
      t))
  (add-hook '+evil-esc-hook #'+org|remove-occur-highlights)

  (after! recentf
    ;; Don't clobber recentf with agenda files
    (defun +org-is-agenda-file (filename)
      (cl-find (file-truename filename) org-agenda-files
               :key #'file-truename
               :test #'equal))
    (add-to-list 'recentf-exclude #'+org-is-agenda-file)))
