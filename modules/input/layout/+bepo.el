;;; input/layout/+bepo.el -*- lexical-binding: t; -*-

;; NOTE: the evaluation loads the whole autoload/bepo.el file but it doesn't really matter as other
;; functions are eagerly called in this block
;; NOTE: since this file is loaded before $DOOMDIR/config.el, the cr-rotation-style variable
;; if not default needs to be set up in $DOOMDIR/init.el
(fset 'doom-bepo--evil-collection-hook
      (doom-bepo-rotate-collection-keymaps-h-builder doom-bepo-cr-rotation-style))
(add-hook 'evil-collection-setup-hook #'doom-bepo--evil-collection-hook)

;; Highlight non breaking spaces as error in prog modes only
;; TODO: this variable is defined in a file called xdisp.c Will that work in non-X builds ?
(setq nobreak-char-display t)
(set-face-attribute 'nobreak-space nil :underline t)

(add-transient-hook! 'doom-init-modules-hook
  ;; "ts" would be a little too common for an evil escape sequence
  (setq evil-escape-key-sequence "gq")
  (setq avy-keys '(?a ?u ?i ?e ?, ?c ?t ?s ?r ?n)
        lispy-avy-keys '(?a ?u ?i ?e ?, ?c ?t ?s ?r ?n ?m ?b ?é ?p ?o ?è ?v ?d ?l ?j ?z))
  ;; :ui window-select settings, ignoring +numbers flag for now
  (after! ace-window
    (setq aw-keys '(?a ?u ?i ?e ?, ?c ?t ?s ?r ?n)))
  (after! switch-window
    (setq switch-window-shortcut-style 'qwerty
          switch-window-qwerty-shortcuts '("a" "u" "i" "e" "," "c" "t" "s" "r")))

  (doom-bepo-rotate-ts-bare-keymap '(read-expression-map))
  (doom-bepo-rotate-bare-keymap '(evil-window-map) doom-bepo-cr-rotation-style)

  (map! :i "C-t" #'+default-newline
        (:when (featurep! :editor multiple-cursors)
         :prefix "gz"
         :nv "t" #'evil-mc-make-cursor-move-next-line
         :nv "s" #'evil-mc-make-cursor-move-prev-line
         ;; the old toggle mapping (t) is made available both on "T" for mnemonics and
         ;; "j" as a "classic" rotation
         :nv "T" #'+multiple-cursors/evil-mc-toggle-cursors
         :nv "j" #'+multiple-cursors/evil-mc-toggle-cursors)
        (:when (featurep! :ui popup)
         :n "C-$"   #'+popup/toggle
         :n "C-#"   #'+popup/raise))
  (map!
   :leader
   :desc "Window" "é" 'evil-window-map
   (:when (featurep! :ui popup)
    :desc "Toggle last popup"     "#"    #'+popup/toggle)
   (:when (featurep! :ui workspaces)
    :desc "Switch buffer"           "«" #'switch-to-buffer)
   :desc "Switch to last buffer" "$"    #'evil-switch-to-windows-last-buffer
   (:when (featurep! :ui workspaces)
    (:prefix-map ("TAB" . "workspace")
     :desc "Switch to last workspace"  "$"   #'+workspace/other
     :desc "Next workspace"            ")"   #'+workspace/switch-right
     :desc "Previous workspace"        "("   #'+workspace/switch-left))
   (:prefix-map ("b" . "buffer")
    :desc "Previous buffer"             "("   #'previous-buffer
    :desc "Next buffer"                 ")"   #'next-buffer)
   (:prefix-map ("c" . "code")
    :desc "Jump to documentation"                 "S"   #'+lookup/documentation)
   (:prefix-map ("g" . "git")
    (:when (featurep! :ui vc-gutter)
     :desc "Jump to next hunk"         ")"   #'git-gutter:next-hunk
     :desc "Jump to previous hunk"     "("   #'git-gutter:previous-hunk))
   (:prefix-map ("p" . "project")
    :desc "Browse other project"         "»" #'doom/browse-in-other-project))
  (after! treemacs
    (doom-bepo-rotate-ts-bare-keymap '(evil-treemacs-state-map)))
  (after! (:or helm ivy)
    (doom-bepo-rotate-bare-keymap +default-minibuffer-maps doom-bepo-cr-rotation-style))
  (after! company
    (doom-bepo-rotate-bare-keymap '(company-active-map company-search-map) doom-bepo-cr-rotation-style))
  (after! helm
    (doom-bepo-rotate-bare-keymap '(helm-map) doom-bepo-cr-rotation-style))
  (after! general
    (doom-bepo-rotate-evil-keymap doom-bepo-cr-rotation-style))
  (after! evil-snipe
    (doom-bepo--evil-collection-hook
     nil
     '(evil-snipe-local-mode-map evil-snipe-override-local-mode-map)))
  (after! lispyville
    ;; <> en direct
    (general-translate-key '(normal motion) 'lispyville-mode-map
      "«" "<"
      "»" ">"))
  (after! org
    (defadvice! doom-bepo--org-completing-read (&rest args)
      "Completing-read with SPACE being a normal character, and C-c mapping left alone."
      :override #'org-completing-read
      (let ((enable-recursive-minibuffers t)
            (minibuffer-local-completion-map
             (copy-keymap minibuffer-local-completion-map)))
        (define-key minibuffer-local-completion-map " " 'self-insert-command)
        (define-key minibuffer-local-completion-map "?" 'self-insert-command)
        (define-key minibuffer-local-completion-map
          (cond
           ((eq doom-bepo-cr-rotation-style 'ergodis)
            (kbd "C-l !"))
           (t
            (kbd "C-h !")))
          'org-time-stamp-inactive)
        (apply #'completing-read args))))
  (after! (evil magit evil-magit)
    (doom-bepo-rotate-ts-bare-keymap
     '(magit-mode-map
       magit-diff-section-base-map
       magit-staged-section-map
       magit-unstaged-section-map
       magit-untracked-section-map))
    ;; Without this, "s" is mapped to 'magit-delete-thing (the old "k" for "kill") and
    ;; takes precedence over the evil command to go up one line
    (map! :map magit-mode-map "s" nil)
    (doom-bepo--evil-collection-hook
     nil
     '(magit-mode-map
       magit-cherry-mode-map
       magit-mode-map
       magit-blob-mode-map
       magit-diff-mode-map
       magit-log-mode-map
       magit-log-select-mode-map
       magit-reflog-mode-map
       magit-status-mode-map
       magit-file-mode-map
       magit-log-read-revs-map
       magit-process-mode-map
       magit-refs-mode-map)))
  (after! evil-easymotion
    (doom-bepo-rotate-bare-keymap '(evilem-map) doom-bepo-cr-rotation-style)))
