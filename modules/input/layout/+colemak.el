;;; input/layout/+colemak.el -*- lexical-binding: t; -*-

;; Colemak keeps exactly the same keys as QWERTY, just moving them around
;; Therefore, it only makes sense to move the ergonomic HJKL, and any conflicting keys
;; This does not remap any keys outside of Evil mode

;; Borrows heavily from https://github.com/wbolster/emacs-evil-colemak-basics

(defun +layout-remap-evil-keys-for-colemak-h ()
  (map! :mnv  "n"  #'evil-next-line
        :nv   "N"  #'evil-join
        :mnv  "gn" #'evil-next-visual-line
        :nv   "gN" #'evil-join-whitespace
        :mnv  "e"  #'evil-previous-line
        :mnv  "E"  #'evil-lookup
        :mnv  "ge" #'evil-previous-visual-line
        :mnvo "i"  #'evil-forward-char
        :mnv  "I"  #'evil-window-bottom
        :mnv  "zi" #'evil-scroll-column-right
        :mnv  "zI" #'evil-scroll-right
        :mnv  "j"  #'evil-forward-word-end
        :mnv  "J"  #'evil-forward-word-end
        :mnv  "gj" #'evil-backward-word-end
        :mnv  "gJ" #'evil-backward-word-end
        :mnv  "k"  #'evil-search-next
        :mnv  "K"  #'evil-search-previous
        :mnv  "gk" #'evil-next-match
        :mnv  "gK" #'evil-previous-match
        :n    "l"  #'evil-undo
        :v    "l"  #'evil-downcase
        :v    "L"  #'evil-upcase
        :nv   "gl" #'evil-downcase
        :nv   "gL" #'evil-upcase
        :n    "u"  #'evil-insert
        :vo   "u"  evil-inner-text-objects-map
        :n    "U"  #'evil-insert-line
        :v    "U"  #'evil-insert
        :n    "gu" #'evil-insert-resume
        :n    "gU" #'evil-insert-0-line
        (:map doom-leader-file-map
         "w"       #'save-buffer)
        (:map evil-window-map
         "n"       #'evil-window-down
         "N"       #'+evil/window-move-down
         "C-n"     #'evil-window-down
         "C-S-n"   #'evil-window-move-very-bottom
         "e"       #'evil-window-up
         "E"       #'+evil/window-move-up
         "C-e"     #'evil-window-up
         "C-S-e"   #'evil-window-move-very-top
         "i"       #'evil-window-right
         "I"       #'+evil/window-move-right
         "C-i"     #'evil-window-right
         "C-S-i"   #'evil-window-move-far-right
         "k"       #'evil-window-new)))

(defun +layout-remap-magit-evil-keys-for-colemak-h ()
  (map! :map magit-mode-map
        :nv "n"    #'evil-next-visual-line
        :nv "e"    #'evil-previous-visual-line))

(when (featurep! :editor evil)
  (+layout-remap-evil-keys-for-colemak-h)
  (when (featurep! :tools magit)
    (add-hook 'magit-mode-hook #'+layout-remap-magit-evil-keys-for-colemak-h)))
