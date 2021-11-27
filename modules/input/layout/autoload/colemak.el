;;; input/layout/autoload/colemak.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +layout-colemak-rotate-dh ()
  "Swap the H and M key-maps.
This is intended for use with the Colemak-DH layout."
  (map! :mnvo "m"  #'evil-backward-char
        :n    "zm" #'evil-scroll-column-left
        :n    "zM" #'evil-scroll-left
        :n    "h"  #'evil-set-marker
        :mnv  "zh" #'evil-close-folds
        (:map evil-window-map
         "m"       #'evil-window-left
         "M"       #'+evil/window-move-left
         "C-m"     #'evil-window-left
         "C-s-m"   #'evil-window-move-far-left
         "h h"     #'doom/window-maximize-buffer
         "h s"     #'doom/window-maximize-horizontally
         "h v"     #'doom/window-maximize-vertically)))

;;;###autoload
(defun +layout-colemak-rotate-t-f-j ()
  "Rotate the T, F and J keys.
T becomes find-char   (Qwerty F, same position)
F becomes end-of-word (Qwerty E, same position)
J becomes until-char  (Qwerty T, different position)"
  (map! :mnv "t"  #'evil-snipe-f
        :mnv "T"  #'evil-snipe-F
        :mnv "gt" #'find-file-at-point
        :mnv "gT" #'evil-find-file-at-point-with-line
        :mnv "f"  #'evil-forward-word-end
        :mnv "F"  #'evil-forward-WORD-end
        :mnv "gf" #'evil-backward-word-end
        :mnv "gF" #'evil-backward-WORD-end
        :mnv "j"  #'evil-snipe-t
        :mnv "J"  #'evil-snipe-T
        :mnv "gj" #'+workspace:switch-next
        :mnv "gJ" #'+workspace:switch-previous))
