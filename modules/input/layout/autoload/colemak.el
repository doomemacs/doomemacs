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
