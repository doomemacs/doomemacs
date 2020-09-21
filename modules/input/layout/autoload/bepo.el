;;; input/keymaps/autoload/bepo.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-bepo-rotate-ts-bare-keymap (keymaps)
  "Rotate [jk] with [ts] in KEYMAP."
  (dolist (keymap keymaps)
    (evil-collection-translate-key nil keymap
      "t" "j"
      "T" "J"
      "s" "k"
      "S" "K"
      "j" "t"
      "J" "T"
      "k" "s"
      "K" "S"
      (kbd "C-t") (kbd "C-j")
      (kbd "C-s") (kbd "C-k")
      (kbd "C-j") (kbd "C-t")
      (kbd "C-k") (kbd "C-s")
      (kbd "M-t") (kbd "M-j")
      (kbd "M-s") (kbd "M-k")
      (kbd "M-j") (kbd "M-t")
      (kbd "M-k") (kbd "M-s")
      (kbd "C-S-t") (kbd "C-S-j")
      (kbd "C-S-s") (kbd "C-S-k")
      (kbd "C-S-j") (kbd "C-S-t")
      (kbd "C-S-k") (kbd "C-S-s")
      (kbd "M-S-t") (kbd "M-S-j")
      (kbd "M-S-s") (kbd "M-S-k")
      (kbd "M-S-j") (kbd "M-S-t")
      (kbd "M-S-k") (kbd "M-S-s"))))

;;;###autoload
(defun doom-bepo-rotate-é-quotes-bare-keymap (keymaps)
  "Rotate [w<>] with [é«»] in KEYMAP."
  (dolist (keymap keymaps)
    (evil-collection-translate-key nil keymap
      "é" "w"
      "É" "W"
      "«" "<"
      "»" ">"
      (kbd "C-é") (kbd "C-w")
      (kbd "C-«") (kbd "C-<")
      (kbd "C-»") (kbd "C->")
      (kbd "M-é") (kbd "M-w")
      (kbd "M-«") (kbd "M-<")
      (kbd "M-»") (kbd "M->")
      (kbd "C-S-é") (kbd "C-S-w")
      (kbd "C-S-«") (kbd "C-S-<")
      (kbd "C-S-»") (kbd "C-S->")
      (kbd "M-S-é") (kbd "M-S-w")
      (kbd "M-S-«") (kbd "M-S-<")
      (kbd "M-S-»") (kbd "M-S->"))))

;;;###autoload
(defun doom-bepo-rotate-cr-bare-keymap (keymaps &optional style)
  "Rotate [hl] with [cr] in KEYMAP.

If STYLE is nil or 'ergodis, the old 'c' bindings will be mapped on 'l' and the old 'r' on 'h'.
Otherwise if STYLE is 'strict, the old 'c' bindings will be mapped on 'h' and the old 'r' on 'l'.
Undefined behaviour in other cases, for forward compatibility."
  (let ((style (or style 'ergodis)))
    (dolist (keymap keymaps)
      (progn
        (evil-collection-translate-key nil keymap
          "c" "h"
          "C" "H"
          "r" "l"
          "R" "L"
          (kbd "C-c") (kbd "C-h")
          (kbd "C-r") (kbd "C-l")
          (kbd "M-c") (kbd "M-h")
          (kbd "M-r") (kbd "M-l")
          (kbd "C-S-c") (kbd "C-S-h")
          (kbd "C-S-r") (kbd "C-S-l")
          (kbd "M-S-c") (kbd "M-S-h")
          (kbd "M-S-r") (kbd "M-S-l"))
        (cond ((eq style 'ergodis)
               (evil-collection-translate-key nil keymap
                 "h" "r"
                 "H" "R"
                 "l" "c"
                 "L" "C"
                 (kbd "C-h") (kbd "C-r")
                 (kbd "C-l") (kbd "C-c")
                 (kbd "M-h") (kbd "M-r")
                 (kbd "M-l") (kbd "M-c")
                 (kbd "C-S-h") (kbd "C-S-r")
                 (kbd "C-S-l") (kbd "C-S-c")
                 (kbd "M-S-h") (kbd "M-S-r")
                 (kbd "M-S-l") (kbd "M-S-c")))
              (t
               (evil-collection-translate-key nil keymap
                 "h" "c"
                 "H" "C"
                 "l" "r"
                 "L" "R"
                 (kbd "C-h") (kbd "C-c")
                 (kbd "C-l") (kbd "C-r")
                 (kbd "M-h") (kbd "M-c")
                 (kbd "M-l") (kbd "M-r")
                 (kbd "C-S-h") (kbd "C-S-c")
                 (kbd "C-S-l") (kbd "C-S-r")
                 (kbd "M-S-h") (kbd "M-S-c")
                 (kbd "M-S-l") (kbd "M-S-r"))))))))

;;;###autoload
(defun doom-bepo-rotate-bare-keymap (keymaps &optional cr-style)
  "Rotate [hjklw<>] with [ctsré«»] in KEYMAP.
See `doom-bepo-cr-rotation-style' for the meaning of CR-STYLE"
  (doom-bepo-rotate-cr-bare-keymap keymaps cr-style)
  (doom-bepo-rotate-ts-bare-keymap keymaps)
  (doom-bepo-rotate-é-quotes-bare-keymap keymaps))

;;;###autoload
(defun doom-bepo-rotate-evil-keymap (&optional cr-style)
  "Remap evil-{normal,operator,motion,...}-state-map
  to be more natural with Bépo keyboard layout.
See `doom-bepo-cr-rotation-style' for the meaning of CR-STYLE."
  (evil-collection-translate-key nil '(evil-normal-state-map evil-motion-state-map evil-visual-state-map)
    "c" "h"
    "C" "H"
    "t" "j"
    "T" "J"
    "s" "k"
    "S" "K"
    "r" "l"
    "R" "L"
    "j" "t"
    "J" "T"
    "k" "s"
    "K" "S")
  (cond ((eq cr-style 'ergodis)
         (evil-collection-translate-key nil '(evil-normal-state-map evil-motion-state-map evil-visual-state-map)
           "h" "r"
           "H" "R"
           "l" "c"
           "L" "C"))
        (t
         (evil-collection-translate-key nil '(evil-normal-state-map evil-motion-state-map evil-visual-state-map)
           "h" "c"
           "H" "C"
           "l" "r"
           "L" "R")))

  (evil-collection-translate-key nil '(evil-insert-state-map)
    (kbd "C-c") (kbd "C-h")
    (kbd "C-C") (kbd "C-H")
    (kbd "C-t") (kbd "C-j")
    (kbd "C-T") (kbd "C-J")
    (kbd "C-s") (kbd "C-k")
    (kbd "C-S") (kbd "C-K")
    (kbd "C-r") (kbd "C-l")
    (kbd "C-R") (kbd "C-L")
    (kbd "C-j") (kbd "C-t")
    (kbd "C-J") (kbd "C-T")
    (kbd "C-k") (kbd "C-s")
    (kbd "C-K") (kbd "C-S"))
  (cond ((eq cr-style 'ergodis)
         (evil-collection-translate-key nil '(evil-insert-state-map)
           (kbd "C-h") (kbd "C-r")
           (kbd "C-H") (kbd "C-R")
           (kbd "C-l") (kbd "C-c")
           (kbd "C-L") (kbd "C-C")))
        (t
         (evil-collection-translate-key nil '(evil-insert-state-map)
           (kbd "C-h") (kbd "C-c")
           (kbd "C-H") (kbd "C-C")
           (kbd "C-l") (kbd "C-r")
           (kbd "C-L") (kbd "C-R"))))


  ;; <> as direct access
  (evil-collection-translate-key nil '(evil-normal-state-map evil-motion-state-map)
    "«" "<"
    "»" ">")

  ;; " è replaces ^0 to go at BOL
  (evil-collection-translate-key nil '(evil-normal-state-map evil-motion-state-map)
    "è" "^"
    "È" "0")

  ;; [W] -> [É]
  ;; [C-W] -> [W]
  (evil-collection-translate-key nil '(evil-normal-state-map evil-motion-state-map evil-operator-state-map)
    "é" "w"
    "É" "W"
    "w" (kbd "C-w")
    "W" (kbd "C-w C-w")))

;;;###autoload
(defun doom-bepo-rotate-collection-keymaps-h-builder (cr-style)
  "Build a hook that remaps evil-collection customizations to be more natural
  with Bépo keyboard layout, according to CR-STYLE (see `doom-bepo-cr-rotation-style')."
  (let* ((cr-style (or cr-style 'ergodis))
         (doom-bepo-hook (lambda (_mode mode-keymaps &rest _rest)
                           (dolist (keymap mode-keymaps)
                             (evil-collection-translate-key '(normal motion visual) keymap
                               "c" "h"
                               "C" "H"
                               "t" "j"
                               "T" "J"
                               "s" "k"
                               "S" "K"
                               "r" "l"
                               "R" "L"
                               "j" "t"
                               "J" "T"
                               "k" "s"
                               "K" "S")
                             (cond ((eq cr-style 'ergodis)
                                    (evil-collection-translate-key '(normal motion visual) keymap
                                      "h" "r"
                                      "H" "R"
                                      "l" "c"
                                      "L" "C"))
                                   (t
                                    (evil-collection-translate-key '(normal motion visual) keymap
                                      "h" "c"
                                      "H" "C"
                                      "l" "r"
                                      "L" "R")))


                             (evil-collection-translate-key '(insert) keymap
                               (kbd "C-c") (kbd "C-h")
                               (kbd "C-C") (kbd "C-H")
                               (kbd "C-t") (kbd "C-j")
                               (kbd "C-T") (kbd "C-J")
                               (kbd "C-s") (kbd "C-k")
                               (kbd "C-S") (kbd "C-K")
                               (kbd "C-r") (kbd "C-l")
                               (kbd "C-R") (kbd "C-L")
                               (kbd "C-j") (kbd "C-t")
                               (kbd "C-J") (kbd "C-T")
                               (kbd "C-k") (kbd "C-s")
                               (kbd "C-K") (kbd "C-S"))
                             (cond ((eq cr-style 'ergodis)
                                    (evil-collection-translate-key '(insert) keymap
                                      (kbd "C-h") (kbd "C-r")
                                      (kbd "C-H") (kbd "C-R")
                                      (kbd "C-l") (kbd "C-c")
                                      (kbd "C-L") (kbd "C-C")))
                                   (t
                                    (evil-collection-translate-key '(insert) keymap
                                      (kbd "C-h") (kbd "C-c")
                                      (kbd "C-H") (kbd "C-C")
                                      (kbd "C-l") (kbd "C-r")
                                      (kbd "C-L") (kbd "C-R"))))

                             ;; <> en direct
                             (evil-collection-translate-key '(normal motion visual) keymap
                               "«" "<"
                               "»" ">")

                             ;; è pour aller au début de ligne
                             (evil-collection-translate-key '(normal motion visual) keymap
                               "è" "^"
                               "È" "0")

                             ;; [W] -> [É]
                             ;; [C-W] -> [W]
                             (evil-collection-translate-key '(normal motion operator visual) keymap
                               "é" "w"
                               "É" "W"
                               "w" (kbd "C-w")
                               "W" (kbd "C-w C-w"))))))
    doom-bepo-hook))
