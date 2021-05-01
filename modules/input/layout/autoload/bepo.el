;;; input/layout/autoload/bepo.el -*- lexical-binding: t; -*-
;;;###if (featurep! +bepo)

;;;###autoload
(defun +layout-bepo-rotate-ts-bare-keymap (keymaps)
  "Rotate [jk] with [ts] in KEYMAP."
  (evil-collection-translate-key nil keymaps
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
    (kbd "M-S-k") (kbd "M-S-s")))

;;;###autoload
(defun +layout-bepo-rotate-é-quotes-bare-keymap (keymaps)
  "Rotate [w<>] with [é«»] in KEYMAP."
  (evil-collection-translate-key nil keymaps
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
    (kbd "M-S-»") (kbd "M-S->")))

;;;###autoload
(defun +layout-bepo-rotate-cr-bare-keymap (keymaps &optional cr-style)
  "Rotate [hl] with [cr] in KEYMAP.

If CR-STYLE is nil or 'ergodis, the old 'c' bindings will be mapped on 'l' and
the old 'r' on 'h'.

Otherwise if CR-STYLE is 'strict, the old 'c' bindings will be mapped on 'h' and
the old 'r' on 'l'.

Undefined behaviour in other cases, for forward compatibility."
  (let ((cr-style (or cr-style +layout-bepo-cr-rotation-style)))
    (evil-collection-translate-key nil keymaps
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
    (if (eq cr-style 'ergodis)
        (evil-collection-translate-key nil keymaps
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
          (kbd "M-S-l") (kbd "M-S-c"))
      (evil-collection-translate-key nil keymaps
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
        (kbd "M-S-l") (kbd "M-S-r")))))

;;;###autoload
(defun +layout-bepo-rotate-bare-keymap (keymaps &optional cr-style)
  "Rotate [hjklw<>] with [ctsré«»] in KEYMAP.
See `+layout-bepo-cr-rotation-style' for the meaning of CR-STYLE"
  (+layout-bepo-rotate-cr-bare-keymap keymaps cr-style)
  (+layout-bepo-rotate-ts-bare-keymap keymaps)
  (+layout-bepo-rotate-é-quotes-bare-keymap keymaps))

;;;###autoload
(defun +layout-bepo-rotate-evil-keymap (&optional cr-style)
  "Remap evil-{normal,operator,motion,...}-state-map to be more natural with
Bépo keyboard layout.

See `+layout-bepo-cr-rotation-style' for the meaning of CR-STYLE."
  (let ((cr-style (or cr-style +layout-bepo-cr-rotation-style)))
    (evil-collection-translate-key nil
      '(evil-normal-state-map evil-motion-state-map evil-visual-state-map evil-operator-state-map)
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
    (if (eq cr-style 'ergodis)
        (evil-collection-translate-key nil
          '(evil-normal-state-map evil-motion-state-map evil-visual-state-map evil-operator-state-map)
          "h" "r"
          "H" "R"
          "l" "c"
          "L" "C")
      (evil-collection-translate-key nil
        '(evil-normal-state-map evil-motion-state-map evil-visual-state-map evil-operator-state-map)
        "h" "c"
        "H" "C"
        "l" "r"
        "L" "R"))
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
    (if (eq cr-style 'ergodis)
        (evil-collection-translate-key nil '(evil-insert-state-map)
          (kbd "C-h") (kbd "C-r")
          (kbd "C-H") (kbd "C-R")
          (kbd "C-l") (kbd "C-c")
          (kbd "C-L") (kbd "C-C"))
      (evil-collection-translate-key nil '(evil-insert-state-map)
        (kbd "C-h") (kbd "C-c")
        (kbd "C-H") (kbd "C-C")
        (kbd "C-l") (kbd "C-r")
        (kbd "C-L") (kbd "C-R")))

    ;; <> as direct access
    (evil-collection-translate-key nil
      '(evil-normal-state-map evil-motion-state-map evil-operator-state-map)
      "«" "<"
      "»" ">")

    ;; " è replaces ^0 to go at BOL
    (evil-collection-translate-key nil
      '(evil-normal-state-map evil-motion-state-map evil-operator-state-map)
      "è" "^"
      "È" "0")

    ;; [W] -> [É]
    ;; [C-W] -> [W]
    (evil-collection-translate-key nil
      '(evil-normal-state-map evil-motion-state-map evil-operator-state-map)
      "é" "w"
      "É" "W"
      "w" (kbd "C-w")
      "W" (kbd "C-w C-w"))))

;;;###autoload
(defun +layout-bepo-rotate-keymaps (keymaps &optional cr-style)
  "Remap evil-collection keybinds in KEYMAPS for Bépo keyboard keyboard layouts.

Remappings are done according to CR-STYLE (see
`+layout-bepo-cr-rotation-style')."
  (let ((cr-style (or cr-style +layout-bepo-cr-rotation-style)))
    (evil-collection-translate-key '(normal motion visual operator) keymaps
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
      "K" "S"
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
      (kbd "C-K") (kbd "C-S")
      (kbd "M-c") (kbd "M-h")
      (kbd "M-C") (kbd "M-H")
      (kbd "M-t") (kbd "M-j")
      (kbd "M-T") (kbd "M-J")
      (kbd "M-s") (kbd "M-k")
      (kbd "M-S") (kbd "M-K")
      (kbd "M-r") (kbd "M-l")
      (kbd "M-R") (kbd "M-L")
      (kbd "M-j") (kbd "M-t")
      (kbd "M-J") (kbd "M-T")
      (kbd "M-k") (kbd "M-s")
      (kbd "M-K") (kbd "M-S"))
    (if (eq cr-style 'ergodis)
        (evil-collection-translate-key '(normal motion visual operator) keymaps
          "h" "r"
          "H" "R"
          "l" "c"
          "L" "C"
          (kbd "C-h") (kbd "C-r")
          (kbd "C-H") (kbd "C-R")
          (kbd "C-l") (kbd "C-c")
          (kbd "C-L") (kbd "C-C")
          (kbd "M-h") (kbd "M-r")
          (kbd "M-H") (kbd "M-R")
          (kbd "M-l") (kbd "M-c")
          (kbd "M-L") (kbd "M-C"))
      (evil-collection-translate-key '(normal motion visual operator) keymaps
        "h" "c"
        "H" "C"
        "l" "r"
        "L" "R"
        (kbd "C-h") (kbd "C-c")
        (kbd "C-H") (kbd "C-C")
        (kbd "C-l") (kbd "C-r")
        (kbd "C-L") (kbd "C-R")
        (kbd "M-h") (kbd "M-c")
        (kbd "M-H") (kbd "M-C")
        (kbd "M-l") (kbd "M-r")
        (kbd "M-L") (kbd "M-R")))

    (evil-collection-translate-key '(insert) keymaps
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
      (kbd "C-K") (kbd "C-S")
      (kbd "M-c") (kbd "M-h")
      (kbd "M-C") (kbd "M-H")
      (kbd "M-t") (kbd "M-j")
      (kbd "M-T") (kbd "M-J")
      (kbd "M-s") (kbd "M-k")
      (kbd "M-S") (kbd "M-K")
      (kbd "M-r") (kbd "M-l")
      (kbd "M-R") (kbd "M-L")
      (kbd "M-j") (kbd "M-t")
      (kbd "M-J") (kbd "M-T")
      (kbd "M-k") (kbd "M-s")
      (kbd "M-K") (kbd "M-S"))
    (if (eq cr-style 'ergodis)
        (evil-collection-translate-key '(insert) keymaps
          (kbd "C-h") (kbd "C-r")
          (kbd "C-H") (kbd "C-R")
          (kbd "C-l") (kbd "C-c")
          (kbd "C-L") (kbd "C-C")
          (kbd "M-h") (kbd "M-r")
          (kbd "M-H") (kbd "M-R")
          (kbd "M-l") (kbd "M-c")
          (kbd "M-L") (kbd "M-C"))
      (evil-collection-translate-key '(insert) keymaps
        (kbd "C-h") (kbd "C-c")
        (kbd "C-H") (kbd "C-C")
        (kbd "C-l") (kbd "C-r")
        (kbd "C-L") (kbd "C-R")
        (kbd "M-h") (kbd "M-c")
        (kbd "M-H") (kbd "M-C")
        (kbd "M-l") (kbd "M-r")
        (kbd "M-L") (kbd "M-R")))

    ;; <> en direct
    (evil-collection-translate-key '(normal motion visual operator) keymaps
      "«" "<"
      "»" ">")

    ;; è pour aller au début de ligne
    (evil-collection-translate-key '(normal motion visual operator) keymaps
      "è" "^"
      "È" "0")

    ;; [W] -> [É]
    ;; [C-W] -> [W]
    (evil-collection-translate-key '(normal motion operator visual) keymaps
      "é" "w"
      "É" "W"
      "w" (kbd "C-w")
      "W" (kbd "C-w C-w"))))
