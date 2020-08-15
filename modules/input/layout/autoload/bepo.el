;;; input/keymaps/autoload/bepo.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-bepo-rotate-ts-bare-keymap (keymaps)
  "Rotate [jk] with [ts] in KEYMAP."
  (dolist (keymap keymaps)
    (general-translate-key nil keymap
      "t" "j"
      "T" "J"
      "s" "k"
      "S" "K"
      "j" "t"
      "J" "T"
      "k" "s"
      "K" "S"
      "C-t" "C-j"
      "C-s" "C-k"
      "C-j" "C-t"
      "C-k" "C-s"
      "M-t" "M-j"
      "M-s" "M-k"
      "M-j" "M-t"
      "M-k" "M-s"
      "C-S-t" "C-S-j"
      "C-S-s" "C-S-k"
      "C-S-j" "C-S-t"
      "C-S-k" "C-S-s"
      "M-S-t" "M-S-j"
      "M-S-s" "M-S-k"
      "M-S-j" "M-S-t"
      "M-S-k" "M-S-s")))

;;;###autoload
(defun doom-bepo-rotate-é-quotes-bare-keymap (keymaps)
  "Rotate [w<>] with [é«»] in KEYMAP."
  (dolist (keymap keymaps)
    (general-translate-key nil keymap
      "é" "w"
      "É" "W"
      "«" "<"
      "»" ">"
      "C-é" "C-w"
      "C-«" "C-<"
      "C-»" "C->"
      "M-é" "M-w"
      "M-«" "M-<"
      "M-»" "M->"
      "C-S-é" "C-S-w"
      "C-S-«" "C-S-<"
      "C-S-»" "C-S->"
      "M-S-é" "M-S-w"
      "M-S-«" "M-S-<"
      "M-S-»" "M-S->")))

;;;###autoload
(defun doom-bepo-rotate-cr-bare-keymap (keymaps &optional style)
  "Rotate [hl] with [cr] in KEYMAP.

If STYLE is nil or 'ergodis, the old 'c' bindings will be mapped on 'l' and the old 'r' on 'h'.
Otherwise if STYLE is 'strict, the old 'c' bindings will be mapped on 'h' and the old 'r' on 'l'.
Undefined behaviour in other cases, for forward compatibility."
  (let ((style (or style 'ergodis)))
    (dolist (keymap keymaps)
      (progn
        (general-translate-key nil keymap
          "c" "h"
          "C" "H"
          "r" "l"
          "R" "L"
          "C-c" "C-h"
          "C-r" "C-l"
          "M-c" "M-h"
          "M-r" "M-l"
          "C-S-c" "C-S-h"
          "C-S-r" "C-S-l"
          "M-S-c" "M-S-h"
          "M-S-r" "M-S-l")
        (cond ((eq style 'ergodis)
               (general-translate-key nil keymap
                 "h" "r"
                 "H" "R"
                 "l" "c"
                 "L" "C"
                 "C-h" "C-r"
                 "C-l" "C-c"
                 "M-h" "M-r"
                 "M-l" "M-c"
                 "C-S-h" "C-S-r"
                 "C-S-l" "C-S-c"
                 "M-S-h" "M-S-r"
                 "M-S-l" "M-S-c"))
              (t
               (general-translate-key nil keymap
                 "h" "c"
                 "H" "C"
                 "l" "r"
                 "L" "R"
                 "C-h" "C-c"
                 "C-l" "C-r"
                 "M-h" "M-c"
                 "M-l" "M-r"
                 "C-S-h" "C-S-c"
                 "C-S-l" "C-S-r"
                 "M-S-h" "M-S-c"
                 "M-S-l" "M-S-r")))))))

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
  (general-translate-key nil '(normal motion visual)
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
         (general-translate-key nil '(normal motion visual)
           "h" "r"
           "H" "R"
           "l" "c"
           "L" "C"))
        (t
         (general-translate-key nil '(normal motion visual)
           "h" "c"
           "H" "C"
           "l" "r"
           "L" "R")))

  (general-translate-key nil '(insert)
    "C-c" "C-h"
    "C-C" "C-H"
    "C-t" "C-j"
    "C-T" "C-J"
    "C-s" "C-k"
    "C-S" "C-K"
    "C-r" "C-l"
    "C-R" "C-L"
    "C-j" "C-t"
    "C-J" "C-T"
    "C-k" "C-s"
    "C-K" "C-S")
  (cond ((eq cr-style 'ergodis)
         (general-translate-key nil '(insert)
           "C-h" "C-r"
           "C-H" "C-R"
           "C-l" "C-c"
           "C-L" "C-C"))
        (t
         (general-translate-key nil '(insert)
           "C-h" "C-c"
           "C-H" "C-C"
           "C-l" "C-r"
           "C-L" "C-R")))


  ;; <> as direct access
  (general-translate-key nil '(normal motion)
    "«" "<"
    "»" ">")

  ;; " è replaces ^0 to go at BOL
  (general-translate-key nil '(normal motion)
    "è" "^"
    "È" "0")

  ;; [W] -> [É]
  ;; [C-W] -> [W]
  (general-translate-key nil '(normal motion operator)
    "é" "w"
    "É" "W"
    "w" "C-w"
    "W" "C-w C-w"))

;;;###autoload
(defun doom-bepo-rotate-collection-keymaps-h-builder (cr-style)
  "Build a hook that remaps evil-collection customizations to be more natural
  with Bépo keyboard layout, according to CR-STYLE (see `doom-bepo-cr-rotation-style')."
  (let* ((cr-style (or cr-style 'ergodis))
         (doom-bepo-hook (lambda (_mode mode-keymaps &rest _rest)
                           (dolist (keymap mode-keymaps)
                             (general-translate-key '(normal motion visual) keymap
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
                                    (general-translate-key '(normal motion visual) keymap
                                      "h" "r"
                                      "H" "R"
                                      "l" "c"
                                      "L" "C"))
                                   (t
                                    (general-translate-key '(normal motion visual) keymap
                                      "h" "c"
                                      "H" "C"
                                      "l" "r"
                                      "L" "R")))


                             (general-translate-key '(insert) keymap
                               "C-c" "C-h"
                               "C-C" "C-H"
                               "C-t" "C-j"
                               "C-T" "C-J"
                               "C-s" "C-k"
                               "C-S" "C-K"
                               "C-r" "C-l"
                               "C-R" "C-L"
                               "C-j" "C-t"
                               "C-J" "C-T"
                               "C-k" "C-s"
                               "C-K" "C-S")
                             (cond ((eq cr-style 'ergodis)
                                    (general-translate-key '(insert) keymap
                                      "C-h" "C-r"
                                      "C-H" "C-R"
                                      "C-l" "C-c"
                                      "C-L" "C-C"))
                                   (t
                                    (general-translate-key '(insert) keymap
                                      "C-h" "C-c"
                                      "C-H" "C-C"
                                      "C-l" "C-r"
                                      "C-L" "C-R")))

                             ;; <> en direct
                             (general-translate-key '(normal motion visual) keymap
                               "«" "<"
                               "»" ">")

                             ;; è pour aller au début de ligne
                             (general-translate-key '(normal motion visual) keymap
                               "è" "^"
                               "È" "0")

                             ;; [W] -> [É]
                             ;; [C-W] -> [W]
                             (general-translate-key '(normal motion operator visual) keymap
                               "é" "w"
                               "É" "W"
                               "w" "C-w"
                               "W" "C-w C-w")))))
    doom-bepo-hook))
