;;; ui/pretty-code/config.el -*- lexical-binding: t; -*-

(defvar +pretty-code-symbols
  '(;; org
    :name          "»"
    :src_block     "»"
    :src_block_end "«"
    :quote         "“"
    :quote_end     "”"
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
    :map           "↦"
    ;; Types
    :null          "∅"
    :true          "𝕋"
    :false         "𝔽"
    :int           "ℤ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    ;; Flow
    :not           "￢"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"
    ;; Other
    :tuple         "⨂"
    :pipe          "" ;; FIXME: find a non-private char
    :dot           "•")
  "Options plist for `set-pretty-symbols!'.

This should not contain any symbols from the Unicode Private Area! There is no
universal way of getting the correct symbol as that area varies from font to
font.")

(defvar +pretty-code-enabled-modes t
  "List of major modes in which `prettify-symbols-mode' should be enabled.
If t, enable it everywhere. If the first element is 'not, enable it in any mode
besides what is listed.")

(defvar +pretty-code-symbols-alist '((t))
  "An alist containing a mapping of major modes to its value for
`prettify-symbols-alist'.")


;;
;;; Packages

;;;###package prettify-symbols
;; When you get to the right edge, it goes back to how it normally prints
(setq prettify-symbols-unprettify-at-point 'right-edge)


;;
;;; Bootstrap

(defun +pretty-code--correct-symbol-bounds (ligature-alist)
  "Prepend non-breaking spaces to a ligature.

This way `compose-region' (called by `prettify-symbols-mode') will use the
correct width of the symbols instead of the width measured by `char-width'."
  (let ((len (length (car ligature-alist)))
        (acc (list   (cdr ligature-alist))))
    (while (> len 1)
      (setq acc (cons #X00a0 (cons '(Br . Bl) acc))
            len (1- len)))
    (cons (car ligature-alist) acc)))

(defun +pretty-code-init-pretty-symbols-h ()
  "Enable `prettify-symbols-mode'.

If in fundamental-mode, or a mode derived from special, comint, eshell or term
modes, this function does nothing.

Otherwise it builds `prettify-code-symbols-alist' according to
`+pretty-code-symbols-alist' for the current major-mode."
  (unless (or (eq major-mode 'fundamental-mode)
              (eq (get major-mode 'mode-class) 'special)
              (derived-mode-p 'comint-mode 'eshell-mode 'term-mode))
    (when (or (eq +pretty-code-enabled-modes t)
              (if (eq (car +pretty-code-enabled-modes) 'not)
                  (not (memq major-mode (cdr +pretty-code-enabled-modes)))
                (memq major-mode +pretty-code-enabled-modes)))
      (setq prettify-symbols-alist
            (append (cdr (assq major-mode +pretty-code-symbols-alist))
                    (default-value 'prettify-symbols-alist)))
      (when prettify-symbols-mode
        (prettify-symbols-mode -1))
      (prettify-symbols-mode +1))))


(add-hook 'after-change-major-mode-hook #'+pretty-code-init-pretty-symbols-h)

(defvar +prog-ligatures-alist
  '((?! . "!\\(?:\\(==\\|[!=]\\)[!=]?\\)")
    (?# . "#\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
    (?$ . "$\\(?:\\(>\\)>?\\)")
    (?% . "%\\(?:\\(%\\)%?\\)")
    (?& . "&\\(?:\\(&\\)&?\\)")
    (?* . "*\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
    ;; (?* . "*\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
    (?+ . "+\\(?:\\([>]\\)>?\\)")
    ;; (?+ . "+\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
    (?- . "-\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
    ;; (?. . "\\.\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
    (?. . "\\.\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
    (?/ . "/\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
    ;; (?/ . "/\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
    (?0 . "0\\(?:\\(x[a-fA-F0-9]\\).?\\)")
    (?: . ":\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
    (59 . ";\\(?:\\(;\\);?\\)") ;; 59 is ;
    (?< . "<\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
    (?= . "=\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
    (?> . ">\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
    (?? . "?\\(?:\\([.:=?]\\)[.:=?]?\\)")
    (91 . "\\[\\(?:\\(|\\)[]|]?\\)") ;; 91 is [
    ;; (?\ . "\\\\\\(?:\\([\\n]\\)[\\]?\\)")
    (?^ . "^\\(?:\\(=\\)=?\\)")
    (?_ . "_\\(?:\\(|_\\|[_]\\)_?\\)")
    (?w . "w\\(?:\\(ww\\)w?\\)")
    (?{ . "{\\(?:\\(|\\)[|}]?\\)")
    (?| . "|\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
    (?~ . "~\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))
  "An alist containing all the ligatures used when in a `+prog-ligatures-modes' mode.

The car is the character ASCII number, cdr is a regex which will call `font-shape-gstring'
when matched.

Because of the underlying code in :ui pretty-code module, the regex should match a string
starting with the character contained in car."
  )

;; Defaults to not org-mode because org-bullets might be incompatible
;; with the ?*-based replacements in the default value of +prg-ligatures-alist
(defvar +prog-ligatures-modes '(not org-mode)
  "List of major modes in which ligatures should be enabled.

If t, enable it everywhere.

If the first element is 'not, enable it in any mode besides what is listed.

If nil, fallback to the prettify-symbols based replacement (add +font features to pretty-code)."
  )

(defun +pretty-code-init-ligatures-h ()
  "Enable ligatures.

If in fundamental-mode, or a mode derived from special, comint, eshell or term
modes, this function does nothing.

Otherwise it sets the buffer-local composition table to a composition table enhanced with
`+prog-ligatures-alist' ligatures regexes."
  (unless (or (eq major-mode 'fundamental-mode)
              (eq (get major-mode 'mode-class) 'special)
              (derived-mode-p 'comint-mode 'eshell-mode 'term-mode))
    (when (or (eq +prog-ligatures-modes t)
              (if (eq (car +prog-ligatures-modes) 'not)
                  (not (memq major-mode (cdr +prog-ligatures-modes)))
                (memq major-mode +prog-ligatures-modes)))
      (setq-local composition-function-table composition-ligature-table))))

(add-hook 'after-change-major-mode-hook #'+pretty-code-init-ligatures-h)

(use-package! composite
  ;; Starting from emacs "28" because this code breaks without fe903c5
  :when (and (version<= "28.0" emacs-version) (string-match-p "HARFBUZZ" system-configuration-features))
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :config
  (dolist (char-regexp +prog-ligatures-alist)
    (set-char-table-range composition-ligature-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring])))
  (set-char-table-parent composition-ligature-table composition-function-table))

;; The emacs-mac build of Emacs appear to have built-in support for ligatures,
;; so use that instead if this module is enabled.
(cond ((and IS-MAC (fboundp 'mac-auto-operator-composition-mode))
       (mac-auto-operator-composition-mode))
      ;; Harfbuzz builds do not need font-specific ligature support
      ;; if they brought in the fe903c5 commit
      ((and (version<= "28.0" emacs-version)
            (string-match-p "HARFBUZZ" system-configuration-features)
            (not (null +prog-ligatures-modes)))
       nil)
      ;; Font-specific ligature support
      ((featurep! +fira)
       (load! "+fira"))
      ((featurep! +iosevka)
       (load! "+iosevka"))
      ((featurep! +hasklig)
       (load! "+hasklig"))
      ((featurep! +pragmata-pro)
       (load! "+pragmata-pro")))
