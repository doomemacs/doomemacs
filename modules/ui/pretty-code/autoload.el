;;; ui/pretty-code/autoload.el -*- lexical-binding: t; -*-

;; When you get to the right edge, it goes back to how it normally prints
(setq prettify-symbols-unprettify-at-point 'right-edge)

;;;###autoload
(defvar +pretty-code-iosevka-ligatures-enabled-by-default nil
  "If non-nil, iosevka ligeratures are enabled by default in modes
`set-pretty-symbols!' has been used on.

This requires the iosevka font!

Use the :iosevka property to enable (or disable) it regardless.")

;;;###autoload
(defvar +pretty-code-enabled-modes t
  "List of major modes in which `prettify-symbols-mode' should not be enabled.
If t, enable it everywhere. If the first element is 'not, enable it in any mode
besides what is listed.")

;;;###autoload
(defvar +pretty-code-symbols
  '(;; org
    :name          "»"
    :src_block     "»"
    :src_block_end " "
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
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
    :pipe          "")
  "Options plist for `pretty-code-get-pairs'.")

(defvar +pretty-code--iosevka-ligeratures-enabled nil)
(defun +pretty-code-setup-iosevka-ligatures ()
  (unless +pretty-code--iosevka-ligeratures-enabled
    (set-fontset-font t '(#xe100 . #xe16f) "Iosevka"))
  (setq prettify-symbols-alist
        (append prettify-symbols-alist
                '(;; Double-ended hyphen arrows
                  ("<->" . #Xe100)
                  ("<-->" . #Xe101)
                  ("<--->" . #Xe102)
                  ("<---->" . #Xe103)
                  ("<----->" . #Xe104)
                  ;; Double-ended equals arrows
                  ("<=>" . #Xe105)
                  ("<==>" . #Xe106)
                  ("<===>" . #Xe107)
                  ("<====>" . #Xe108)
                  ("<=====>" . #Xe109)
                  ;; Double-ended asterisk operators
                  ("<**>" . #Xe10a)
                  ("<***>" . #Xe10b)
                  ("<****>" . #Xe10c)
                  ("<*****>" . #Xe10d)
                  ;; HTML comments
                  ("<!--" . #Xe10e)
                  ("<!---" . #Xe10f)
                  ;; Three-char ops with discards
                  ("<$" . #Xe110)
                  ("<$>" . #Xe111)
                  ("$>" . #Xe112)
                  ("<." . #Xe113)
                  ("<.>" . #Xe114)
                  (".>" . #Xe115)
                  ("<*" . #Xe116)
                  ("<*>" . #Xe117)
                  ("*>" . #Xe118)
                  ("<\\" . #Xe119)
                  ("<\\>" . #Xe11a)
                  ("\\>" . #Xe11b)
                  ("</" . #Xe11c)
                  ("</>" . #Xe11d)
                  ("/>" . #Xe11e)
                  ("<\"" . #Xe11f)
                  ("<\">" . #Xe120)
                  ("\">" . #Xe121)
                  ("<'" . #Xe122)
                  ("<'>" . #Xe123)
                  ("'>" . #Xe124)
                  ("<^" . #Xe125)
                  ("<^>" . #Xe126)
                  ("^>" . #Xe127)
                  ("<&" . #Xe128)
                  ("<&>" . #Xe129)
                  ("&>" . #Xe12a)
                  ("<%" . #Xe12b)
                  ("<%>" . #Xe12c)
                  ("%>" . #Xe12d)
                  ("<@" . #Xe12e)
                  ("<@>" . #Xe12f)
                  ("@>" . #Xe130)
                  ("<#" . #Xe131)
                  ("<#>" . #Xe132)
                  ("#>" . #Xe133)
                  ("<+" . #Xe134)
                  ("<+>" . #Xe135)
                  ("+>" . #Xe136)
                  ("<-" . #Xe137)
                  ("<->" . #Xe138)
                  ("->" . #Xe139)
                  ("<!" . #Xe13a)
                  ("<!>" . #Xe13b)
                  ("!>" . #Xe13c)
                  ("<?" . #Xe13d)
                  ("<?>" . #Xe13e)
                  ("?>" . #Xe13f)
                  ("<|" . #Xe140)
                  ("<|>" . #Xe141)
                  ("|>" . #Xe142)
                  ("<:" . #Xe143)
                  ("<:>" . #Xe144)
                  (":>" . #Xe145)
                  ;; Colons
                  ("::" . #Xe146)
                  (":::" . #Xe147)
                  ("::::" . #Xe148)
                  ;; Arrow-like operators
                  ("->" . #Xe149)
                  ("->-" . #Xe14a)
                  ("->--" . #Xe14b)
                  ("->>" . #Xe14c)
                  ("->>-" . #Xe14d)
                  ("->>--" . #Xe14e)
                  ("->>>" . #Xe14f)
                  ("->>>-" . #Xe150)
                  ("->>>--" . #Xe151)
                  ("-->" . #Xe152)
                  ("-->-" . #Xe153)
                  ("-->--" . #Xe154)
                  ("-->>" . #Xe155)
                  ("-->>-" . #Xe156)
                  ("-->>--" . #Xe157)
                  ("-->>>" . #Xe158)
                  ("-->>>-" . #Xe159)
                  ("-->>>--" . #Xe15a)
                  (">-" . #Xe15b)
                  (">--" . #Xe15c)
                  (">>-" . #Xe15d)
                  (">>--" . #Xe15e)
                  (">>>-" . #Xe15f)
                  (">>>--" . #Xe160)
                  ("=>" . #Xe161)
                  ("=>=" . #Xe162)
                  ("=>==" . #Xe163)
                  ("=>>" . #Xe164)
                  ("=>>=" . #Xe165)
                  ("=>>==" . #Xe166)
                  ("=>>>" . #Xe167)
                  ("=>>>=" . #Xe168)
                  ("=>>>==" . #Xe169)
                  ("==>" . #Xe16a)
                  ("==>=" . #Xe16b)
                  ("==>==" . #Xe16c)
                  ("==>>" . #Xe16d)
                  ("==>>=" . #Xe16e)
                  ("==>>==" . #Xe16f)
                  ("==>>>" . #Xe170)
                  ("==>>>=" . #Xe171)
                  ("==>>>==" . #Xe172)
                  (">=" . #Xe173)
                  (">==" . #Xe174)
                  (">>=" . #Xe175)
                  (">>==" . #Xe176)
                  (">>>=" . #Xe177)
                  (">>>==" . #Xe178)
                  ("<-" . #Xe179)
                  ("-<-" . #Xe17a)
                  ("--<-" . #Xe17b)
                  ("<<-" . #Xe17c)
                  ("-<<-" . #Xe17d)
                  ("--<<-" . #Xe17e)
                  ("<<<-" . #Xe17f)
                  ("-<<<-" . #Xe180)
                  ("--<<<-" . #Xe181)
                  ("<--" . #Xe182)
                  ("-<--" . #Xe183)
                  ("--<--" . #Xe184)
                  ("<<--" . #Xe185)
                  ("-<<--" . #Xe186)
                  ("--<<--" . #Xe187)
                  ("<<<--" . #Xe188)
                  ("-<<<--" . #Xe189)
                  ("--<<<--" . #Xe18a)
                  ("-<" . #Xe18b)
                  ("--<" . #Xe18c)
                  ("-<<" . #Xe18d)
                  ("--<<" . #Xe18e)
                  ("-<<<" . #Xe18f)
                  ("--<<<" . #Xe190)
                  ("<=" . #Xe191)
                  ("=<=" . #Xe192)
                  ("==<=" . #Xe193)
                  ("<<=" . #Xe194)
                  ("=<<=" . #Xe195)
                  ("==<<=" . #Xe196)
                  ("<<<=" . #Xe197)
                  ("=<<<=" . #Xe198)
                  ("==<<<=" . #Xe199)
                  ("<==" . #Xe19a)
                  ("=<==" . #Xe19b)
                  ("==<==" . #Xe19c)
                  ("<<==" . #Xe19d)
                  ("=<<==" . #Xe19e)
                  ("==<<==" . #Xe19f)
                  ("<<<==" . #Xe1a0)
                  ("=<<<==" . #Xe1a1)
                  ("==<<<==" . #Xe1a2)
                  ("=<" . #Xe1a3)
                  ("==<" . #Xe1a4)
                  ("=<<" . #Xe1a5)
                  ("==<<" . #Xe1a6)
                  ("=<<<" . #Xe1a7)
                  ("==<<<" . #Xe1a8)
                  ;; Monadic operators
                  (">=>" . #Xe1a9)
                  (">->" . #Xe1aa)
                  (">-->" . #Xe1ab)
                  (">==>" . #Xe1ac)
                  ("<=<" . #Xe1ad)
                  ("<-<" . #Xe1ae)
                  ("<--<" . #Xe1af)
                  ("<==<" . #Xe1b0)
                  ;; Composition operators
                  (">>" . #Xe1b1)
                  (">>>" . #Xe1b2)
                  ("<<" . #Xe1b3)
                  ("<<<" . #Xe1b4)
                  ;; Lens operators
                  (":+" . #Xe1b5)
                  (":-" . #Xe1b6)
                  (":=" . #Xe1b7)
                  ("+:" . #Xe1b8)
                  ("-:" . #Xe1b9)
                  ("=:" . #Xe1ba)
                  ("=^" . #Xe1bb)
                  ("=+" . #Xe1bc)
                  ("=-" . #Xe1bd)
                  ("=*" . #Xe1be)
                  ("=/" . #Xe1bf)
                  ("=%" . #Xe1c0)
                  ("^=" . #Xe1c1)
                  ("+=" . #Xe1c2)
                  ("-=" . #Xe1c3)
                  ("*=" . #Xe1c4)
                  ("/=" . #Xe1c5)
                  ("%=" . #Xe1c6)
                  ;; Logical
                  ("/\\" . #Xe1c7)
                  ("\\/" . #Xe1c8)
                  ;; Semigroup/monoid operators
                  ("<>" . #Xe1c9)
                  ("<+" . #Xe1ca)
                  ("<+>" . #Xe1cb)
                  ("+>" . #Xe1cc)))))

(defun +pretty-code--icon-to-char (l)
  "Borrowed from `prettify-utils'."
  (let ((glue '(Br . Bl))
        (head (car l))
        (tail (cdr l)))
    (cond ((not (consp l)) '())
          ((not (consp tail))  (list head))
          ((cons head (cons glue (+pretty-code--icon-to-char tail)))))))

;;;###autodef
(defun set-pretty-symbols! (modes &rest plist)
  "Associates string patterns with icons in certain major-modes.

  MODES is a major mode symbol or a list of them.
  PLIST is a property list whose keys must match keys in `+pretty-code-symbols',
and whose values are strings representing the text to be replaced with that
symbol. If the car of PLIST is nil, then unset any pretty symbols previously
defined for MODES.

The following properties are special:

  :iosevka BOOL
    Enables (or disables) iosevka ligeratures for MODES. See the definition of
    `+pretty-code-setup-iosevka-ligatures' for more info.
    `+pretty-code-iosevka-ligatures-enabled-by-default' determines the default
    setting.
  :alist ALIST
    Appends ALIST to `prettify-symbols-alist' literally, without mapping text to
    `+pretty-code-symbols'.
  :merge BOOL
    If non-nil, merge with previously defined `prettify-symbols-alist',
    otherwise overwrite it.

For example, the rule for emacs-lisp-mode is very simple:

  (set-pretty-symbols! 'emacs-lisp-mode
    :lambda \"lambda\")

This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
assicated with :lambda in `+pretty-code-symbols'.

Pretty symbols can be unset for emacs-lisp-mode with:

  (set-pretty-symbols! 'emacs-lisp-mode nil)"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (let ((hook (intern (format "%s-hook" mode)))
          (fn   (intern (format "+pretty-code|init-%s" mode))))
      (cond ((null (car-safe plist))
             (remove-hook hook fn)
             (unintern fn nil))
            ((or (eq +pretty-code-enabled-modes 't)
                 (if (eq (car +pretty-code-enabled-modes) 'not)
                     (not (memq mode (cdr +pretty-code-enabled-modes)))
                   (memq mode +pretty-code-enabled-modes)))
             (fset fn
                   (lambda ()
                     (when (eq major-mode mode)
                       (unless (cadr (plist-member plist :merge))
                         (setq prettify-symbols-alist nil))
                       (if-let ((alist (plist-get plist :alist)))
                           (setq prettify-symbols-alist (append alist prettify-symbols-alist))
                         (let ((plist plist)
                               results)
                           (while plist
                             (let ((prop (car plist))
                                   (sym (cadr plist)))
                               (when-let* ((icon (plist-get +pretty-code-symbols prop)))
                                 (push (cons sym (+pretty-code--icon-to-char (append icon nil)))
                                       results))
                               (setq plist (cddr plist))))
                           (setq prettify-symbols-alist (append results prettify-symbols-alist))))
                       (when (or (cadr (plist-member plist :iosevka))
                                 +pretty-code-iosevka-ligatures-enabled-by-default)
                         (+pretty-code-setup-iosevka-ligatures))
                       (when prettify-symbols-mode
                         (prettify-symbols-mode -1))
                       (prettify-symbols-mode +1))))
             (add-hook hook fn))))))
