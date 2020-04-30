;;; ui/pretty-code/+hasklig.el -*- lexical-binding: t; -*-

(defvar +pretty-code-hasklig-font-name "Hasklig"
  "Name of the hasklig ligature font.")

(defvar +pretty-code-hasklig-font-ligatures
  '(("&&"   . #Xe100)
    ("***"  . #Xe101)
    ("*>"   . #Xe102)
    ("\\\\" . #Xe103)
    ("||"   . #Xe104)
    ("|>"   . #Xe105)
    ("::"   . #Xe106)
    ("=="   . #Xe107)
    ("==="  . #Xe108)
    ("==>"  . #Xe109)
    ("=>"   . #Xe10a)
    ("=<<"  . #Xe10b)
    ("!!"   . #Xe10c)
    (">>"   . #Xe10d)
    (">>="  . #Xe10e)
    (">>>"  . #Xe10f)
    (">>-"  . #Xe110)
    (">-"   . #Xe111)
    ("->"   . #Xe112)
    ("-<"   . #Xe113)
    ("-<<"  . #Xe114)
    ("<*"   . #Xe115)
    ("<*>"  . #Xe116)
    ("<|"   . #Xe117)
    ("<|>"  . #Xe118)
    ("<$>"  . #Xe119)
    ("<>"   . #Xe11a)
    ("<-"   . #Xe11b)
    ("<<"   . #Xe11c)
    ("<<<"  . #Xe11d)
    ("<+>"  . #Xe11e)
    (".."   . #Xe11f)
    ("..."  . #Xe120)
    ("++"   . #Xe121)
    ("+++"  . #Xe122)
    ("/="   . #Xe123)
    (":::"  . #Xe124)
    (">=>"  . #Xe125)
    ("->>"  . #Xe126)
    ("<=>"  . #Xe127)
    ("<=<"  . #Xe128)
    ("<->"  . #Xe129)))


(defun +pretty-code-setup-hasklig-ligatures-h ()
  (set-fontset-font t '(#Xe100 . #Xe129) +pretty-code-hasklig-font-name nil 'prepend)
  (setq-default prettify-symbols-alist
                (append prettify-symbols-alist
                        (mapcar #'+pretty-code--correct-symbol-bounds
                                +pretty-code-hasklig-font-ligatures))))

(add-hook 'doom-init-ui-hook #'+pretty-code-setup-hasklig-ligatures-h)
