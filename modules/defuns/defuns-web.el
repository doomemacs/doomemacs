;;; defuns-web.el

;;;###autoload
(defun narf/web-html-email2mailto (beg end)
  (interactive "r")
  (replace-regexp "\\b\\([a-zA-Z0-9._+-%]+@[a-zA-Z0-9-.]+\\.[a-zA-Z]+\\)\\b"
                  "<a href=\"mailto:\\1\">\\1</a>"
                  nil beg end))

;;;###autoload
(defun narf/web-html-url2anchor (beg end)
  (interactive "r")
  (replace-regexp "\\bhttps?://.+?\\b"
                  "<a href=\"\\1\">\\1</a>"
                  nil beg end))

;;;###autoload
(defun narf/web-refresh-browser ()
  (interactive)
  (call-process-shell-command "osascript -e 'tell application \"Google Chrome\" to tell the active tab of its first window to reload' &" nil 0))

;;;###autoload (autoload 'narf/html-entities "defuns-web" nil t)
(evil-define-operator narf/html-entities (bang beg end)
  "HTML encode/decode the selected region. Based on Xah's replace HTML named entities
function @ http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html"
  (interactive "<!><r>")
  (let ((reps [["&nbsp;" " "] ["&ensp;" " "] ["&emsp;" " "] ["&thinsp;" " "]
               ["&rlm;" "‏"] ["&lrm;" "‎"] ["&zwj;" "‍"] ["&zwnj;" "‌"]
               ["&iexcl;" "¡"] ["&cent;" "¢"] ["&pound;" "£"] ["&curren;" "¤"] ["&yen;" "¥"] ["&brvbar;" "¦"] ["&sect;" "§"] ["&uml;" "¨"] ["&copy;" "©"] ["&ordf;" "ª"] ["&laquo;" "«"] ["&not;" "¬"] ["&shy;" "­"] ["&reg;" "®"] ["&macr;" "¯"] ["&deg;" "°"] ["&plusmn;" "±"] ["&sup2;" "²"] ["&sup3;" "³"] ["&acute;" "´"] ["&micro;" "µ"] ["&para;" "¶"] ["&middot;" "·"] ["&cedil;" "¸"] ["&sup1;" "¹"] ["&ordm;" "º"] ["&raquo;" "»"] ["&frac14;" "¼"] ["&frac12;" "½"] ["&frac34;" "¾"] ["&iquest;" "¿"]
               ["&Agrave;" "À"] ["&Aacute;" "Á"] ["&Acirc;" "Â"] ["&Atilde;" "Ã"] ["&Auml;" "Ä"] ["&Aring;" "Å"] ["&AElig;" "Æ"] ["&Ccedil;" "Ç"] ["&Egrave;" "È"] ["&Eacute;" "É"] ["&Ecirc;" "Ê"] ["&Euml;" "Ë"] ["&Igrave;" "Ì"] ["&Iacute;" "Í"] ["&Icirc;" "Î"] ["&Iuml;" "Ï"] ["&ETH;" "Ð"] ["&Ntilde;" "Ñ"] ["&Ograve;" "Ò"] ["&Oacute;" "Ó"] ["&Ocirc;" "Ô"] ["&Otilde;" "Õ"] ["&Ouml;" "Ö"] ["&times;" "×"] ["&Oslash;" "Ø"] ["&Ugrave;" "Ù"] ["&Uacute;" "Ú"] ["&Ucirc;" "Û"] ["&Uuml;" "Ü"] ["&Yacute;" "Ý"] ["&THORN;" "Þ"] ["&szlig;" "ß"] ["&agrave;" "à"] ["&aacute;" "á"] ["&acirc;" "â"] ["&atilde;" "ã"] ["&auml;" "ä"] ["&aring;" "å"] ["&aelig;" "æ"] ["&ccedil;" "ç"] ["&egrave;" "è"] ["&eacute;" "é"] ["&ecirc;" "ê"] ["&euml;" "ë"] ["&igrave;" "ì"] ["&iacute;" "í"] ["&icirc;" "î"] ["&iuml;" "ï"] ["&eth;" "ð"] ["&ntilde;" "ñ"] ["&ograve;" "ò"] ["&oacute;" "ó"] ["&ocirc;" "ô"] ["&otilde;" "õ"] ["&ouml;" "ö"]
               ["&divide;" "÷"] ["&oslash;" "ø"] ["&ugrave;" "ù"] ["&uacute;" "ú"] ["&ucirc;" "û"] ["&uuml;" "ü"] ["&yacute;" "ý"] ["&thorn;" "þ"] ["&yuml;" "ÿ"] ["&fnof;" "ƒ"]
               ["&Alpha;" "Α"] ["&Beta;" "Β"] ["&Gamma;" "Γ"] ["&Delta;" "Δ"] ["&Epsilon;" "Ε"] ["&Zeta;" "Ζ"] ["&Eta;" "Η"] ["&Theta;" "Θ"] ["&Iota;" "Ι"] ["&Kappa;" "Κ"] ["&Lambda;" "Λ"] ["&Mu;" "Μ"] ["&Nu;" "Ν"] ["&Xi;" "Ξ"] ["&Omicron;" "Ο"] ["&Pi;" "Π"] ["&Rho;" "Ρ"] ["&Sigma;" "Σ"] ["&Tau;" "Τ"] ["&Upsilon;" "Υ"] ["&Phi;" "Φ"] ["&Chi;" "Χ"] ["&Psi;" "Ψ"] ["&Omega;" "Ω"] ["&alpha;" "α"] ["&beta;" "β"] ["&gamma;" "γ"] ["&delta;" "δ"] ["&epsilon;" "ε"] ["&zeta;" "ζ"] ["&eta;" "η"] ["&theta;" "θ"] ["&iota;" "ι"] ["&kappa;" "κ"] ["&lambda;" "λ"] ["&mu;" "μ"] ["&nu;" "ν"] ["&xi;" "ξ"] ["&omicron;" "ο"] ["&pi;" "π"] ["&rho;" "ρ"] ["&sigmaf;" "ς"] ["&sigma;" "σ"] ["&tau;" "τ"] ["&upsilon;" "υ"] ["&phi;" "φ"] ["&chi;" "χ"] ["&psi;" "ψ"] ["&omega;" "ω"] ["&thetasym;" "ϑ"] ["&upsih;" "ϒ"] ["&piv;" "ϖ"]
               ["&bull;" "•"] ["&hellip;" "…"] ["&prime;" "′"] ["&Prime;" "″"] ["&oline;" "‾"] ["&frasl;" "⁄"] ["&weierp;" "℘"] ["&image;" "ℑ"] ["&real;" "ℜ"] ["&trade;" "™"] ["&alefsym;" "ℵ"] ["&larr;" "←"] ["&uarr;" "↑"] ["&rarr;" "→"] ["&darr;" "↓"] ["&harr;" "↔"] ["&crarr;" "↵"] ["&lArr;" "⇐"] ["&uArr;" "⇑"] ["&rArr;" "⇒"] ["&dArr;" "⇓"] ["&hArr;" "⇔"] ["&forall;" "∀"] ["&part;" "∂"] ["&exist;" "∃"] ["&empty;" "∅"] ["&nabla;" "∇"] ["&isin;" "∈"] ["&notin;" "∉"] ["&ni;" "∋"] ["&prod;" "∏"] ["&sum;" "∑"] ["&minus;" "−"] ["&lowast;" "∗"] ["&radic;" "√"] ["&prop;" "∝"] ["&infin;" "∞"] ["&ang;" "∠"] ["&and;" "∧"] ["&or;" "∨"] ["&cap;" "∩"] ["&cup;" "∪"] ["&int;" "∫"] ["&there4;" "∴"] ["&sim;" "∼"] ["&cong;" "≅"] ["&asymp;" "≈"] ["&ne;" "≠"] ["&equiv;" "≡"] ["&le;" "≤"] ["&ge;" "≥"] ["&sub;" "⊂"] ["&sup;" "⊃"] ["&nsub;" "⊄"] ["&sube;" "⊆"] ["&supe;" "⊇"] ["&oplus;" "⊕"] ["&otimes;" "⊗"] ["&perp;" "⊥"] ["&sdot;" "⋅"] ["&lceil;" "⌈"] ["&rceil;" "⌉"] ["&lfloor;" "⌊"] ["&rfloor;" "⌋"] ["&lang;" "〈"] ["&rang;" "〉"] ["&loz;" "◊"] ["&spades;" "♠"] ["&clubs;" "♣"] ["&hearts;" "♥"] ["&diams;" "♦"] ["&quot;" "\""] ["&OElig;" "Œ"] ["&oelig;" "œ"] ["&Scaron;" "Š"] ["&scaron;" "š"] ["&Yuml;" "Ÿ"] ["&circ;" "ˆ"] ["&tilde;" "˜"] ["&ndash;" "–"] ["&mdash;" "—"] ["&lsquo;" "‘"] ["&rsquo;" "’"] ["&sbquo;" "‚"] ["&ldquo;" "“"] ["&rdquo;" "”"] ["&bdquo;" "„"] ["&dagger;" "†"] ["&Dagger;" "‡"] ["&permil;" "‰"] ["&lsaquo;" "‹"] ["&rsaquo;" "›"] ["&euro;" "€"]
               ]))
    (save-restriction
      (narrow-to-region beg end)
      (let (case-fold-search)
        (mapc (lambda (rep)
                (goto-char (point-min))
                (while (search-forward (elt rep (if bang 0 1)) nil t)
                  (replace-match (elt rep (if bang 1 0)) 'FIXEDCASE 'LITERAL)))
              reps)))))

(provide 'defuns-web)
;;; defuns-web.el ends here
