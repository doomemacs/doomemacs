;;; tools/lookup/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (modulep! :editor evil)

;;;###autoload (autoload '+lookup:online "tools/lookup/autoload/evil" nil t)
(evil-define-command +lookup:online (query &optional bang)
  "Look up QUERY online. Will prompt for search engine the first time, then
reuse it on consecutive uses of this command. If BANG, always prompt for search
engine."
  (interactive "<a><!>")
  (+lookup/online query (+lookup--online-provider bang 'evil-ex)))

;;;###autoload (autoload '+lookup:dash "tools/lookup/autoload/evil" nil t)
(evil-define-command +lookup:dash (query &optional bang)
  "Look up QUERY in your dash docsets. If BANG, prompt to select a docset (and
install it if necessary)."
  (interactive "<a><!>")
  (let (selected)
    (when bang
      (setq selected (helm-dash-read-docset "Select docset" (helm-dash-official-docsets)))
      (unless (dash-docs-docset-path selected)
        (+lookup/install-docset selected)))
    (+lookup/in-docsets query selected)))
