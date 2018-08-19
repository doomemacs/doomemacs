;;; feature/lookup/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autoload (autoload '+lookup:online "feature/lookup/autoload/evil" nil t)
(evil-define-command +lookup:online (query &optional bang)
  "Look up QUERY online. Will prompt for search engine the first time, then
reuse it on consecutive uses of this command. If BANG, always prompt for search
engine."
  (interactive "<a><!>")
  (+lookup/online query (+lookup--online-provider bang 'evil-ex)))

;;;###autoload (autoload '+lookup:dash "feature/lookup/autoload/evil" nil t)
(evil-define-command +lookup:dash (query &optional bang)
  "TODO"
  (interactive "<a><!>")
  (+lookup/in-docsets query (if bang 'blank)))

;;;###autoload (autoload '+lookup:devdocs "feature/lookup/autoload/evil" nil t)
(evil-define-command +lookup:devdocs (query &optional bang)
  "TODO"
  (interactive "<a><!>")
  (+lookup/in-devdocs query (if bang 'blank)))
