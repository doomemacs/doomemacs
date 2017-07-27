;;; feature/jump/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+jump:online "feature/jump/autoload/evil" nil t)
(evil-define-command +jump:online (query &optional bang)
  "Look up QUERY online. Will prompt for search engine the first time, then
reuse it on consecutive uses of this command. If BANG, always prompt for search
engine."
  (interactive "<a><!>")
  (+jump/online (or query (thing-at-point 'symbol t))
                (+jump--online-get-provider bang)))
