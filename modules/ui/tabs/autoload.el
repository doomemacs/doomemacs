;;; ui/tabs/autoload.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor evil)

;;;###autoload (autoload '+tabs:next-or-goto "ui/tabs/autoload" nil t)
(evil-define-command +tabs:next-or-goto (index)
  "Switch to the next tab, or to INDEXth tab if a count is given."
  (interactive "<c>")
  (if index
      (centaur-tabs-select-visible-nth-tab index)
    (centaur-tabs-forward)))

;;;###autoload (autoload '+tabs:previous-or-goto "ui/tabs/autoload" nil t)
(evil-define-command +tabs:previous-or-goto (index)
  "Switch to the previous tab, or to INDEXth tab if a count is given."
  (interactive "<c>")
  (if index
      (centaur-tabs-select-visible-nth-tab index)
    (centaur-tabs-backward)))
