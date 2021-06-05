;;; ui/tabs/autoload.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor evil)

;;;###autoload (autoload '+tabs:next-or-goto "ui/tabs/autoload" nil t)
(evil-define-command +tabs:next-or-goto (index)
  "Switch to the next tab, or to INDEXth tab if a count is given."
  (interactive "<C>")
  (if current-prefix-arg
      (centaur-tabs-select-visible-nth-tab current-prefix-arg)
    (centaur-tabs-forward)))

;;;###autoload (autoload '+tabs:previous-or-goto "ui/tabs/autoload" nil t)
(evil-define-command +tabs:previous-or-goto (index)
  "Switch to the previous tab, or to INDEXth tab if a count is given."
  (interactive "<C>")
  (if current-prefix-arg
      (centaur-tabs-select-visible-nth-tab current-prefix-arg)
    (centaur-tabs-backward)))
