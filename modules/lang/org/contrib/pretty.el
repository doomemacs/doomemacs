;;; lang/org/contrib/pretty.el -*- lexical-binding: t; -*-
;;;###if (modulep! +pretty)
(after! org
  (setq org-highlight-latex-and-related '(native script entities)))


(use-package! org-appear
  :hook (org-mode . org-appear-mode))


(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-mode)
  :hook (org-modern-mode . +org-pretty-mode)
  :config
  ;; HACK: If `org-indent-mode' is active, org-modern's default of hiding
  ;;   leading stars makes sub-headings look too sunken into the left margin.
  ;;   Those stars are already "hidden" by `org-hide-leading-stars' anyway, so
  ;;   rely on just that.
  (add-hook! 'org-modern-mode-hook
    (defun +org-modern-show-hidden-stars-in-indent-mode-h ()
      (when org-indent-mode
        (setq-local org-modern-hide-stars nil)))))
