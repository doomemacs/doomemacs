;;; lang/org/contrib/pretty.el -*- lexical-binding: t; -*-
;;;###if (modulep! +pretty)
(after! org
  (setq org-highlight-latex-and-related '(native script entities)))


(use-package! org-superstar ; "prettier" bullets
  :hook (org-mode . org-superstar-mode)
  :config
  ;; Make leading stars truly invisible, by rendering them as spaces!
  (setq org-superstar-leading-bullet ?\s
        org-superstar-leading-fallback ?\s
        org-hide-leading-stars nil
        org-superstar-todo-bullet-alist
        '(("TODO" . 9744)
          ("[ ]"  . 9744)
          ("DONE" . 9745)
          ("[X]"  . 9745))))


(use-package! org-fancy-priorities ; priority icons
  :hook (org-mode . org-fancy-priorities-mode)
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚑" "⬆" "■"))
  ;; HACK: Prevent org-fancy-priorities from interfering with org exporters or
  ;;   other non-interactive Org crawlers/parsers (see #8280).
  (defadvice! +org--inhibit-org-fancy-in-non-real-buffers-a (&rest _)
    :before-until #'org-fancy-priorities-mode
    org-inhibit-startup))


(use-package! org-appear ; better markup edit
  :hook (org-mode . org-appear-mode))
