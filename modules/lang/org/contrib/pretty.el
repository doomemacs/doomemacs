;;; lang/org/contrib/pretty.el -*- lexical-binding: t; -*-
;;;###if (modulep! +pretty)
(after! org
  (setq org-highlight-latex-and-related '(native script entities)))


(use-package! org-appear
  :hook (org-mode . org-appear-mode))


(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda)
  :init
  (after! org
    (setq org-hide-emphasis-markers t
          org-pretty-entities t))
  :config
  ;; HACK: If `org-indent-mode' is active, org-modern's default of hiding
  ;;   leading stars makes sub-headings look too sunken into the left margin.
  ;;   Those stars are already "hidden" by `org-hide-leading-stars' anyway, so
  ;;   rely on just that.
  (add-hook! 'org-modern-mode-hook
    (defun +org-modern-show-hidden-stars-in-indent-mode-h ()
      (when (bound-and-true-p org-indent-mode)
        (setq-local org-modern-hide-stars nil))))

  ;; Carry over the default values of `org-todo-keyword-faces', `org-tag-faces',
  ;; and `org-priority-faces' as reasonably as possible, but only if the user
  ;; hasn't already modified them.
  (letf! (defun new-spec (spec)
           (if (or (facep (cdr spec))
                   (not (keyword (car-safe (cdr spec)))))
               `(:inherit ,(cdr spec))
             (cdr spec)))
    (unless org-modern-tag-faces
      (dolist (spec org-tag-faces)
        (add-to-list 'org-modern-tag-faces `(,(car spec) :inverse-video t ,@(new-spec spec)))))
    (unless org-modern-todo-faces
      (dolist (spec org-todo-keyword-faces)
        (add-to-list 'org-modern-todo-faces `(,(car spec) :inverse-video t ,@(new-spec spec)))))
    (unless org-modern-priority-faces
      (dolist (spec org-priority-faces)
        (add-to-list 'org-modern-priority-faces `(,(car spec) :inverse-video t ,@(new-spec spec)))))))
