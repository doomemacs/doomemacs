;;; tools/biblio/config.el -*- lexical-binding: t; -*-

;;
;;; `org-cite'

(after! oc
  (setq org-cite-global-bibliography
        (ensure-list
         (or (bound-and-true-p citar-bibliography)
             (bound-and-true-p bibtex-completion-bibliography)))
        ;; Setup export processor; default csl/citeproc-el, with biblatex for
        ;; latex
        org-cite-export-processors '((latex biblatex) (t csl))
        org-support-shift-select t)

  (require 'oc-biblatex))

;; oc-csl requires citeproc, which requires the top-level org, so loading oc-csl
;; after oc interferes with incremental loading of Org
(after! org (require 'oc-csl))


(use-package! citar
  :when (modulep! :completion vertico)
  :defer t
  :init
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)

  :config
  (when (modulep! :completion vertico +icons)
    (defvar citar-indicator-files-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-file_o"
                :face 'nerd-icons-green
                :v-adjust -0.1)
       :function #'citar-has-files
       :padding "  " ; need this because the default padding is too low for these icons
       :tag "has:files"))
    (defvar citar-indicator-links-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-link"
                :face 'nerd-icons-orange
                :v-adjust 0.01)
       :function #'citar-has-links
       :padding "  "
       :tag "has:links"))
    (defvar citar-indicator-notes-icons
      (citar-indicator-create
       :symbol (nerd-icons-codicon
                "nf-cod-note"
                :face 'nerd-icons-blue
                :v-adjust -0.3)
       :function #'citar-has-notes
       :padding "    "
       :tag "has:notes"))
    (defvar citar-indicator-cited-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-circle_o"
                :face 'nerd-icon-green)
       :function #'citar-is-cited
       :padding "  "
       :tag "is:cited"))
    (setq citar-indicators
          (list citar-indicator-files-icons
                citar-indicator-links-icons
                citar-indicator-notes-icons
                citar-indicator-cited-icons))))


;;
;;; Third-party

(use-package! citar-embark
  :defer t
  :init
  (after! (citar embark)
    (citar-embark-mode)))


(use-package! citar-org-roam
  :defer t
  :init
  (after! (citar org-roam)
    (citar-org-roam-mode)))


(use-package! bibtex-completion
  :when (or (modulep! :completion ivy)
            (modulep! :completion helm))
  :defer t
  :config
  (setq bibtex-completion-additional-search-fields '(keywords)
        ;; Tell bibtex-completion to look at the File field of the bibtex to
        ;; figure out which pdf to open:
        bibtex-completion-pdf-field "file"))


(use-package! ivy-bibtex
  :when (modulep! :completion ivy)
  :defer t
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))
