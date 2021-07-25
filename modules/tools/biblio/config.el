;;; tools/biblio/config.el -*- lexical-binding: t; -*-

(use-package! bibtex-completion
  :defer t
  :config
  (setq bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-field "file"));; This tell bibtex-completion to look at the File field of the bibtex to figure out which pdf to open


(use-package! ivy-bibtex
  :when (featurep! :completion ivy)
  :defer t
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))

(use-package! citeproc
  :defer t)

;;; Org-Cite configuration

(when (and (featurep! :lang org)
            ;; Only run if org-cite present.
            ;;
            ;; REVIEW is this necessary? OC is new, but built-in.
           (featurep 'oc))

    ;;; Org-cite configuration
  (use-package! oc
    :after org
    :config
    ;; activate processor for fontification, preview, etc
    ;; currently using basic, but would prefer org-cite-csl-activate
    (setq org-cite-activate-processor 'basic)

    (if (featurep! :completion vertico)
        (setq org-cite-follow-processor 'oc-bibtex-actions
              org-cite-insert-processor 'oc-bibtex-actions)
      (setq org-cite-follow-processor 'basic
            org-cite-insert-processor 'basic))

    ;; setup export processor; default csl/citeproc-el, with biblatex for latex
    (setq org-cite-export-processors
          '((beamer natbib)
            (latex biblatex)
            (t csl)))

    ;; need to set oc to use +biblio-default-bibliography-files
    (setq org-cite-global-bibliography '("~/bib/references.bib")))

  ;;; Org-cite processors
  (use-package! oc-basic
    :after oc)

  (use-package! oc-biblatex
    :after oc)

  (use-package! oc-csl
    :after oc
    :config
    ;; optional; add to docs instead?
    (setq org-cite-csl-styles-dir "~/.local/share/csl/styles")
    (setq org-cite-csl-locales-dir "~/.local/share/csl/locales"))

  (use-package! oc-natbib
    :after oc)

  (use-package! oc-bibtex-actions
    ;; REVIEW Currently, I (the author of this package) put org-cite-related
    ;;   functions and variable setting in this separate package file. Is this
    ;;   the best approach, or is there a better way?
    :when (featurep! :completion vertico)
    :after (embark org oc bibtex-actions)))

(use-package! bibtex-actions
  :when (featurep! :completion vertico)
  :after embark
  :defer t
  :config
  (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map)))
