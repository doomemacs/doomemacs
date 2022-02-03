;;; tools/biblio/config.el -*- lexical-binding: t; -*-

;;
;;; `org-cite'

(use-package! oc
  :defer t
  :config
  (setq org-cite-global-bibliography
        (doom-enlist
         (or (bound-and-true-p citar-bibliography)
             (bound-and-true-p bibtex-completion-bibliography)))
        ;; Setup export processor; default csl/citeproc-el, with biblatex for
        ;; latex
        org-cite-export-processors '((latex biblatex) (t csl))
        org-support-shift-select t))


(use-package! citar
  :when (featurep! :completion vertico)
  :no-require
  :config
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar))

;; `org-cite' processors
(use-package! oc-biblatex :after oc)
(use-package! oc-csl :after oc)
(use-package! oc-natbib :after oc)


;;
;;; Third-party

(use-package! bibtex-completion
  :when (or (featurep! :completion ivy)
            (featurep! :completion helm))
  :defer t
  :config
  (setq bibtex-completion-additional-search-fields '(keywords)
        ;; Tell bibtex-completion to look at the File field of the bibtex to
        ;; figure out which pdf to open:
        bibtex-completion-pdf-field "file"))


(use-package! ivy-bibtex
  :when (featurep! :completion ivy)
  :defer t
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))


(use-package! org-roam-bibtex
  :when (featurep! :lang org +roam2)
  :after org-roam
  :preface
  ;; if the user has not set a template mechanism set a reasonable one of them
  ;; The package already tests for nil itself so we define a dummy tester
  (defvar orb-preformat-keywords
    '("title" "url" "file" "author-or-editor" "keywords" "citekey" "pdf"))
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :custom
  (orb-note-actions-interface (cond ((featurep! :completion ivy)  'ivy)
                                    ((featurep! :completion helm) 'helm)
                                    ((t                           'default))))
  :config
  (setq orb-insert-interface (cond ((featurep! :completion ivy)  'ivy-bibtex)
                                   ((featurep! :completion helm) 'helm-bibtex)
                                   ((t                           'generic))))
  (setq orb-process-file-keyword t
        orb-file-field-extensions '("pdf"))

  (add-to-list 'org-roam-capture-templates
               '("b" "Bibliography note" plain
                 "%?
- keywords :: %^{keywords}
- related ::

* %^{title}
:PROPERTIES:
:Custom_ID: %^{citekey}
:URL: %^{url}
:AUTHOR: %^{author-or-editor}
:NOTER_DOCUMENT: %^{file}
:NOTER_PAGE:
:END:\n\n"
                 :if-new (file+head "${citekey}.org" ":PROPERTIES:
:ROAM_REFS: cite:${citekey}
:END:
#+TITLE: ${title}\n")
                 :unnarrowed t)))
