;;; tools/biblio/config.el -*- lexical-binding: t; -*-

;;
;;; `org-cite'

(use-package! oc
  :defer t
  :config
  (setq org-cite-global-bibliography
        (ensure-list
         (or (bound-and-true-p citar-bibliography)
             (bound-and-true-p bibtex-completion-bibliography)))
        ;; Setup export processor; default csl/citeproc-el, with biblatex for
        ;; latex
        org-cite-export-processors '((latex biblatex) (t csl))
        org-support-shift-select t))


(use-package! citar
  :when (modulep! :completion vertico)
  :no-require
  :config
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)
  (when (modulep! :completion vertico +icons)
    (defvar citar-indicator-files-icons
      (citar-indicator-create
       :symbol (all-the-icons-faicon
                "file-o"
                :face 'all-the-icons-green
                :v-adjust -0.1)
       :function #'citar-has-files
       :padding "  " ; need this because the default padding is too low for these icons
       :tag "has:files"))
    (defvar citar-indicator-links-icons
      (citar-indicator-create
       :symbol (all-the-icons-octicon
                "link"
                :face 'all-the-icons-orange
                :v-adjust 0.01)
       :function #'citar-has-links
       :padding "  "
       :tag "has:links"))
    (defvar citar-indicator-notes-icons
      (citar-indicator-create
       :symbol (all-the-icons-material
                "speaker_notes"
                :face 'all-the-icons-blue
                :v-adjust -0.3)
       :function #'citar-has-notes
       :padding "    "
       :tag "has:notes"))
    (defvar citar-indicator-cited-icons
      (citar-indicator-create
       :symbol (all-the-icons-faicon
                "circle-o"
                :face 'all-the-icon-green)
       :function #'citar-is-cited
       :padding "  "
       :tag "is:cited"))
    (setq citar-indicators
          (list citar-indicator-files-icons
                citar-indicator-links-icons
                citar-indicator-notes-icons
                citar-indicator-cited-icons))))

(use-package! citar-embark
  :when (modulep! :completion vertico)
  :after citar embark
  :config (citar-embark-mode))

(use-package! citar-org-roam
  :when (and (modulep! +roam2)
             (modulep! :completion vertico))
  :after citar org-roam
  :config (citar-org-roam-mode))

;; `org-cite' processors
(use-package! oc-biblatex :after oc)
(use-package! oc-csl :after oc)
(use-package! oc-natbib :after oc)


;;
;;; Third-party

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
