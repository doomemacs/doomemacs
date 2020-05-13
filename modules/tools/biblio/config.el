;;; tools/biblio/config.el -*- lexical-binding: t; -*-

;;

;; Internal function to set the various paths used in the
;; reference packages.
(defun +biblio-set-paths-fn (&optional symbol value)
  (when symbol
    (set-default symbol value))
  (when value
    (cond ((eq symbol '+biblio-pdf-library-dir)
           (when (featurep! :lang org)
             (setq org-ref-pdf-directory value))
           (setq bibtex-completion-library-path value))
          ((eq symbol '+biblio-default-bibliography-files)
           (when (featurep! :lang org)
             (setq reftex-default-bibliography value
                   org-ref-default-bibliography value))
           (setq bibtex-completion-bibliography value)))))

(defcustom +biblio-pdf-library-dir nil
  "Directory where pdf files are stored. Must end with a slash."
  :type 'string
  :set #'+biblio-set-paths-fn)

(defcustom +biblio-default-bibliography-files nil
  "A list of default bibtex files to use."
  :type '(repeat :tag "List of bibtex files" file)
  :set #'+biblio-set-paths-fn)

(defvar +biblio-notes-path nil)


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
        org-cite-activate-processor 'citar)
  (when (featurep! :lang org +roam2)
    ;; Include property drawer metadata for 'org-roam' v2.
    (setq citar-file-note-org-include '(org-id org-roam-ref))))


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
  :preface
  ;; Allow the user to set a template of their own via (setq). if the user does
  ;; not set one fall back to the +biblio variants which have a reasonable
  ;; fallback.
  (defvar bibtex-completion-notes-template-multiple-files nil)
   :config

  (when (featurep! :completion ivy)
    (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))

  (setq bibtex-completion-additional-search-fields '(keywords)
        ;; This tell bibtex-completion to look at the File field of the bibtex
        ;; to figure out which pdf to open
        bibtex-completion-pdf-field "file")
  ;; orb will define handlers for note taking so not needed to use the
  ;; ones set for bibtex-completion
  (unless (featurep! :lang org +roam2)

    (setq bibtex-completion-notes-path +biblio-notes-path)
    (unless bibtex-completion-notes-template-multiple-files
      (setq bibtex-completion-notes-template-multiple-files
            "${title} : (${=key=})

- tags ::
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: /${file}\n  :NOTER_PAGE: \n  :END:\n\n"))))

;; TODO which set of keys that should be bound for commonly used functions
;; see https://github.com/jkitchin/org-ref/blob/master/org-ref-core.el#L3998
(use-package! org-ref
  :when (featurep! :lang org +ref)
  :after org
  :preface
  ;; This need to be set before the package is loaded, because org-ref will
  ;; automatically `require' an associated package during its loading.
  (setq org-ref-completion-library (cond ((featurep! :completion ivy)  #'org-ref-ivy-cite)
                                         ((featurep! :completion helm) #'org-ref-helm-bibtex)
                                         (t                            #'org-ref-reftex)))
  :config
;;  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus))

  ;; Although the name is helm-bibtex, it is actually a bibtex-completion function
  ;; it is the legacy naming of the project helm-bibtex that causes confusion.
  (setq org-ref-open-pdf-function 'org-ref-get-pdf-filename-helm-bibtex)
  ;; orb will define handlers for note taking so not needed to use the
  ;; ones set for bibtex-completion
  (unless (featurep! :lang org +roam2)
    ;; determine how org ref should handle the users notes path (dir, or file)
    (if (directory-name-p +biblio-notes-path)
        (setq org-ref-notes-directory +biblio-notes-path)
      (setq org-ref-bibliography-notes +biblio-notes-path))
    ;; Allow org-ref to use the same template mechanism as {helm,ivy}-bibtex for
    ;; multiple files if the user has chosen to spread their notes.
    (setq org-ref-notes-function (if (and org-ref-notes-directory (directory-name-p org-ref-notes-directory))
                                     #'org-ref-notes-function-many-files
                                   #'org-ref-notes-function-one-file))))


(use-package! org-roam-bibtex
  :when (featurep! :lang org +roam2)
  :after org-roam
  :preface
  ;; if the user has not set a template mechanism set a reasonable one of them
  ;; The package already tests for nil itself so we define a dummy tester
  (defvar orb-preformat-keywords
    '("title" "url" "file" "author-or-editor" "keywords" "citekey" "pdf"))
  ;;:hook (org-roam-mode . org-roam-bibtex-mode)
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
                 :unnarrowed t))
  (require 'org-ref))
