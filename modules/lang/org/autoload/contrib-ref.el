;; lang/org/autoload/contrib-ref.el -*- lexical-binding: t; -*-
;;;###if (featurep! +ref)
;; * batch
;;;###autoload
(defun +org-reference-bibtex-wash--bibtex ()
  (interactive)
  (while (parsebib-find-next-item)
    (if (and (not (string-equal
                   "bioRxiv"
                   (bibtex-completion-get-value
                    "journal"
                    (bibtex-completion-get-entry
                     (bibtex-completion-key-at-point)))))
             (not (string-equal
                   "bioRxiv"
                   (bibtex-completion-get-value
                    "location"
                    (bibtex-completion-get-entry
                     (bibtex-completion-key-at-point)))))
             (not (string-equal
                   "arXiv.org"
                   (bibtex-completion-get-value
                    "journal"
                    (bibtex-completion-get-entry
                     (bibtex-completion-key-at-point)))))
             (not (string-equal
                   ""
                   (bibtex-completion-get-value
                    "journal"
                    (bibtex-completion-get-entry
                     (bibtex-completion-key-at-point)))))
             (not (string-equal
                   ""
                   (bibtex-completion-get-value
                    "doi"
                    (bibtex-completion-get-entry
                     (bibtex-completion-key-at-point))))))
        (progn
          (condition-case nil
              (call-interactively
               'doi-utils-update-bibtex-entry-from-doi)
            (error t))
          (parsebib-find-next-item)))))
;;;###autoload
(defun +org-reference-bibtex-wash--biorxiv ()
  (interactive)
  (while (parsebib-find-next-item)
    (if (and (or (string-equal
                  "bioRxiv"
                  (bibtex-completion-get-value
                   "journal"
                   (bibtex-completion-get-entry
                    (bibtex-completion-key-at-point))))
                 (string-equal
                  "bioRxiv"
                  (bibtex-completion-get-value
                   "location"
                   (bibtex-completion-get-entry
                    (bibtex-completion-key-at-point)))))
             (not (string-equal
                   ""
                   (bibtex-completion-get-value
                    "url"
                    (bibtex-completion-get-entry
                     (bibtex-completion-key-at-point))))))
        (progn
          (condition-case nil
              (call-interactively
               '+org-reference-biorxiv-update-bibtex)
            (error t))
          (parsebib-find-next-item))
      (parsebib-find-next-item))))
;;;###autoload
(defun +org-reference-bibtex-get-all-pdf ()
  (interactive)
  (while (parsebib-find-next-item)
    (if (not (bibtex-completion-find-pdf
              (bibtex-completion-key-at-point)))
        (progn
          (call-interactively
           'doi-utils-get-bibtex-entry-pdf)
          (parsebib-find-next-item))
      (parsebib-find-next-item))))


