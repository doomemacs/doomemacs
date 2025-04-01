;;; lang/graphql/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +graphql-doc-open-config ()
  "Locate a .graphqlconfig file in the current tree and use that to point to a
  schema."
  (interactive)
  (let ((config (json-read-file (graphql-locate-config "."))))
    (let-alist config
      (if-let* ((endpoints .extensions.endpoints)
                (endpoint (cdr (assq (intern (graphql--completing-read-endpoint endpoints)) endpoints))))
          (let-alist endpoint
            (graphql-doc--start .url `(:url ,.url :headers ,.headers)))
        (error "No endpoint configurations in .graphqlconfig")))))
