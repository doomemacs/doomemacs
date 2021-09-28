;;; tools/lsp/autoload/eglot.el -*- lexical-binding: t; -*-
;;;###if (featurep! +eglot)

;;;###autodef
(defun set-eglot-client! (mode server-call)
  "Add SERVER-CALL list as a possible lsp server for given major MODE.

Example : (set-eglot-client! 'python-mode `(,(concat doom-etc-dir \"lsp/mspyls/Microsoft.Python.LanguageServer\")))"
  (after! eglot
    (add-to-list 'eglot-server-programs `(,mode . ,server-call))))

;; HACK Eglot removed `eglot-help-at-point' in joaotavora/eglot@a044dec for a
;;      more problematic approach of deferred to eldoc. Here, I've restored it.
;;      Doom's lookup handlers try to open documentation in a separate window
;;      (so they can be copied or kept open), but doing so with an eldoc buffer
;;      is difficult because a) its contents are generated asynchronously,
;;      making them tough to scrape, and b) their contents change frequently
;;      (every time you move your cursor).
(defvar +eglot--help-buffer nil)
;;;###autoload
(defun +eglot-lookup-documentation (_identifier)
  "Request documentation for the thing at point."
  (eglot--dbind ((Hover) contents range)
      (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                       (eglot--TextDocumentPositionParams))
    (let ((blurb (and (not (seq-empty-p contents))
                      (eglot--hover-info contents range)))
          (hint (thing-at-point 'symbol)))
      (if blurb
          (with-current-buffer
              (or (and (buffer-live-p +eglot--help-buffer)
                       +eglot--help-buffer)
                  (setq +eglot--help-buffer (generate-new-buffer "*eglot-help*")))
            (with-help-window (current-buffer)
              (rename-buffer (format "*eglot-help for %s*" hint))
              (with-current-buffer standard-output (insert blurb))
              (setq-local nobreak-char-display nil)))
        (display-local-help))))
  'deferred)
