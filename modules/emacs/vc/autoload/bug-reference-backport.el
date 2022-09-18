;;; emacs/vc/autoload/bug-reference-backport.el -*- lexical-binding: t; -*-
;;;###if (< emacs-major-version 28)
;; DEPRECATED Remove when Emacs 27.x support is dropped

;; In Emacs 28, the built-in bug-reference package started consulting vc for
;; repo information (to inform its bug reference URLs). This incredibly useful
;; feature is not available in 27.x yet, so I've backported it:

(defvar bug-reference-setup-from-vc-alist
  `(;;
    ;; GNU projects on savannah.
    ;;
    ;; Not all of them use debbugs but that doesn't really matter
    ;; because the auto-setup is only performed if
    ;; `bug-reference-url-format' and `bug-reference-bug-regexp'
    ;; aren't set already.
    ("git\\.\\(?:sv\\|savannah\\)\\.gnu\\.org:"
     "\\<\\([Bb]ug ?#?\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)\\>"
     ,(lambda (_) "https://debbugs.gnu.org/%s"))
    ;;
    ;; GitHub projects.
    ;;
    ;; Here #17 may refer to either an issue or a pull request but
    ;; visiting the issue/17 web page will automatically redirect to
    ;; the pull/17 page if 17 is a PR.  Explicit user/project#17 links
    ;; to possibly different projects are also supported.
    ("[/@]github.com[/:]\\([.A-Za-z0-9_/-]+\\)\\.git"
     "\\([.A-Za-z0-9_/-]+\\)?\\(?:#\\)\\([0-9]+\\)\\>"
     ,(lambda (groups)
        (let ((ns-project (nth 1 groups)))
          (lambda ()
            (concat "https://github.com/"
                    (or
                     ;; Explicit user/proj#18 link.
                     (match-string 1)
                     ns-project)
                    "/issues/"
                    (match-string 2))))))
    ;;
    ;; GitLab projects.
    ;;
    ;; Here #18 is an issue and !17 is a merge request.  Explicit
    ;; namespace/project#18 or namespace/project!17 references to
    ;; possibly different projects are also supported.
    ("[/@]gitlab.com[/:]\\([.A-Za-z0-9_/-]+\\)\\.git"
     "\\(?1:[.A-Za-z0-9_/-]+\\)?\\(?3:[#!]\\)\\(?2:[0-9]+\\)\\>"
     ,(lambda (groups)
        (let ((ns-project (nth 1 groups)))
          (lambda ()
            (concat "https://gitlab.com/"
                    (or (match-string 1)
                        ns-project)
                    "/-/"
                    (if (string= (match-string 3) "#")
                        "issues/"
                      "merge_requests/")
                    (match-string 2)))))))
  "An alist for setting up `bug-reference-mode' based on VC URL.

Each element has the form (URL-REGEXP BUG-REGEXP URL-FORMAT-FN).

URL-REGEXP is matched against the version control URL of the
current buffer's file.  If it matches, BUG-REGEXP is set as
`bug-reference-bug-regexp'.  URL-FORMAT-FN is a function of one
argument that receives a list of the groups 0 to N of matching
URL-REGEXP against the VCS URL and returns the value to be set as
`bug-reference-url-format'.")

(defun bug-reference--maybe-setup-from-vc (url url-rx bug-rx bug-url-fmt)
  (when (string-match url-rx url)
    (setq-local bug-reference-bug-regexp bug-rx)
    (setq-local bug-reference-url-format
                (let (groups)
                  (dotimes (i (/ (length (match-data)) 2))
                    (push (match-string i url) groups))
                  (funcall bug-url-fmt (nreverse groups))))))

;;;###autoload
(defun bug-reference-try-setup-from-vc ()
  "Try setting up `bug-reference-mode' based on VC information.
Test each configuration in `bug-reference-setup-from-vc-alist'
and apply it if applicable."
  (when (require 'browse-at-remote nil t)
    (when-let* ((remote (car (browse-at-remote--get-remotes)))
                (url (browse-at-remote--get-remote-url remote)))
      (catch 'found
        (dolist (config bug-reference-setup-from-vc-alist)
          (and (apply #'bug-reference--maybe-setup-from-vc
                      url config)
               (throw 'found t)))))))

;;;###autoload
(add-hook! '(bug-reference-mode-hook
             bug-reference-prog-mode-hook)
  (defun +vc-init-bug-reference-from-vc-h ()
    (when (or bug-reference-mode
              bug-reference-prog-mode)
      (unless (and bug-reference-bug-regexp
                   bug-reference-url-format)
        (bug-reference-try-setup-from-vc)))))
