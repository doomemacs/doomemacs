;;; lang/coq/config.el -*- lexical-binding: t; -*-

;;;###package proof-general
(setq proof-splash-enable nil)
;; REVIEW: Remove when ProofGeneral/PG#771 is fixed. Also see #8169.
(require 'proof-site
         (expand-file-name "generic/proof-site"
                           (file-name-directory (locate-library "proof-general"))))


;;;###package coq
(setq-hook! 'coq-mode-hook
  ;; Doom syncs other indent variables with `tab-width'; we trust major modes to
  ;; set it -- which most of them do -- but coq-mode doesn't, so...
  tab-width proof-indent
  ;; HACK Fix #2081: Doom continues comments on RET, but coq-mode doesn't have a
  ;;      sane `comment-line-break-function', so...
  comment-line-break-function nil)

;; We've replaced coq-mode abbrevs with yasnippet snippets (in the snippets
;; library included with Doom).
(setq coq-mode-abbrev-table '())

(map! :after coq-mode
      :map coq-mode-map
      :localleader
      "]"  #'proof-assert-next-command-interactive
      "["  #'proof-undo-last-successful-command
      "."  #'proof-goto-point
      (:prefix ("l" . "layout")
        "c" #'pg-response-clear-displays
        "l" #'proof-layout-windows
        "p" #'proof-prf)
      (:prefix ("p" . "proof")
        "i" #'proof-interrupt-process
        "p" #'proof-process-buffer
        "q" #'proof-shell-exit
        "r" #'proof-retract-buffer)
      (:prefix ("a" . "about/print/check")
        "a" #'coq-Print
        "A" #'coq-Print-with-all
        "b" #'coq-About
        "B" #'coq-About-with-all
        "c" #'coq-Check
        "C" #'coq-Check-show-all
        "f" #'proof-find-theorems
        (:prefix ("i" . "implicits")
          "b" #'coq-About-with-implicits
          "c" #'coq-Check-show-implicits
          "i" #'coq-Print-with-implicits))
      (:prefix ("g" . "goto")
        "e" #'proof-goto-command-end
        "l" #'proof-goto-end-of-locked
        "s" #'proof-goto-command-start)
      (:prefix ("i" . "insert")
        "c" #'coq-insert-command
        "e" #'coq-end-Section
        "i" #'coq-insert-intros
        "r" #'coq-insert-requires
        "s" #'coq-insert-section-or-module
        "t" #'coq-insert-tactic
        "T" #'coq-insert-tactical))


;; This package provides more than just code completion, so we load it whether
;; or not :completion company is enabled.
(use-package! company-coq
  :hook (coq-mode . company-coq-mode)
  :config
  (set-popup-rule! "^\\*\\(?:response\\|goals\\)\\*" :ignore t)
  (set-lookup-handlers! 'company-coq-mode
    :definition #'company-coq-jump-to-definition
    :references #'company-coq-grep-symbol
    :documentation #'company-coq-doc)

  (setq company-coq-disabled-features '(hello company company-defaults spinner))

  (cond ((modulep! :completion corfu)
         ;; HACK: company-coq activates `company-mode', though it's not really
         ;;   needed when we're relying on Corfu, hence these hacks:
         (add-hook! 'coq-mode-local-vars-hook
           (defun +coq-init-capf-completion-h ()
             (dolist (b '(company-coq-master-backend company-coq-math-symbols-backend))
               (add-to-list 'completion-at-point-functions (cape-company-to-capf b)))))
         (defadvice! +coq--proof-goto-point-advice (&rest _)
           :override #'company-coq--proof-goto-point-advice
           (when (bound-and-true-p company-candidates)
             (company-abort))))

        ;; DEPRECATED: The company module is deprecated.
        ((modulep! :completion company)
         (define-key coq-mode-map [remap company-complete-common]
                     #'company-indent-or-complete-common)))

  ;; HACK: Doom treats the use of package.el and its API as user error unless
  ;;   they've called `package-initialize' themselves (in which case, it is
  ;;   assumed you know what you're doing).
  (defadvice! +coq--noop-upgrade-elpa-packages-a (fn &rest args)
    :override #'proof-upgrade-elpa-packages
    (if (and (featurep 'package) package--initialized)
        (apply fn args)
      (user-error "Doom doesn't support this command (update packages through `package!' statements!)")))

  (map! :map coq-mode-map
        :localleader
        "ao" #'company-coq-occur
        (:prefix "i"
          "l" #'company-coq-lemma-from-goal
          "m" #'company-coq-insert-match-construct)
        (:prefix ("h" . "help")
          "e" #'company-coq-document-error
          "E" #'company-coq-browse-error-messages
          "h" #'company-coq-doc)))
