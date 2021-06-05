;;; editor/snippets/config.el -*- lexical-binding: t; -*-

(defvar +snippets-dir (expand-file-name "snippets/" doom-private-dir)
  "Directory where `yasnippet' will search for your private snippets.")


;;
;;; Packages

(use-package! yasnippet
  :defer-incrementally eldoc easymenu help-mode
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode
             yas-maybe-expand-abbrev-key-filter)
  :init
  ;; Remove default ~/.emacs.d/snippets
  (defvar yas-snippet-dirs nil)

  ;; Lazy load yasnippet until it is needed
  (add-transient-hook! #'company-yasnippet (require 'yasnippet))

  :config
  (add-to-list 'doom-debug-variables '(yas-verbosity . 3))

  ;; Allow private snippets in DOOMDIR/snippets
  (add-to-list 'yas-snippet-dirs '+snippets-dir)

  ;; Reduce verbosity. 3 is too chatty about initializing yasnippet. 2 is just
  ;; right (only shows errors).
  (setq yas-verbosity (if doom-debug-p 3 0))

  ;; default snippets library, if available
  (add-to-list 'load-path +snippets-dir)
  (require 'doom-snippets nil t)

  ;; HACK In case `+snippets-dir' and `doom-snippets-dir' are the same, or
  ;;      duplicates exist in `yas-snippet-dirs'.
  (advice-add #'yas-snippet-dirs :filter-return #'delete-dups)

  ;; Remove GUI dropdown prompt (prefer ivy/helm)
  (delq! 'yas-dropdown-prompt yas-prompt-functions)
  ;; Prioritize private snippets in `+snippets-dir' over built-in ones if there
  ;; are multiple choices.
  (add-to-list 'yas-prompt-functions #'+snippets-prompt-private)

  ;; Register `def-project-mode!' modes with yasnippet. This enables project
  ;; specific snippet libraries (e.g. for Laravel, React or Jekyll projects).
  (add-hook 'doom-project-hook #'+snippets-enable-project-modes-h)

  ;; Exit snippets on ESC from normal mode
  (add-hook 'doom-escape-hook #'yas-abort-snippet)

  (after! smartparens
    ;; tell smartparens overlays not to interfere with yasnippet keybinds
    (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay))

  ;; (Evil only) fix off-by-one issue with line-wise visual selections in
  ;; `yas-insert-snippet', and switches to insert mode afterwards.
  (advice-add #'yas-insert-snippet :around #'+snippets-expand-on-region-a)

  ;; Show keybind hints in snippet header-line
  (add-hook 'snippet-mode-hook #'+snippets-show-hints-in-header-line-h)
  ;; Enable `read-only-mode' for built-in snippets (in `doom-local-dir')
  (add-hook 'snippet-mode-hook #'+snippets-read-only-maybe-h)

  (map! (:map yas-keymap
         "C-e"         #'+snippets/goto-end-of-field
         "C-a"         #'+snippets/goto-start-of-field
         [M-right]     #'+snippets/goto-end-of-field
         [M-left]      #'+snippets/goto-start-of-field
         [M-backspace] #'+snippets/delete-to-start-of-field
         [backspace]   #'+snippets/delete-backward-char
         [delete]      #'+snippets/delete-forward-char-or-field
         ;; Replace commands with superior alternatives
         :map yas-minor-mode-map
         [remap yas-new-snippet]        #'+snippets/new
         [remap yas-visit-snippet-file] #'+snippets/edit)
        (:map snippet-mode-map
         "C-c C-k" #'+snippet--abort))

  ;; REVIEW Fix #2639: For some reason `yas--all-templates' returns duplicates
  ;;        of some templates. Until I figure out the real cause this fixes it.
  (defadvice! +snippets--remove-duplicates-a (templates)
    :filter-return #'yas--all-templates
    (cl-delete-duplicates templates :test #'equal))

  ;; HACK Smartparens will interfere with snippets expanded by `hippie-expand`,
  ;;      so temporarily disable smartparens during snippet expansion.
  (after! hippie-exp
    (defvar +snippets--smartparens-enabled-p t)
    (defvar +snippets--expanding-p nil)

    ;; Is called for all snippet expansions,
    (add-hook! 'yas-before-expand-snippet-hook
      (defun +snippets--disable-smartparens-before-expand-h ()
        ;; Remember the initial smartparens state only once, when expanding a
        ;; top-level snippet.
        (unless +snippets--expanding-p
          (setq +snippets--expanding-p t
                +snippets--smartparens-enabled-p smartparens-mode))
        (when smartparens-mode
          (smartparens-mode -1))))

    ;; Is called only for the top level snippet, but not for the nested ones.
    ;; Hence `+snippets--expanding-p'.
    (add-hook! 'yas-after-exit-snippet-hook
      (defun +snippets--restore-smartparens-after-expand-h ()
        (setq +snippets--expanding-p nil)
        (when +snippets--smartparens-enabled-p
          (smartparens-mode 1)))))

  ;; If in a daemon session, front-load this expensive work:
  (yas-global-mode +1))


(use-package! auto-yasnippet
  :defer t
  :config
  (setq aya-persist-snippets-dir +snippets-dir)
  (defadvice! +snippets--inhibit-yas-global-mode-a (orig-fn &rest args)
    "auto-yasnippet enables `yas-global-mode'. This is obnoxious for folks like
us who use yas-minor-mode and enable yasnippet more selectively. This advice
swaps `yas-global-mode' with `yas-minor-mode'."
    :around '(aya-expand aya-open-line)
    (letf! ((#'yas-global-mode #'yas-minor-mode)
            (yas-global-mode yas-minor-mode))
      (apply orig-fn args))))
