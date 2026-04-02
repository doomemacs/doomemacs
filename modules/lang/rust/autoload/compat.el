;;; lang/rust/autoload/compat.el -*- lexical-binding: t; -*-

;; `rust-mode' clumsily stacks rust-mode onto `rust-ts-mode' or `prog-mode'
;; depending on treesit's presence (and `rust-mode-treesitter-derive', and
;; `rustic' tries to stack *its* cludges on top of that, creating a brittle
;; jenga tower that can overwrite (or be overwritten by) Doom's defaults or the
;; user's config. I undo most of the damage here and consolidate Rust support
;; under `rustic-mode' so we don't have to juggle three major modes.
;;
;; This must be done in autoloads so it is evaluated before the user, packages,
;; or Doom has a chance to break it.

;;;###autoload
(progn
  ;; HACK: Ensure `rust-mode' derives from `rust-ts-mode', b/c `rustic-mode'
  ;;   derives from rust-mode. This way, rustic-mode is the only major mode we
  ;;   have to worry about for Rust support in Emacs.
  (setq rust-mode-treesitter-derive (modulep! :lang rust +tree-sitter))

  ;; HACK: Prevent `auto-mode-alist' conflicts between rust-mode, rust-ts-mode,
  ;;   and rustic, caused by their own cludgy attempts to do the same.
  (cl-callf2 rassq-delete-all 'rust-mode auto-mode-alist)
  (cl-callf2 rassq-delete-all 'rustic-mode auto-mode-alist)

  ;; HACK: Emacs 31's built-in `rust-ts-mode' calls
  ;;   (derived-mode-add-parents 'rust-ts-mode '(rust-mode))
  ;;   so that hooks on `rust-mode' also fire in `rust-ts-mode' buffers. But
  ;;   when `rust-mode-treesitter-derive' is non-nil, `rust-mode' derives FROM
  ;;   `rust-ts-mode', creating a cycle in `derived-mode-all-parents':
  ;;     rust-mode → rust-ts-mode → rust-mode
  ;;   Remove the extra parent to break the cycle. The derive direction already
  ;;   ensures `rust-mode' hooks fire (since `rust-mode' IS a `rust-ts-mode').
  (when (and rust-mode-treesitter-derive
             (>= emacs-major-version 31))
    (after! rust-ts-mode
      (put 'rust-ts-mode 'derived-mode-extra-parents
           (remq 'rust-mode (get 'rust-ts-mode 'derived-mode-extra-parents)))
      (put 'rust-ts-mode 'derived-mode--all-parents nil)
      (put 'rust-mode 'derived-mode--all-parents nil)))

  ;; HACK: If rustic isn't loaded *after* `rust-mode' is defined, chaos and
  ;;   errors can ensue, but `rust-mode' is defined *after* it `provide's the
  ;;   `rust-mode' package, so (after! rust-mode ...) isn't sufficient. Sigh.
  (after! (:or rust-prog-mode rust-mode-treesitter)
    (let (auto-mode-alist)
      (require 'rustic nil t))))
