;;; lang/agda/config.el -*- lexical-binding: t; -*-

(defvar +agda-dir
  (when (executable-find "agda-mode")
    (file-name-directory (shell-command-to-string "agda-mode locate"))))

(def-package! agda-input
  :load-path +agda-dir)

(def-package! agda2-mode
  :mode "\\.agda\\'"
  :after agda-input
  :init
  ;; fix syntax highlighting
  ;; (taken from https://github.com/syl20bnr/spacemacs/blob/develop/layers/%2Blang/agda/packages.el)
   (progn
        (mapc
         (lambda (x) (add-to-list 'face-remapping-alist x))
         '((agda2-highlight-datatype-face              . font-lock-type-face)
           (agda2-highlight-function-face              . font-lock-type-face)
           (agda2-highlight-inductive-constructor-face . font-lock-function-name-face)
           (agda2-highlight-keyword-face               . font-lock-keyword-face)
           (agda2-highlight-module-face                . font-lock-constant-face)
           (agda2-highlight-number-face                . nil)
           (agda2-highlight-postulate-face             . font-lock-type-face)
           (agda2-highlight-primitive-type-face        . font-lock-type-face)
           (agda2-highlight-record-face                . font-lock-type-face))))
  :config
  (map! :map agda2-mode-map
                :localleader
                :n "l" #'agda2-load
                :n "d" #'agda2-infer-type-maybe-toplevel
                :n "o" #'agda2-module-contents-maybe-toplevel
                :n "n" #'agda2-compute-normalised-maybe-toplevel))
  ;; TODO finish keybindings
