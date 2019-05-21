;;; ui/vc-gutter/config.el -*- lexical-binding: t; -*-

(defvar +vc-gutter-in-margin nil
  "If non-nil, use the margin for diffs instead of the fringe.")

(defvar +vc-gutter-in-remote-files nil
  "If non-nil, enable the vc gutter in remote files (e.g. open through TRAMP).")

(defvar +vc-gutter-diff-unsaved-buffer nil
  "If non-nil, `diff-hl-flydiff-mode' will be activated. This allows on-the-fly
diffing, even for unsaved buffers.")

(defvar +vc-gutter-default-style t
  "If non-nil, enable the default look of the vc gutter. This means subtle thin
bitmaps on the left, an arrow bitmap for flycheck, and flycheck indicators moved
to the right fringe.")


;;
;; Packages

(def-package! git-gutter
  :commands (git-gutter:revert-hunk git-gutter:stage-hunk)
  :init
  (defun +version-control|git-gutter-maybe ()
    "Enable `git-gutter-mode' in the current buffer.

If the buffer doesn't represent an existing file, `git-gutter-mode's activation
is deferred until the file is saved."
    (when (or +vc-gutter-in-remote-files
              (not (file-remote-p (or buffer-file-name default-directory))))
      (if (not buffer-file-name)
          (add-hook 'after-save-hook #'+version-control|git-gutter-maybe nil t)
        (when (vc-backend buffer-file-name)
          (if (display-graphic-p)
              (progn
                (require 'git-gutter-fringe)
                (setq-local git-gutter:init-function      #'git-gutter-fr:init)
                (setq-local git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos)
                (setq-local git-gutter:clear-function     #'git-gutter-fr:clear)
                (setq-local git-gutter:window-width -1))
            (setq-local git-gutter:init-function      'nil)
            (setq-local git-gutter:view-diff-function #'git-gutter:view-diff-infos)
            (setq-local git-gutter:clear-function     #'git-gutter:clear-diff-infos)
            (setq-local git-gutter:window-width 1))
          (git-gutter-mode +1)
          (remove-hook 'after-save-hook #'+version-control|git-gutter-maybe t)))))
  (add-hook! (text-mode prog-mode conf-mode)
    #'+version-control|git-gutter-maybe)
  ;; standardize default fringe width
  (if (fboundp 'fringe-mode) (fringe-mode '4))
  :config
  (set-popup-rule! "^\\*git-gutter" :select nil :size '+popup-shrink-to-fit)

  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (defun +version-control|update-git-gutter (&rest _)
    "Refresh git-gutter on ESC. Return nil to prevent shadowing other
`doom-escape-hook' hooks."
    (when git-gutter-mode
      (ignore (git-gutter))))
  (add-hook 'doom-escape-hook #'+version-control|update-git-gutter t)

  ;; update git-gutter when using magit commands
  (advice-add #'magit-stage-file   :after #'+version-control|update-git-gutter)
  (advice-add #'magit-unstage-file :after #'+version-control|update-git-gutter))


;; subtle diff indicators in the fringe
(when +vc-gutter-default-style
  (after! git-gutter-fringe
    ;; places the git gutter outside the margins.
    (setq-default fringes-outside-margins t)
    ;; thin fringe bitmaps
    (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
      nil nil 'bottom)
    ;; let diff have left fringe, flycheck can have right fringe
    (after! flycheck
      (setq flycheck-indication-mode 'right-fringe)
      ;; A non-descript, left-pointing arrow
      (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
        [16 48 112 240 112 48 16] nil nil 'center))))
