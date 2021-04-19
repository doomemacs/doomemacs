;;; ui/vc-gutter/config.el -*- lexical-binding: t; -*-

(defvar +vc-gutter-in-margin nil
  "If non-nil, use the margin for diffs instead of the fringe.")

(defvar +vc-gutter-in-remote-files nil
  "If non-nil, enable the vc gutter in remote files (e.g. open through TRAMP).")

(defvar +vc-gutter-diff-unsaved-buffer nil
  "If non-nil, `diff-hl-flydiff-mode' will be activated. This allows on-the-fly
diffing, even for unsaved buffers.")

(defvar +vc-gutter-default-style t
  "If non-nil, enable the default look of the vc gutter.
This means subtle thin bitmaps on the left, an arrow bitmap for flycheck, and
flycheck indicators moved to the right fringe.")


;;
;;; Packages

(use-package! git-gutter
  :commands git-gutter:revert-hunk git-gutter:stage-hunk
  :init
  (add-hook! 'find-file-hook
    (defun +vc-gutter-init-maybe-h ()
      "Enable `git-gutter-mode' in the current buffer.
If the buffer doesn't represent an existing file, `git-gutter-mode's activation
is deferred until the file is saved. Respects `git-gutter:disabled-modes'."
      (let ((file-name (buffer-file-name (buffer-base-buffer))))
        (cond
         ((and (file-remote-p (or file-name default-directory))
               (not +vc-gutter-in-remote-files)))
         ;; If not a valid file, wait until it is written/saved to activate
         ;; git-gutter.
         ((not (and file-name (vc-backend file-name)))
          (add-hook 'after-save-hook #'+vc-gutter-init-maybe-h nil 'local))
         ;; Allow git-gutter or git-gutter-fringe to activate based on the type
         ;; of frame we're in. This allows git-gutter to work for silly geese
         ;; who open both tty and gui frames from the daemon.
         ((if (and (display-graphic-p)
                   (require 'git-gutter-fringe nil t))
              (setq-local git-gutter:init-function      #'git-gutter-fr:init
                          git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
                          git-gutter:clear-function     #'git-gutter-fr:clear
                          git-gutter:window-width -1)
            (setq-local git-gutter:init-function      'nil
                        git-gutter:view-diff-function #'git-gutter:view-diff-infos
                        git-gutter:clear-function     #'git-gutter:clear-diff-infos
                        git-gutter:window-width 1))
          (unless (memq major-mode git-gutter:disabled-modes)
            (git-gutter-mode +1)
            (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local)))))))

  ;; Disable in Org mode, as per syl20bnr/spacemacs#10555 and
  ;; syohex/emacs-git-gutter#24. Apparently, the mode-enabling function for
  ;; global minor modes gets called for new buffers while they are still in
  ;; `fundamental-mode', before a major mode has been assigned. I don't know why
  ;; this is the case, but adding `fundamental-mode' here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  :config
  (set-popup-rule! "^\\*git-gutter" :select nil :size '+popup-shrink-to-fit)

  ;; Only enable the backends that are available, so it doesn't have to check
  ;; when opening each buffer.
  (setq git-gutter:handled-backends
        (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                     :key #'symbol-name)))

  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (add-hook! '(doom-escape-hook doom-switch-window-hook) :append
    (defun +vc-gutter-update-h (&rest _)
      "Refresh git-gutter on ESC. Return nil to prevent shadowing other
`doom-escape-hook' hooks."
      (ignore (or (memq this-command '(git-gutter:stage-hunk
                                       git-gutter:revert-hunk))
                  inhibit-redisplay
                  (if git-gutter-mode
                      (git-gutter)
                    (+vc-gutter-init-maybe-h))))))
  ;; update git-gutter when using magit commands
  (advice-add #'magit-stage-file   :after #'+vc-gutter-update-h)
  (advice-add #'magit-unstage-file :after #'+vc-gutter-update-h)

  (defadvice! +vc-gutter--fix-linearity-of-hunks-a (diffinfos is-reverse)
    "Fixes `git-gutter:next-hunk' and `git-gutter:previous-hunk' sometimes
  jumping to random hunks."
    :override #'git-gutter:search-near-diff-index
    (cl-position-if (let ((lineno (line-number-at-pos))
                          (fn (if is-reverse #'> #'<)))
                      (lambda (line) (funcall fn lineno line)))
                    diffinfos
                    :key #'git-gutter-hunk-start-line
                    :from-end is-reverse)))


;; subtle diff indicators in the fringe
(after! git-gutter-fringe
  (when +vc-gutter-default-style
    ;; standardize default fringe width
    (if (fboundp 'fringe-mode) (fringe-mode '4))

    ;; places the git gutter outside the margins.
    (setq-default fringes-outside-margins t)
    ;; thin fringe bitmaps
    (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
      nil nil 'bottom)))

(after! flycheck
  (when +vc-gutter-default-style
    ;; let diff have left fringe, flycheck can have right fringe
    (setq flycheck-indication-mode 'right-fringe)
    ;; A non-descript, left-pointing arrow
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)))
