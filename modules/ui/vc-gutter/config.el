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
    "Enable `git-gutter-mode' in non-remote buffers."
    (when (and buffer-file-name
               (vc-backend buffer-file-name)
               (or +vc-gutter-in-remote-files
                   (not (file-remote-p buffer-file-name))))
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
      (git-gutter-mode +1)))
  (add-hook! (text-mode prog-mode conf-mode after-save)
    #'+version-control|git-gutter-maybe)
  ;; standardize default fringe width
  (if (fboundp 'fringe-mode) (fringe-mode '4))
  :config
  (set-popup-rule! "^\\*git-gutter" :select nil)

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


;; (def-package! diff-hl
;;   :defer t
;;   :init
;;   (defun +vc-gutter|init ()
;;     "Start `diff-hl-mode' if in a file-visiting and tracked buffer."
;;     (when (and buffer-file-name
;;                (vc-state buffer-file-name)
;;                (or +vc-gutter-in-remote-files
;;                    (not (file-remote-p buffer-file-name))))
;;       (diff-hl-mode +1)))
;;   (add-hook! (text-mode prog-mode conf-mode after-save)
;;     #'+vc-gutter|init)
;;   ;; standardize fringe size
;;   (if (fboundp 'fringe-mode) (fringe-mode '4))
;;   :config
;;   (setq vc-git-diff-switches '("--histogram"))
;;   ;; Update diffs when it makes sense too, without being too slow
;;   (if (not +vc-gutter-diff-unsaved-buffer)
;;       (add-hook! '(doom-escape-hook focus-in-hook) #'diff-hl-update)
;;     (diff-hl-flydiff-mode +1)
;;     (add-hook! '(doom-escape-hook focus-in-hook) #'diff-hl-flydiff-update)
;;     (when (featurep! :feature evil)
;;       (when diff-hl-flydiff-timer
;;         (cancel-timer diff-hl-flydiff-timer))
;;       (add-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)))
;;   ;; Don't delete the current hunk's indicators while we're editing
;;   (advice-remove #'diff-hl-overlay-modified #'ignore)
;;   ;; Update diff-hl when magit refreshes
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
;;   ;; update git-gutter when using these commands
;;   (advice-add #'magit-stage :after #'+version-control|update-git-gutter)
;;   (advice-add #'magit-unstage :after #'+version-control|update-git-gutter)
;;   (advice-add #'magit-stage-file :after #'+version-control|update-git-gutter)
;;   (advice-add #'magit-unstage-file :after #'+version-control|update-git-gutter)
;;   ;; Draw me like one of your French editors
;;   (setq-default fringes-outside-margins t)
;;   (cond ((or +vc-gutter-in-margin (not (display-graphic-p)))
;;          (diff-hl-margin-mode)
;;          (setq diff-hl-margin-symbols-alist
;;                '((insert . "❙") (delete . "^") (change . "❙")
;;                  (unknown . "❙") (ignored . "❙"))))
;;         (t
;;          ;; Because diff-hl is in the left fringe
;;          (setq flycheck-indication-mode 'right-fringe)
;;          (defun +vc-gutter|setup-fringe-bitmaps ()
;;            "Define thin fringe bitmaps for maximum sexiness."
;;            (define-fringe-bitmap 'diff-hl-bmp-top [224] nil nil '(center repeated))
;;            (define-fringe-bitmap 'diff-hl-bmp-middle [224] nil nil '(center repeated))
;;            (define-fringe-bitmap 'diff-hl-bmp-bottom [224] nil nil '(center repeated))
;;            (define-fringe-bitmap 'diff-hl-bmp-insert [224] nil nil '(center repeated))
;;            (define-fringe-bitmap 'diff-hl-bmp-single [224] nil nil '(center repeated))
;;            (define-fringe-bitmap 'diff-hl-bmp-delete [240 224 192 128] nil nil 'top))
;;          (defun +vc-gutter-type-at-pos (type _pos)
;;            "Return the bitmap for `diff-hl' to use for change at point."
;;            (pcase type
;;              (`unknown 'question-mark)
;;              (`delete  'diff-hl-bmp-delete)
;;              (`change  'diff-hl-bmp-middle)
;;              (`ignored 'diff-hl-bmp-i)
;;              (x (intern (format "diff-hl-bmp-%s" x)))))
;;          ;; Tweak the fringe bitmaps so we get long, elegant bars
;;          (setq diff-hl-fringe-bmp-function #'+vc-gutter-type-at-pos
;;                diff-hl-draw-borders nil)
;;          (add-hook 'diff-hl-mode-hook #'+vc-gutter|setup-fringe-bitmaps))))

