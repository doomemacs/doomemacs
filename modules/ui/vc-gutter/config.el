;;; ui/vc-gutter/config.el -*- lexical-binding: t; -*-

;; TODO Implement me
(defvar +vc-gutter-in-margin nil
  "If non-nil, use the margin for diffs instead of the fringe.")

(defvar +vc-gutter-in-remote-files nil
  "If non-nil, enable the vc gutter in remote files (e.g. open through TRAMP).")


;;
;;; Default styles

(when (featurep! +pretty)
  ;; UI: make the fringe small enough that the diff bars aren't too domineering,
  ;; while leaving enough room for other indicators.
  (if (fboundp 'fringe-mode) (fringe-mode '8))
  ;; UI: the gutter looks less cramped with some space between it and  buffer.
  (setq-default fringes-outside-margins t)

  ;; STYLE: Redefine fringe bitmaps to take up only half the horizontal space in
  ;; the fringe. This way we avoid overbearingly large diff bars without having
  ;; to shrink the fringe and sacrifice precious space for other fringe
  ;; indicators (like flycheck or flyspell).
  ;; TODO Extract these into a package with faces that themes can target.
  (if (not (featurep! +diff-hl))
      (after! git-gutter-fringe
        (define-fringe-bitmap 'git-gutter-fr:added [224]
          nil nil '(center repeated))
        (define-fringe-bitmap 'git-gutter-fr:modified [224]
          nil nil '(center repeated))
        (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
          nil nil 'bottom))
    (defadvice! +vc-gutter-define-thin-bitmaps-a (&rest args)
      :override #'diff-hl-define-bitmaps
      (set-face-background 'diff-hl-insert nil)
      (set-face-background 'diff-hl-delete nil)
      (set-face-background 'diff-hl-change nil)
      (define-fringe-bitmap 'diff-hl-bmp-middle [224] nil nil '(center repeated))
      (define-fringe-bitmap 'diff-hl-bmp-delete [240 224 192 128] nil nil 'top))
    (defun +vc-gutter-type-face-fn (type _pos)
      (intern (format "diff-hl-%s" type)))
    (defun +vc-gutter-type-at-pos-fn (type _pos)
      (if (eq type 'delete)
          'diff-hl-bmp-delete
        'diff-hl-bmp-middle))
    (setq diff-hl-fringe-bmp-function #'+vc-gutter-type-at-pos-fn
          diff-hl-draw-borders nil))

  ;; FIX: To minimize overlap between flycheck indicators and git-gutter/diff-hl
  ;; indicators in the left fringe.
  (after! flycheck
    ;; let diff have left fringe, flycheck can have right fringe
    (setq flycheck-indication-mode 'right-fringe)
    ;; A non-descript, left-pointing arrow
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)))


;;
;;; git-gutter

(use-package! git-gutter
  :unless (featurep! +diff-hl)
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


;;
;;; diff-hl

(use-package! diff-hl
  :when (featurep! +diff-hl)
  :hook (doom-first-file . global-diff-hl-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :config
  (set-popup-rule! "^\\*diff-hl" :select nil :size '+popup-shrink-to-fit)

  ;; PERF: reduce load on remote
  (defvaralias 'diff-hl-disable-on-remote '+vc-gutter-in-remote-files)
  ;; PERF: A slightly faster algorithm for diffing.
  (setq vc-git-diff-switches '("--histogram"))
  ;; PERF: Slightly more conservative delay before updating the diff
  (setq diff-hl-flydiff-delay 0.5)  ; default: 0.3

  ;; UX: get realtime feedback in diffs after staging/unstaging hunks.
  (setq diff-hl-show-staged-changes nil)

  ;; UX: Update diffs when it makes sense too, without being too slow
  (when (featurep! :editor evil)
    (map! :after diff-hl-show-hunk
          :map diff-hl-show-hunk-map
          :n "p" #'diff-hl-show-hunk-previous
          :n "n" #'diff-hl-show-hunk-next
          :n "c" #'diff-hl-show-hunk-copy-original-text
          :n "r" #'diff-hl-show-hunk-revert-hunk
          :n "[" #'diff-hl-show-hunk-previous
          :n "]" #'diff-hl-show-hunk-next
          :n "{" #'diff-hl-show-hunk-previous
          :n "}" #'diff-hl-show-hunk-next
          :n "S" #'diff-hl-show-hunk-stage-hunk))
  ;; UX: Refresh git-gutter on ESC or refocusing the Emacs frame.
  (add-hook! '(doom-escape-hook doom-switch-window-hook) :append
    (defun +vc-gutter-update-h (&rest _)
      "Return nil to prevent shadowing other `doom-escape-hook' hooks."
      (ignore (or inhibit-redisplay
                  (and (or (bound-and-true-p diff-hl-mode)
                           (bound-and-true-p diff-hl-dir-mode))
                       (diff-hl-update-once))))))
  ;; UX: Update diff-hl when magit alters git state.
  (when (featurep! :tools magit)
    (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
    (add-hook 'magit-post-stage-hook   #'+vc-gutter-update-h)
    (add-hook 'magit-post-unstage-hook #'+vc-gutter-update-h))

  ;; UX: Don't delete the current hunk's indicators while we're editing
  (add-hook! 'diff-hl-flydiff-mode-hook
    (defun +vc-gutter-init-flydiff-mode-h ()
      (if diff-hl-flydiff-mode
          (progn
            (advice-remove #'diff-hl-overlay-modified #'ignore)
            (when (featurep! :editor evil)
              (add-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)))
        (remove-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update))))

  ;; FIX: Reverting a hunk causes the cursor to be moved to an unexpected place,
  ;; often far from the target hunk.
  (defadvice! +vc-gutter--save-excursion-a (fn &rest args)
    "Suppresses unexpected cursor movement by `diff-hl-revert-hunk'."
    :around #'diff-hl-revert-hunk
    (let ((pt (point)))
      (prog1 (apply fn args)
        (goto-char pt)))))
