;;; ui/vc-gutter/config.el -*- lexical-binding: t; -*-

;;
;;; Default styles

;; STYLE: Redefine fringe bitmaps to be sleeker by making them solid bars (with
;;   no border) that only take up half the horizontal space in the fringe. This
;;   approach lets us avoid robbing fringe space from other packages/modes that
;;   may need benefit from it (like magit, flycheck, or flyspell).
(when (modulep! +pretty)
  (if (fboundp 'fringe-mode) (fringe-mode '8))
  (setq-default fringes-outside-margins t)

  (defadvice! +vc-gutter-define-thin-bitmaps-a (&rest _)
    :after #'diff-hl-define-bitmaps
    (let* ((scale (if (and (boundp 'text-scale-mode-amount)
                           (numberp text-scale-mode-amount))
                      (expt text-scale-mode-step text-scale-mode-amount)
                    1))
           (spacing (or (and (display-graphic-p) (default-value 'line-spacing)) 0))
           (h (+ (ceiling (* (frame-char-height) scale))
                 (if (floatp spacing)
                     (truncate (* (frame-char-height) spacing))
                   spacing)))
           (w (min (frame-parameter nil (intern (format "%s-fringe" diff-hl-side)))
                   diff-hl-bmp-max-width))
           (_ (if (zerop w) (setq w diff-hl-bmp-max-width))))
      (define-fringe-bitmap 'diff-hl-bmp-middle
        (make-vector
         h (string-to-number (let ((half-w (1- (/ w 2))))
                               (concat (make-string half-w ?1)
                                       (make-string (- w half-w) ?0)))
                             2))
        nil nil 'center)))
  (defun +vc-gutter-type-at-pos-fn (type _pos)
    (if (eq type 'delete)
        'diff-hl-bmp-delete
      'diff-hl-bmp-middle))
  (setq diff-hl-fringe-bmp-function #'+vc-gutter-type-at-pos-fn)
  (setq diff-hl-draw-borders nil)

  (add-hook! 'diff-hl-mode-hook
    (defun +vc-gutter-make-diff-hl-faces-transparent-h ()
      (mapc (doom-rpartial #'set-face-background nil)
            '(diff-hl-insert
              diff-hl-delete
              diff-hl-change))))

  ;; FIX: To minimize overlap between flycheck indicators and diff-hl indicators
  ;;   in the left fringe.
  (after! flycheck
    ;; Let diff-hl have left fringe, flycheck can have right fringe
    (setq flycheck-indication-mode 'right-fringe)
    ;; A non-descript, left-pointing arrow
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)))


;;
;;; diff-hl

(use-package! diff-hl
  :hook (doom-first-file . global-diff-hl-mode)
  :hook (vc-dir-mode . turn-on-diff-hl-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :commands diff-hl-stage-current-hunk diff-hl-revert-hunk diff-hl-next-hunk diff-hl-previous-hunk
  :init
  (add-hook! 'dired-mode-hook
    (defun +vc-gutter-enable-maybe-h ()
      "Conditionally enable `diff-hl-dired-mode' in dired buffers.
Respects `diff-hl-disable-on-remote'."
      ;; Neither `diff-hl-dired-mode' or `diff-hl-dired-mode-unless-remote'
      ;; respect `diff-hl-disable-on-remote', so...
      (unless (and (bound-and-true-p diff-hl-disable-on-remote)
                   (file-remote-p default-directory))
        (diff-hl-dired-mode +1))))

  ;; HACK: diff-hl won't be visible in TTY frames, but there's no simple way to
  ;;   use the fringe in GUI Emacs *and* use the margin in the terminal *AND*
  ;;   support daemon users, so we need more than a static `display-graphic-p'
  ;;   check at startup.
  (if (not (daemonp))
      (unless (display-graphic-p)
        (add-hook 'global-diff-hl-mode-hook #'diff-hl-margin-mode))
    (when (modulep! :os tty)
      (put 'diff-hl-mode 'last t)
      (add-hook! 'doom-switch-window-hook
        (defun +vc-gutter-use-margins-in-tty-h ()
          (when (bound-and-true-p global-diff-hl-mode)
            (let ((graphic? (display-graphic-p)))
              (unless (eq (get 'diff-hl-mode 'last) graphic?)
                (diff-hl-margin-mode (if graphic? -1 +1))
                (put 'diff-hl-mode 'last graphic?))))))))

  :config
  (set-popup-rule! "^\\*diff-hl" :select nil)

  (setq diff-hl-global-modes '(not image-mode pdf-view-mode))
  ;; PERF: A slightly faster algorithm for diffing.
  (setq vc-git-diff-switches '("--histogram"))
  ;; PERF: Slightly more conservative delay before updating the diff
  (setq diff-hl-flydiff-delay 0.5)  ; default: 0.3
  ;; PERF: don't block Emacs when updating vc gutter
  (setq diff-hl-update-async t)
  ;; UX: get realtime feedback in diffs after staging/unstaging hunks.
  (setq diff-hl-show-staged-changes nil)

  ;; UX: Update diffs when it makes sense too, without being too slow
  (when (modulep! :editor evil)
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
  ;; UX: Refresh gutter in the selected buffer on ESC, switching windows, or
  ;;   refocusing the frame.
  (add-hook! '(doom-escape-hook doom-switch-window-hook doom-switch-frame-hook) :append
    (defun +vc-gutter-update-h (&rest _)
      "Return nil to prevent shadowing other `doom-escape-hook' hooks."
      (ignore (and (or (bound-and-true-p diff-hl-mode)
                       (bound-and-true-p diff-hl-dir-mode))
                   (diff-hl-update-once)))))
  ;; UX: Update diff-hl when magit alters git state.
  (when (modulep! :tools magit)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; FIX: The revert popup consumes 50% of the frame, whether or not you're
  ;;   reverting 2 lines or 20. This resizes the popup to match its contents.
  (defadvice! +vc-gutter--shrink-popup-a (fn &rest args)
    :around #'diff-hl-revert-hunk-1
    (letf! ((refine-mode diff-auto-refine-mode)
            (diff-auto-refine-mode t)
            (defun diff-refine-hunk ()
              (when refine-mode
                (funcall diff-refine-hunk))
              (shrink-window-if-larger-than-buffer)))
      (apply fn args)))

  ;; UX: Don't delete the current hunk's indicators while we're editing
  (when (modulep! :editor evil)
    (add-hook! 'diff-hl-flydiff-mode-hook
      (defun +vc-gutter-init-flydiff-mode-h ()
        (if diff-hl-flydiff-mode
            (add-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)
          (remove-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)))))

  ;; FIX: Reverting a hunk causes the cursor to be moved to an unexpected place,
  ;;   often far from the target hunk.
  (defadvice! +vc-gutter--save-excursion-a (fn &rest args)
    "Suppresses unexpected cursor movement by `diff-hl-revert-hunk'."
    :around #'diff-hl-revert-hunk
    (let ((pt (point)))
      (prog1 (apply fn args)
        (goto-char pt))))

  ;; FIX: `global-diff-hl-mode' enables `diff-hl-mode' *everywhere*, which calls
  ;;   `diff-hl-update'. If `diff-hl-update-async' is non-nil, this means a new
  ;;   thread is spawned for *every* buffer, whether they're visible or not. Not
  ;;   only can this slow a lot down, but `kill-buffer' will silently refuse to
  ;;   kill buffers with a thread associated with it. Chaos ensues (see #7991
  ;;   and #7954).
  ;; REVIEW: Report this upstream.
  (defun +vc-gutter--kill-thread (&optional block?)
    (when-let ((th +vc-gutter--diff-hl-thread))
      (when (thread-live-p th)
        (thread-signal th 'quit nil)
        (when block?
          (condition-case _
              (thread-join th)
            ((quit error) nil))))))

  (defvar-local +vc-gutter--diff-hl-thread nil)
  (defadvice! +vc-gutter--debounce-threads-a (&rest _)
    :override #'diff-hl-update
    (unless (or inhibit-redisplay
                non-essential
                delay-mode-hooks
                (null (buffer-file-name (buffer-base-buffer)))
                (null (get-buffer-window (current-buffer))))
      (if (and diff-hl-update-async
               (not
                (run-hook-with-args-until-success 'diff-hl-async-inhibit-functions
                                                  default-directory)))
          (progn
            (+vc-gutter--kill-thread)
            (setq +vc-gutter--diff-hl-thread
                  (make-thread (lambda ()
                                 (unwind-protect
                                     (diff-hl--update-safe)
                                   (setq +vc-gutter--diff-hl-thread nil)))
                               "diff-hl--update-safe")))
        (diff-hl--update))
      t))

  (defadvice! +vc-gutter--only-tick-on-success-a (&rest _)
    :override #'diff-hl-update-once
    (unless (equal diff-hl--modified-tick (buffer-chars-modified-tick))
      (when (diff-hl-update)
        (setq diff-hl--modified-tick (buffer-chars-modified-tick)))))

  ;; HACK: This advice won't work in *all* cases (it's a C function, and any
  ;;   calls to it from C won't trigger advice), but the thread issues above are
  ;;   triggered from Elisp's buffer API (from what I can tell).
  (defadvice! +vc-gutter--kill-diff-hl-thread-a (&optional buf)
    :before #'kill-buffer
    (when-let ((buf (ignore-errors (window-normalize-buffer buf))))
      (with-current-buffer buf
        (+vc-gutter--kill-thread t)))))
