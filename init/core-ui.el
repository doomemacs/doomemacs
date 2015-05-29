;;; core-ui.el -- User interface layout & behavior

;;;; Load Theme ;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (set-frame-font *default-font)
  (set-frame-parameter nil 'alpha '(100 75)))
(add-to-list 'custom-theme-load-path my-themes-dir)
(load-theme *default-theme t)


;;;; GUI Settings ;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (scroll-bar-mode -1)       ; no scrollbar
  (tool-bar-mode -1)         ; no toolbar
  (menu-bar-mode -1)         ; no menubar
  (fringe-mode '(2 . 8)))    ; no nonsense

(global-hl-line-mode 1)      ; do highlight line
(blink-cursor-mode 1)        ; do blink cursor
(line-number-mode 1)         ; do show line no in modeline
(column-number-mode 1)       ; do show col no in modeline
(tooltip-mode -1)            ; don't show tooltips

;; Multiple cursors across buffers cause a strange redraw delay for
;; some things, like auto-complete or evil-mode's cursor color
;; switching.
(setq-default cursor-in-non-selected-windows nil
              visible-bell nil    ; silence of the bells
              use-dialog-box nil  ; avoid GUI
              redisplay-dont-pause t
              ;; do not soft-wrap lines
              truncate-lines t
              truncate-partial-width-windows nil
              indicate-buffer-boundaries nil
              indicate-empty-lines nil
              fringes-outside-margins t)

(use-package nlinum
  :commands nlinum-mode
  :init
  (progn
    (defface linum-highlight-face '((t (:inherit linum)))
      "Face for line highlights")

    ;; Preset width nlinum
    (add-hook! 'nlinum-mode-hook
               (setq nlinum--width
                     (length (number-to-string
                              (count-lines (point-min) (point-max))))))

    ;; Highlight line number
    (setq hl-nlinum-overlay nil)
    (setq hl-nlinum-line nil)
    (defun hl-nlinum-unhighlight-line ()
      (when hl-nlinum-overlay
        (let* ((ov hl-nlinum-overlay)
               (disp (get-text-property 0 'display (overlay-get ov 'before-string)))
               (str (nth 1 disp)))
          (put-text-property 0 (length str) 'face 'linum str)
          (setq hl-nlinum-overlay nil)
          (setq hl-nlinum-line nil))))

    (defun hl-nlinum-highlight-line ()
      (let ((line-no (line-number-at-pos (point))))
        (when (and nlinum-mode (not (eq line-no hl-nlinum-line)))
          (let* ((pbol (point-at-bol))
                 (peol (1+ pbol)))
            ;; Handle EOF case
            (when (>= peol (point-max))
              (setq pbol (line-beginning-position 0))
              (setq peol (line-end-position 0)))
            (jit-lock-fontify-now pbol peol)
            (let* ((overlays (overlays-in pbol peol))
                   (ov (-first (lambda (item) (overlay-get item 'nlinum)) overlays)))
              (when ov
                (hl-nlinum-unhighlight-line)
                (let* ((disp (get-text-property 0 'display (overlay-get ov 'before-string)))
                       (str (nth 1 disp)))
                  (put-text-property 0 (length str) 'face 'linum-highlight-face str)
                  (put-text-property 0 (length str) 'face 'linum-highlight-face str)
                  (setq hl-nlinum-overlay ov)
                  (setq hl-nlinum-line line-no))))))))

    (defun nlinum-toggle ()
      (interactive)
      (if nlinum-mode
          (nlinum-disable)
        (nlinum-enable)))
    (defun nlinum-enable ()
      (nlinum-mode +1)
      (add-hook 'post-command-hook 'hl-nlinum-highlight-line))
    (defun nlinum-disable ()
      (nlinum-mode -1)
      (remove-hook 'post-command-hook 'hl-nlinum-highlight-line)
      (hl-nlinum-unhighlight-line))

    (add-hooks '(text-mode-hook prog-mode-hook) 'nlinum-enable)
    (add-hook 'org-mode-hook 'nlinum-disable))
  :config
  (setq-default nlinum-format " %4d  "))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (if (string-equal (system-name) "io")
      (set-frame-size (selected-frame) 326 119)))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; [pedantry intensifies]
(rename-mode-name emacs-lisp-mode "Elisp")

(use-package vim-empty-lines-mode
  :config (global-vim-empty-lines-mode +1))


;;;; Modeline ;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-separator ":"
        uniquify-ignore-buffers-re "^\\*"))

(use-package smart-mode-line
  :config
  (progn
    (setq sml/no-confirm-load-theme t
          sml/mode-width      'full
          sml/extra-filler    (if window-system -1 0)
          sml/show-remote     nil
          sml/modified-char   "*"
          sml/encoding-format nil
          sml/replacer-regexp-list '(("^~/Dropbox/Projects/" "PROJECTS:")
                                     ("^~/.emacs.d/" "EMACS.D:")
                                     ("^~/Dropbox/notes/" "NOTES:")))

    ;; Hide evil state indicator
    (after "evil" (setq evil-mode-line-format nil))

    (setq-default mode-line-misc-info
      '((which-func-mode ("" which-func-format ""))
        (global-mode-string ("" global-mode-string ""))))

    (add-hook! 'after-change-major-mode-hook
               (unless (null mode-line-format)
                 (setq mode-line-format
                       '((if window-system " ")
                         "%e"
                         ;; mode-line-mule-info
                         ;; mode-line-client
                         ;; mode-line-remote
                         ;; mode-line-frame-identification
                         mode-line-buffer-identification
                         mode-line-modified
                         mode-line-modes
                         mode-line-misc-info
                         (vc-mode vc-mode)
                         " "
                         mode-line-position
                         " "
                         mode-line-front-space
                         )))

               (add-to-list 'mode-line-modes
                            '(sml/buffer-identification-filling
                              sml/buffer-identification-filling
                              (:eval (setq sml/buffer-identification-filling
                                           (sml/fill-for-buffer-identification))))))

    (let ((-linepo mode-line-position))
      (sml/setup)
      (sml/apply-theme 'respectful)
      (setq mode-line-position -linepo)
      (sml/filter-mode-line-list 'mode-line-position))))


(provide 'core-ui)
;;; core-ui.el ends here
