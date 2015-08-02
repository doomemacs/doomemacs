;;; core-ui.el --- interface settings
;; see lib/ui-defuns.el

(load-theme (if window-system narf-default-theme narf-term-theme) t)
(when window-system
  (set-frame-font (apply #'font-spec narf-default-font))
  (scroll-bar-mode -1)        ; no scrollbar
  (tool-bar-mode -1)          ; no toolbar
  (menu-bar-mode -1)          ; no menubar
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (fringe-mode '(2 . 8))
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq show-paren-delay 0)

(global-hl-line-mode  1)    ; do highlight line
(blink-cursor-mode    1)    ; do blink cursor
(line-number-mode     1)    ; do show line no in modeline
(column-number-mode   1)    ; do show col no in modeline
(size-indication-mode 1)    ; do show file size
(tooltip-mode        -1)    ; don't show tooltips

(setq-default
 ;; Multiple cursors across buffers cause a strange redraw delay for
 ;; some things, like auto-complete or evil-mode's cursor color
 ;; switching.
 cursor-in-non-selected-windows  nil

 visible-bell                    nil    ; silence of the bells
 use-dialog-box                  nil    ; avoid GUI
 redisplay-dont-pause            t
 indicate-buffer-boundaries      nil
 indicate-empty-lines            nil
 fringes-outside-margins         t)     ; fringes on the other side of line numbers

(add-hook! after-init
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (flet ((process-list ())) ad-do-it)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nlinum ; line numbers
  :defer t
  :defines nlinum--width
  :preface
  (defface linum '((t (:inherit default)))
    "Face for line numbers" :group 'nlinum-mode)
  (defface linum-highlight-face '((t (:inherit linum)))
    "Face for line highlights" :group 'nlinum-mode)
  (defvar narf--hl-nlinum-overlay nil)
  (defvar narf--hl-nlinum-line    nil)
  (defvar nlinum-format " %3d  ")
  :init
  ;; Highlight line number
  (defun narf|nlinum-unhl-line ()
    (when narf--hl-nlinum-overlay
      (let* ((ov narf--hl-nlinum-overlay)
             (disp (get-text-property 0 'display (overlay-get ov 'before-string)))
             (str (nth 1 disp)))
        (put-text-property 0 (length str) 'face 'linum str)
        (setq narf--hl-nlinum-overlay nil
              narf--hl-nlinum-line nil))))

  (defun narf|nlinum-hl-line (&optional line)
    (let ((line-no (or line (line-number-at-pos (point)))))
      (when (and nlinum-mode (not (eq line-no narf--hl-nlinum-line)))
        (let* ((pbol (if line (save-excursion (goto-char (point-min))
                                              (forward-line line-no)
                                              (point-at-bol))
                       (point-at-bol)))
               (peol (1+ pbol)))
          ;; Handle EOF case
          (when (>= peol (point-max))
            (setq peol (point-max)))
          (jit-lock-fontify-now pbol peol)
          (let* ((overlays (overlays-in pbol peol))
                 (ov (-first (lambda (item) (overlay-get item 'nlinum)) overlays)))
            (when ov
              (narf|nlinum-unhl-line)
              (let* ((disp (get-text-property 0 'display (overlay-get ov 'before-string)))
                     (str (nth 1 disp)))
                (put-text-property 0 (length str) 'face 'linum-highlight-face str)
                (put-text-property 0 (length str) 'face 'linum-highlight-face str)
                (setq narf--hl-nlinum-overlay ov
                      narf--hl-nlinum-line line-no))))))))

  (defun narf|nlinum-enable ()
    (nlinum-mode +1)
    (add-hook 'post-command-hook 'narf|nlinum-hl-line))

  (defun narf|nlinum-disable ()
    (nlinum-mode -1)
    (remove-hook 'post-command-hook 'narf|nlinum-hl-line)
    (narf|nlinum-unhl-line))

  ;; Preset width nlinum
  (add-hook! (text-mode prog-mode scss-mode web-mode) 'narf|nlinum-enable)
  (add-hook! org-mode 'narf|nlinum-disable)
  (add-hook! nlinum-mode
    (setq nlinum--width (length (number-to-string (count-lines (point-min) (point-max)))))))

(use-package smart-mode-line ; customized modeline
  :init (setq-default
         sml/no-confirm-load-theme t
         sml/mode-width            'full
         sml/extra-filler          -6
         sml/show-remote           nil
         sml/encoding-format       nil
         sml/modified-char         "*"
         sml/numbers-separator     "/"
         sml/line-number-format    "%l"
         sml/col-number-format     "%c"
         sml/position-percentage-format "%P"
         sml/pre-modes-separator       " ["
         sml/pre-minor-modes-separator " "
         sml/pos-minor-modes-separator "] "
         sml/replacer-regexp-list '(("^~/.emacs.d/" "EMACS.D:")
                                    ("^~/Dropbox/Projects/" "PROJECTS:")
                                    ("^~/Dropbox/notes/" "NOTES:")
                                    ("^/usr/local/Cellar/" "HOMEBREW:")))
  :config
  ;; Hack modeline to be more vim-like, and right-aligned
  (defun sml/generate-minor-modes ()
    (if sml/simplified
        ""
      (let* ((nameList (rm--mode-list-as-string-list))
             (last nil)
             (concatList (mapconcat (lambda (mode)
                                      (setq mode (s-trim mode))
                                      (if (> (length mode) 1)
                                          (prog1 (concat (if last " ") mode " ")
                                            (setq last nil))
                                        (prog1 mode
                                          (setq last t))))
                                    nameList ""))
             (size (sml/fill-width-available))
             (finalNameList concatList)
             needs-removing filling)
        (when (and sml/shorten-modes (> (length finalNameList) size))
          (setq needs-removing
                (1+ (sml/count-occurrences-starting-at
                     " " finalNameList
                     (- size (string-width sml/full-mode-string))))))
        (when needs-removing
          (setcdr (last nameList (1+ needs-removing))
                  (list t sml/propertized-full-mode-string)))
        (unless sml/shorten-modes
          (add-to-list 'nameList sml/propertized-shorten-mode-string t))
        (setq filling (- size (+ (length (format-mode-line concatList)) (length mode-name) (length vc-mode))))
        (setq filling (make-string (max 0 filling) sml/fill-char))
        (list (propertize filling 'face 'sml/modes)
              (propertize (or vc-mode "") 'face 'sml/vc)
              (propertize sml/pre-modes-separator 'face 'font-lock-comment-delimiter-face)
              (propertize mode-name)
              'sml/pre-minor-modes-separator
              concatList
              (propertize sml/pos-minor-modes-separator 'face
                          'font-lock-comment-delimiter-face)))))

  ;; Hide evil state indicator
  (after! evil (setq evil-mode-line-format nil))
  ;; Add small gap for anzu display
  (after! anzu
    (defun narf--anzu-update-mode-line (here total)
      (concat (anzu--update-mode-line-default here total) " "))
    (setq anzu-mode-line-update-function 'narf--anzu-update-mode-line))
  (sml/setup)
  (sml/apply-theme 'respectful)
  ;; Remove extra spaces in format lists
  (pop mode-line-modes)
  (nbutlast mode-line-modes)
  ;; Remove spacing in mode-line position so we can put it elsewhere
  (setq mode-line-position
        '(":" (sml/position-percentage-format
               (-3 (:propertize (:eval sml/position-percentage-format) face sml/position-percentage)))))

  ;; Rearrange and cleanup
  (setq-default mode-line-format
                '("%e"
                  mode-line-mule-info
                  mode-line-client
                  ;; mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  mode-line-modified
                  mode-line-misc-info
                  mode-line-modes
                  mode-line-front-space
                  mode-line-end-spaces
                  " "
                  mode-line-position)))

(provide 'core-ui)
;;; core-ui.el ends here
