;;; ui/zen/config.el -*- lexical-binding: t; -*-

(defvar +zen-mixed-pitch-modes '(markdown-mode org-mode org-journal-mode)
  "What major-modes to enable `mixed-pitch-mode' in with `writeroom-mode'.")

(defvar +zen-text-scale 2
  "The text-scaling level for `writeroom-mode'.")

(defvar +zen-window-divider-size 4
  "Pixel size of window dividers when `writeroom-mode' is active.")


(defvar +zen--old-window-divider-size window-divider-default-bottom-width)


;;
;;; Packages

(after! writeroom-mode
  ;; Users should be able to activate writeroom-mode in one buffer (e.g. an org
  ;; buffer) and code in another. Fullscreening/maximizing will be opt-in.
  (setq writeroom-maximize-window nil)
  (remove-hook 'writeroom-global-effects #'writeroom-set-fullscreen)

  (add-hook! 'writeroom-mode-hook
    (defun +zen-enable-text-scaling-mode-h ()
      "Enable `mixed-pitch-mode' when in `+zen-mixed-pitch-modes'."
      (when (/= +zen-text-scale 0)
        (text-scale-set (if writeroom-mode +zen-text-scale 0))
        (visual-fill-column-adjust)))
    (defun +zen-toggle-large-window-dividers-h ()
      "Make window dividers larger and easier to see."
      (when (bound-and-true-p window-divider-mode)
        (if writeroom-mode-hook
            (setq +zen--old-window-divider-size (cons window-divider-default-bottom-width
                                                      window-divider-default-right-width)
                  window-divider-default-bottom-width +zen-window-divider-size
                  window-divider-default-right-width +zen-window-divider-size)
          (setq window-divider-default-bottom-width (car +zen--old-window-divider-size)
                window-divider-default-right-width (cdr +zen--old-window-divider-size)))
        (window-divider-mode +1))))

  ;; Adjust margins when text size is changed
  (advice-add #'text-scale-adjust :after #'visual-fill-column-adjust))


(use-package! mixed-pitch
  :hook (writeroom-mode . +zen-enable-mixed-pitch-mode-h)
  :config
  (defun +zen-enable-mixed-pitch-mode-h ()
    "Enable `mixed-pitch-mode' when in `+zen-mixed-pitch-modes'."
    (when (apply #'derived-mode-p +zen-mixed-pitch-modes)
      (mixed-pitch-mode (if writeroom-mode +1 -1))))

  (pushnew! mixed-pitch-fixed-pitch-faces
            'org-date
            'org-special-keyword
            'org-property-value
            'org-ref-cite-face
            'org-tag
            'org-todo-keyword-todo
            'org-todo-keyword-habt
            'org-todo-keyword-done
            'org-todo-keyword-wait
            'org-todo-keyword-kill
            'org-todo-keyword-outd
            'org-todo
            'org-done
            'font-lock-comment-face
            'line-number
            'line-number-current-line))
