;;; ui/doom/config.el -*- lexical-binding: t; -*-

(defvar +doom-padding 6)


;;
;;; Configuration

;;;###package pos-tip
(setq pos-tip-internal-border-width 6
      pos-tip-border-width 1)


(when (modulep! +tabs)
  (after! tab-bar
    ;; Add some left-side padding to tab bar labels.
    (setq tab-bar-tab-name-format-function #'+doom-tab-name-format-fn)
    (defun +doom-tab-name-format-fn (tab i)
      (let ((name (concat " " (copy-sequence (alist-get 'name tab)))))
        (run-hook-wrapped 'tab-bar-tab-name-format-functions
                          (lambda (fun)
                            (setq name (funcall fun name tab i))
                            nil))
        name))

    ;; Give the tab bar some artifical height.
    (defvar +doom-tab-bar-height (+ (window-font-height nil 'tab-bar-tab) +doom-padding))
    (defadvice! +doom-tab-bar-separator-a ()
      :override #'tab-bar-separator
      (or tab-bar-separator
          (if (window-system)
              (if (image-type-available-p 'pbm)
                  (with-memoization (frame-parameter nil 'tab-bar-separator)
                    (propertize
                     " " 'display
                     (ignore-errors
                       (create-image
                        (concat (format "P1\n%i %i\n" 2 +doom-tab-bar-height)
                                (make-string (* 2 +doom-tab-bar-height) ?1)
                                "\n")
                        'pbm t :scale 1 :foreground "None" :ascent 'center))))
                " ")
            "|")))))


;;
;;; Packages

(use-package! doom-themes
  ;; improve integration w/ org-mode
  :hook (doom-load-theme . doom-themes-org-config)
  :init (setq doom-theme 'doom-one)
  ;; more Atom-esque file icons for neotree/treemacs
  ;; (when (modulep! :ui neotree)
  ;;   (add-hook 'doom-load-theme-hook #'doom-themes-neotree-config)
  ;;   (setq doom-themes-neotree-enable-variable-pitch t
  ;;         doom-themes-neotree-file-icons 'simple
  ;;         doom-themes-neotree-line-spacing 2))
  )


(use-package! solaire-mode
  :hook (doom-load-theme . solaire-global-mode)
  :hook (+popup-buffer-mode . turn-on-solaire-mode))
