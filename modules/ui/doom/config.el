;;; ui/doom/config.el -*- lexical-binding: t; -*-

;;;###package pos-tip
(setq pos-tip-internal-border-width 6
      pos-tip-border-width 1)


(use-package! doom-themes
  :defer t
  :init
  (unless doom-theme
    (setq doom-theme 'doom-one))
  ;; improve integration w/ org-mode
  (add-hook 'doom-load-theme-hook #'doom-themes-org-config)
  ;; more Atom-esque file icons for neotree/treemacs
  (when (featurep! :ui neotree)
    (add-hook 'doom-load-theme-hook #'doom-themes-neotree-config)
    (setq doom-themes-neotree-enable-variable-pitch t
          doom-themes-neotree-file-icons 'simple
          doom-themes-neotree-line-spacing 2))
  (when (featurep! :ui treemacs)
    (add-hook 'doom-load-theme-hook #'doom-themes-treemacs-config)))


(use-package! solaire-mode
  :when (or (daemonp) (display-graphic-p))
  :hook (doom-load-theme . solaire-global-mode)
  :config
  (when (daemonp)
    (add-hook! '(doom-switch-frame-hook after-make-frame-functions)
      (defun +doom-disable-solaire-mode-maybe-h (&optional frame)
        (if (display-graphic-p frame)
            (unless solaire-global-mode
              (solaire-global-mode +1))
          (when solaire-global-mode
            (solaire-global-mode -1))))))

  ;; DEPRECATED No longer needed in Emacs 27+
  (unless EMACS27+
    ;; HACK On Emacs <=26, when point is on the last (or second to last) line
    ;;      and solaire-mode is remapping the hl-line face, hl-line's highlight
    ;;      bleeds into the rest of the window after eob. On Emacs 27 this no
    ;;      longer happens. This tries to fix it for 26 users, but it imposes
    ;;      another problem: the 2nd-to-last line will only be highlighted up to
    ;;      the (n-1)th character, but I think that is the lesser evil.
    (defun +doom--line-range-fn ()
      (if solaire-mode
          (let ((bol (line-beginning-position))
                (eol (line-end-position))
                (eob (point-max)))
            (cond ((= bol eob)
                   nil)
                  ((= (1+ eol) eob)
                   (cons bol (1- eob)))
                  ((or (= eol eob) (eobp))
                   (cons bol eol))
                  ((cons bol (line-beginning-position 2)))))
        (cons (line-beginning-position)
              (line-beginning-position 2))))
    (setq hl-line-range-function #'+doom--line-range-fn)

    ;; HACK The fringe cannot have a buffer-local remapping on Emacs <= 26, so
    ;;      we jump through hoops to reset it (globally) whenever it is likely
    ;;      that the fringe will have lost its background color.
    (add-hook! '(doom-load-theme-hook doom-reload-hook) :append
               #'solaire-mode-reset)

    ;; fringe can become unstyled when deleting or focusing frames
    (add-hook 'focus-in-hook #'solaire-mode-reset)

    ;; A global fringe color means the minibuffer (with its fringes) will always
    ;; stand out, so we remove them (in which-key popups too).
    (add-hook! 'solaire-mode-hook
      (defun +doom-disable-fringes-in-minibuffer-h (&rest _)
        (set-window-fringes (minibuffer-window) 0 0 nil)))
    (defadvice! +doom--no-fringes-in-which-key-buffer-a (&rest _)
      :after 'which-key--show-buffer-side-window
      (+doom-disable-fringes-in-minibuffer-h)
      (set-window-fringes (get-buffer-window which-key--buffer) 0 0 nil))
    (add-hook! '(minibuffer-setup-hook window-configuration-change-hook)
               #'+doom-disable-fringes-in-minibuffer-h)))
