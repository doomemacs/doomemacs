;;; ui/doom/config.el -*- lexical-binding: t; -*-

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
  :defer t
  :init
  (add-hook! 'doom-load-theme-hook :append
    (defun +doom-solaire-mode-swap-bg-maybe-h ()
      (when (string-prefix-p "doom-" (symbol-name doom-theme))
        (require 'solaire-mode)
        (solaire-mode-swap-bg))))
  :config
  ;; fringe can become unstyled when deleting or focusing frames
  (add-hook 'focus-in-hook #'solaire-mode-reset)
  ;; Prevent color glitches when reloading either DOOM or loading a new theme
  (add-hook! '(doom-load-theme-hook doom-reload-hook) :append
             #'solaire-mode-reset)
  ;; org-capture takes an org buffer and narrows it. The result is erroneously
  ;; considered an unreal buffer, so solaire-mode must be restored.
  (add-hook 'org-capture-mode-hook #'turn-on-solaire-mode)

  ;; On Emacs 26+, when point is on the last line and solaire-mode is remapping
  ;; the hl-line face, hl-line's highlight bleeds into the rest of the window
  ;; after eob. On Emacs 27 this no longer happens.
  (unless EMACS27+
    (defun +doom--line-range-fn ()
      (let ((bol (line-beginning-position))
            (eol (line-end-position))
            (pmax (point-max)))
        (cons bol
              (cond ((and (=  eol pmax)
                          (/= eol bol))
                     (1- eol))
                    ((or (eobp)
                         (= eol pmax))
                     eol)
                    ((line-beginning-position 2))))))
    (setq hl-line-range-function #'+doom--line-range-fn))

  ;; Because fringes can't be given a buffer-local face, they can look odd, so
  ;; we remove them in the minibuffer and which-key popups (they serve no
  ;; purpose there anyway).
  (add-hook! 'solaire-mode-hook
    (defun +doom-disable-fringes-in-minibuffer-h (&rest _)
      (set-window-fringes (minibuffer-window) 0 0 nil)))

  (defadvice! +doom--no-fringes-in-which-key-buffer-a (&rest _)
    :after 'which-key--show-buffer-side-window
    (+doom-disable-fringes-in-minibuffer-h)
    (set-window-fringes (get-buffer-window which-key--buffer) 0 0 nil))

  (add-hook! '(minibuffer-setup-hook window-configuration-change-hook)
             #'+doom-disable-fringes-in-minibuffer-h)

  (solaire-global-mode +1))
