;;; ui/doom/config.el -*- lexical-binding: t; -*-

(defvar +doom-solaire-themes
  '((doom-acario-dark . t)
    (doom-acario-light . t)
    (doom-challenger-deep . t)
    (doom-city-lights . t)
    (doom-dark+ . t)
    (doom-dracula . t)
    (doom-fairy-floss . t)
    (doom-gruvbox . t)
    (doom-horizon . t)
    (doom-laserwave . t)
    (doom-losvkem . t)
    (doom-manegarm . t)
    (doom-material . t)
    (doom-molokai . t)
    (doom-moonlight . t)
    (doom-nord . t)
    (doom-nord-light . t)
    (doom-nova . t)
    (doom-oceanic-next . t)
    (doom-one . t)
    (doom-one-light . t)
    (doom-outrun-electric . t)
    (doom-opera . t)
    (doom-palenight . t)
    (doom-peacock . t)
    (doom-snazzy . t)
    (doom-solarized-dark . t)
    (doom-solarized-light)
    (doom-sourcerer . t)
    (doom-spacegrey . t)
    (doom-tomorrow-day . t)
    (doom-tomorrow-night . t)
    (doom-vibrant . t))
  "An alist of themes that support `solaire-mode'. If CDR is t, then
`solaire-mode-swap-bg' will be used automatically, when the theme is loaded.")


;;
;;; Packages

(use-package! doom-themes
  :defer t
  :init
  (unless doom-theme
    (setq doom-theme 'doom-one))
  :config
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
      (pcase-let ((`(,_theme . ,swap) (assq doom-theme +doom-solaire-themes)))
        (require 'solaire-mode)
        (if swap (solaire-mode-swap-bg)))))
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
      (cons (line-beginning-position)
            (cond ((let ((eol (line-end-position)))
                     (and (=  eol (point-max))
                          (/= eol (line-beginning-position))))
                   (1- (line-end-position)))
                  ((or (eobp)
                       (= (line-end-position 2) (point-max)))
                   (line-end-position))
                  ((line-beginning-position 2)))))
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
