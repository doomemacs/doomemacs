;;; private/hlissner/config.el

(load! +bindings) ; my key bindings
(when (featurep 'evil)
  (load! +commands)) ; my custom ex commands

(defvar +hlissner-snippets-dir
  (expand-file-name "snippets/" (file-name-directory load-file-name)))


;; On Arch, bspwm is my window manager. When I open GUI Emacs there's a gap on
;; the right side of the frame (which causes display glitches). I fix this by
;; simply cycling fullscreen:
(when (and (display-graphic-p) IS-LINUX)
  (add-hook! 'after-init-hook
    (set-frame-parameter nil 'fullscreen 'fullboth)
    (set-frame-parameter nil 'fullscreen nil)))


;; Don't use default snippets, use mine.
(after! yasnippet
  (setq yas-snippet-dirs (append (list '+hlissner-snippets-dir)
                                 (delete 'yas-installed-snippets-dir yas-snippet-dirs))))


;; Repeat all sorts of motion and searches with SPC & C-SPC
(defmacro +my!repeat-with-spc (command next-func prev-func)
  "Repeat motions with SPC/S-SPC"
  (let ((fn-sym (intern (format "+evil*repeat-%s" command))))
    `(progn
       (defun ,fn-sym (&rest _)
         (define-key evil-motion-state-map (kbd "SPC") ',next-func)
         (define-key evil-motion-state-map (kbd "S-SPC") ',prev-func))
       (advice-add ',command :before ',fn-sym))))

(after! evil
  ;; n/N
  (+my!repeat-with-spc evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
  (+my!repeat-with-spc evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
  (+my!repeat-with-spc evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
  (+my!repeat-with-spc evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)

  ;; f/F/t/T/s/S
  (after! evil-snipe
    (setq evil-snipe-repeat-keys nil
          evil-snipe-override-evil-repeat-keys nil) ; causes problems with remapped ;

    (+my!repeat-with-spc evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
    (+my!repeat-with-spc evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
    (+my!repeat-with-spc evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
    (+my!repeat-with-spc evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
    (+my!repeat-with-spc evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
    (+my!repeat-with-spc evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
    (+my!repeat-with-spc evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
    (+my!repeat-with-spc evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse))

  ;; */#
  (after! evil-visualstar
    (+my!repeat-with-spc evil-visualstar/begin-search-forward
      evil-ex-search-next evil-ex-search-previous)
    (+my!repeat-with-spc evil-visualstar/begin-search-backward
      evil-ex-search-previous evil-ex-search-next)))

