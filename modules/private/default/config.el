;;; private/default/config.el -*- lexical-binding: t; -*-

(load! +bindings)


;;
;; Plugins
;;

(def-package! emacs-snippets :after yasnippet)


;;
;; Config
;;

(after! epa
  (setq epa-file-encrypt-to (or epa-file-encrypt-to user-mail-address)
        ;; With GPG 2.1, this forces gpg-agent to use the Emacs minibuffer to
        ;; prompt for the key passphrase.
        epa-pinentry-mode 'loopback))


(when (featurep 'evil)
  (load! +evil-commands)

  ;; Makes ; and , the universal repeat-keys in evil-mode
  (defmacro do-repeat! (command next-func prev-func)
    "Repeat motions with ;/,"
    (let ((fn-sym (intern (format "+evil*repeat-%s" command))))
      `(progn
         (defun ,fn-sym (&rest _)
           (define-key evil-motion-state-map (kbd ";") ',next-func)
           (define-key evil-motion-state-map (kbd ",") ',prev-func))
         (advice-add #',command :before #',fn-sym))))

  ;; n/N
  (do-repeat! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)

  ;; f/F/t/T/s/S
  (after! evil-snipe
    (setq evil-snipe-repeat-keys nil
          evil-snipe-override-evil-repeat-keys nil) ; causes problems with remapped ;

    (do-repeat! evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse))

  ;; */#
  (after! evil-visualstar
    (do-repeat! evil-visualstar/begin-search-forward
                evil-ex-search-next evil-ex-search-previous)
    (do-repeat! evil-visualstar/begin-search-backward
                evil-ex-search-previous evil-ex-search-next))

  (after! evil-easymotion
    (let ((prefix (concat doom-leader-key " /")))
      ;; NOTE `evilem-default-keybinds' unsets all other keys on the prefix (in
      ;; motion state)
      (evilem-default-keybindings prefix)
      (evilem-define (kbd (concat prefix " n")) #'evil-ex-search-next)
      (evilem-define (kbd (concat prefix " N")) #'evil-ex-search-previous)
      (evilem-define (kbd (concat prefix " s")) #'evil-snipe-repeat
                     :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                     :bind ((evil-snipe-scope 'buffer)
                            (evil-snipe-enable-highlight)
                            (evil-snipe-enable-incremental-highlight)))
      (evilem-define (kbd (concat prefix " S")) #'evil-snipe-repeat-reverse
                     :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                     :bind ((evil-snipe-scope 'buffer)
                            (evil-snipe-enable-highlight)
                            (evil-snipe-enable-incremental-highlight))))))
