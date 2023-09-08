;;; editor/evil/init.el -*- lexical-binding: t; -*-

(when (and (not noninteractive)
           (not (doom-context-p 'reload))
           (modulep! +everywhere))
  (setq
   ;; We do this ourselves, and better.
   evil-collection-want-unimpaired-p nil
   ;; Doom binds goto-reference on gD and goto-assignments on gA ourselves
   evil-collection-want-find-usages-bindings-p nil
   ;; Reduces keybind conflicts between outline-mode and org-mode (which is
   ;; derived from outline-mode).
   evil-collection-outline-enable-in-minor-mode-p nil

   evil-collection-setup-minibuffer nil
   ;; must be set before evil/evil-collection is loaded
   evil-collection-company-use-tng (modulep! :completion company +tng)
   evil-want-keybinding nil)

  (defadvice! +evil-collection-disable-blacklist-a (fn)
    :around #'evil-collection-vterm-toggle-send-escape ; allow binding to ESC
    (let (evil-collection-key-blacklist)
      (funcall-interactively fn)))

  ;; These modes belong to packages that Emacs always loads at startup, causing
  ;; evil-collection and it's co-packages to all load immediately. We avoid this
  ;; by loading them after evil-collection has first loaded...
  (after! evil-collection
    ;; Don't let evil-collection interfere with certain keys
    (setq evil-collection-key-blacklist
          (append (list doom-leader-key doom-localleader-key
                        doom-leader-alt-key)
                  evil-collection-key-blacklist
                  (when (modulep! :tools lookup)
                    '("gd" "gf" "K"))
                  (when (modulep! :tools eval)
                    '("gr" "gR"))
                  '("[" "]" "gz" "<escape>"))
          evil-collection-mode-list
          (cl-set-difference evil-collection-mode-list
                             '(anaconda-mode
                               company
                               eldoc
                               ert
                               helm
                               kotlin-mode
                               outline
                               simple
                               slime
                               lispy)))

    (evil-define-key* 'normal process-menu-mode-map
      "q" #'kill-current-buffer
      "d" #'process-menu-delete-process))

  (evil-collection-init))
