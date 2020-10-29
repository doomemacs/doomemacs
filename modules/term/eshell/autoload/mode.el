;;; term/eshell/autoload/mode.el -*- lexical-binding: t; -*-
;;;###if (not EMACS28+)

;; DEPRECATED Remove this when we drop Emacs 27 support.

;; HACK Eshell resets its keymap every time `eshell-mode' is enabled. Why? It
;;      is not for us mere mortals to question! Anyhow, we undo this brilliant
;;      design by backporting the fix from Emacs 28, so keys can be bound to
;;      `eshell-mode-map' & `eshell-command-map' like any normal keymap,
;;      rather than in a hook.
;;
;;      Fun fact: there's a "FIXME What the hell?!" above the offending line
;;      in esh-mode.el.

;;;###autoload
(defvar eshell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c)] 'eshell-command-map)
    (define-key map "\r" #'eshell-send-input)
    (define-key map "\M-\r" #'eshell-queue-input)
    (define-key map [(meta control ?l)] #'eshell-show-output)
    (define-key map [(control ?a)] #'eshell-bol)
    map))

;;;###autoload
(defvar eshell-command-map
  (let ((map (define-prefix-command 'eshell-command-map)))
    (define-key map [(meta ?o)] #'eshell-mark-output)
    (define-key map [(meta ?d)] #'eshell-toggle-direct-send)
    (define-key map [(control ?a)] #'eshell-bol)
    (define-key map [(control ?b)] #'eshell-backward-argument)
    (define-key map [(control ?e)] #'eshell-show-maximum-output)
    (define-key map [(control ?f)] #'eshell-forward-argument)
    (define-key map [(control ?m)] #'eshell-copy-old-input)
    (define-key map [(control ?o)] #'eshell-kill-output)
    (define-key map [(control ?r)] #'eshell-show-output)
    (define-key map [(control ?t)] #'eshell-truncate-buffer)
    (define-key map [(control ?u)] #'eshell-kill-input)
    (define-key map [(control ?w)] #'backward-kill-word)
    (define-key map [(control ?y)] #'eshell-repeat-argument)
    map))

;;;###autoload
(add-hook! 'eshell-mode-hook
  (defun +eshell-fix-keymap-h ()
    "Undo buffer-local `eshell-mode-map', so global keybinds work."
    (kill-local-variable 'eshell-mode-map)
    (kill-local-variable 'eshell-command-prefix)
    (kill-local-variable 'eshell-command-map)
    (use-local-map eshell-mode-map)))
