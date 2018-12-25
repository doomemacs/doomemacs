;;; lang/org/+babel.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org|init-babel)

(defvar +org-babel-mode-alist
  '((cpp . C)
    (C++ . C)
    (D . C)
    (sh . shell)
    (bash . shell)
    (matlab . octave))
  "An alist mapping languages to babel libraries. This is necessary for babel
libraries (ob-*.el) that don't match the name of the language.

For example, with (fish . shell) will cause #+BEGIN_SRC fish to load ob-shell.el
when executed.")

(defvar +org-babel-load-functions ()
  "A list of functions for loading the current executing src block. They take
one argument (the language specified in the src block, as a string). Stops at
the first function to return non-nil.")

(defun +org|init-babel ()
  (setq org-src-fontify-natively t      ; make code pretty
        org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil) ; you don't need my permission

  (defun +org*babel-lazy-load-library (info)
    "Load babel libraries as needed when babel blocks are executed."
    (let* ((lang (nth 0 info))
           (lang (if (symbolp lang) lang (intern lang)))
           (lang (or (cdr (assq lang +org-babel-mode-alist))
                     lang)))
      (when (and (not (cdr (assq lang org-babel-load-languages)))
                 (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
                     (require (intern (format "ob-%s" lang)) nil t)))
        (when (assq :async (nth 2 info))
          ;; ob-async has its own agenda for lazy loading packages (in the
          ;; child process), so we only need to make sure it's loaded.
          (require 'ob-async nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))
      t))
  (advice-add #'org-babel-confirm-evaluate :after-while #'+org*babel-lazy-load-library)

  ;; I prefer C-c C-c over C-c ' (more consistent)
  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  ;; `org-babel-get-header' was removed from org in 9.0. Quite a few babel
  ;; plugins use it, so until those plugins update, this polyfill will do:
  (defun org-babel-get-header (params key &optional others)
    (cl-loop with fn = (if others #'not #'identity)
             for p in params
             if (funcall fn (eq (car p) key))
             collect p)))


;;
;; Packages

(def-package! ob-ipython
  :when (featurep! +ipython)
  :defer t
  :init
  (defvar +ob-ipython-local-runtime-dir nil
    "TODO")

  (setq ob-ipython-resources-dir ".ob-ipython-resrc")

  (defun +org|babel-load-ipython (lang)
    (and (string-prefix-p "jupyter-" (symbol-name lang))
         (require 'ob-ipython nil t)))
  (add-hook '+org-babel-load-functions #'+org|babel-load-ipython)
  :config
  (set-popup-rules!
    '(("\\*ob-ipython.*"
       :slot 2 :side right :size 100 :height 0.2
       :select nil :quit nil :transient nil)
      ("^\\*Python"
       :slot 0 :side right :size 100
       :select nil :quit nil :ttl nil)
      ("\\*Python:.*"
       :slot 0 :side right :size 100
       :select nil :quit nil :transient nil)))
  ;; TODO Add more popup styles

  ;; advices for remote kernel and org-src-edit
  (advice-add 'org-babel-edit-prep:ipython :override #'+org*org-babel-edit-prep:ipython)
  (advice-add 'org-babel-ipython-initiate-session :override #'+org*org-babel-ipython-initiate-session)
  (advice-add 'ob-ipython--create-repl :override #'+org*ob-ipython--create-repl)
  (advice-add 'org-babel-execute:ipython :override #'+org*org-babel-execute:ipython)

  ;; retina resolution image hack
  (when (eq window-system 'ns)
    (advice-add 'ob-ipython--write-base64-string :around #'+org*ob-ipython--write-base64-string)))
