;;; lang/org/+babel.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org|init-babel)

(defvar +org-babel-mode-alist
  '(("cpp" . C)
    ("C++" . C)
    ("D" . C)
    ("sh" . shell)
    ("bash" . shell)
    ("matlab" . octave))
  "An alist that maps languages to babel libraries. This is necessary for babel
libraries (ob-*.el) that don't match the name of the language.")

(defvar +org-babel-load-functions ()
  "A list of functions that are used to try to load the current executing src
block. They take one argument (the language specified in the src block, as
string). Stops at the first function to return non-nil.")

(defun +org|init-babel ()
  (setq org-src-fontify-natively t      ; make code pretty
        org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil) ; you don't need my permission

  (defun +org*babel-execute-src-block (orig-fn &rest args)
    "Load babel libraries as needed when babel blocks are executed."
    (let* ((language (org-element-property :language (org-element-at-point)))
           (lang-sym (intern language)))
      (when (and (not (cdr (assq lang-sym org-babel-load-languages)))
                 (or (run-hook-with-args-until-success '+org-babel-load-functions language)
                     (require
                      (intern (format "ob-%s"
                                      (or (cdr (assoc (downcase language) +org-babel-mode-alist))
                                          language)))
                      nil t)))
        (map-put org-babel-load-languages lang-sym t))
      (apply orig-fn args)))
  (advice-add #'org-babel-execute-src-block :around #'+org*babel-execute-src-block)

  ;; I prefer C-c C-c for confirming over the default C-c '
  (map! :map org-src-mode-map "C-c C-c" #'org-edit-src-exit)

  ;; In a recent update, `org-babel-get-header' was removed from org-mode, which
  ;; is something a fair number of babel plugins use. So until those plugins
  ;; update, this polyfill will do:
  (defun org-babel-get-header (params key &optional others)
    (cl-loop with fn = (if others #'not #'identity)
             for p in params
             if (funcall fn (eq (car p) key))
             collect p)))


;;
;; Plugins
;;

(def-package! ob-ipython
  :when (featurep! +ipython)
  :defer t
  :init
  (defvar +ob-ipython-local-runtime-dir nil
    "TODO")

  (setq ob-ipython-resources-dir ".ob-ipython-resrc")

  (defun +org|babel-load-ipython (language)
    (and (string-match-p "^jupyter-" language)
         (require 'ob-ipython nil t)))
  (add-hook '+org-babel-load-functions #'+org|babel-load-ipython)
  :config
  (set! :popups
    '("^\\*Org Src"
      ((size . 100) (side . right) (slot . -1) (window-height . 0.6))
      ((quit) (select . t) (modeline)))
    '("^\\*Python"
      ((slot . 0) (side . right) (size . 100))
      ((select) (quit) (transient)))
    '("\\*ob-ipython.*"
      ((slot . 2) (side . right) (size . 100) (window-height . 0.2))
      ((select) (quit) (transient)))
    '("\\*Python:.*"
      ((slot . 0) (side . right) (size . 100))
      ((select) (quit) (transient))))
  ;; TODO Add more popup styles

  ;; advices for remote kernel and org-src-edit
  (advice-add 'org-babel-edit-prep:ipython :override #'+org*org-babel-edit-prep:ipython)
  (advice-add 'org-babel-ipython-initiate-session :override #'+org*org-babel-ipython-initiate-session)
  (advice-add 'ob-ipython--create-repl :override #'+org*ob-ipython--create-repl)
  (advice-add 'org-babel-execute:ipython :override #'+org*org-babel-execute:ipython)

  ;; retina resolution image hack
  (when (eq window-system 'ns)
    (advice-add 'ob-ipython--write-base64-string :around #'+org*ob-ipython--write-base64-string)))
