;;; tools/ein/config.el -*- lexical-binding: t; -*-

(defvar +ein-notebook-dir "~/"
  "Default directory from where Jupyter notebooks are to be opened.")

(def-setting! :ein-notebook-dir (dir)
  "Set the default directory from where to open Jupyter notebooks."
  `(setq +ein-notebook-dir ,dir))


(def-package! ein
  :commands (ein:notebooklist-open ein:notebooklist-login ein:jupyter-server-start)
  :init
  (push (lambda (buf) (string-match-p "^\\*ein: .*" (buffer-name buf)))
        doom-real-buffer-functions)
  (set! :popup "\\*ein: .*" :ignore)
  (set! :popup "\\*ein:tb .*" '((side . bottom) (size . 0.3)) '((quit . t) (transient) (select)))
  (set! :popup "\\*ein:notebooklist *" '((side . left) (size . 50)) '((select)))
  ;; Ace-link on notebook list buffers
  (add-hook! 'ein:notebooklist-mode-hook
    (map! :map ein:notebooklist-mode-map
          "o" #'+ein/ace-link-ein))
  ;; Ein uses request to store http cookies. Store them in the cache dir.
  (setq request-storage-directory (concat doom-cache-dir "/request"))
  ;; Auto complete with company
  (when (featurep! :completion company)
    (setq ein:completion-backend 'ein:use-company-backend)
    (set! :company-backend
      '(ein:notebook-multilang-mode
        ein:notebook-python-mode
        ein:notebook-plain-mode)
      'ein:company-backend))

  :config
  ;; Manually load the autoloads of EIN. This takes time...
  (load "ein-loaddefs.el" nil t t)
  (setq
   ;; Slice images into rows so that we can navigate buffers with images more easily
   ein:slice-image t
   ein:jupyter-default-notebook-directory +ein-notebook-dir
   ein:jupyter-default-server-command "jupyter"
   ein:jupyter-server-args '("--no-browser")
   ein:notebook-modes
   '(ein:notebook-multilang-mode ein:notebook-python-mode ein:notebook-plain-mode))
  ;; Avy is required for showing links in the notebook list with ace-link.
  (require 'avy)
  ;; add hydra
  (def-hydra! +ein/hydra (:hint t :color red)
    "
 Operations on Cells^^^^^^            Other
 ----------------------------^^^^^^   ----------------------------------^^^^
 [_k_/_j_]^^     select prev/next     [_t_]^^         toggle output
 [_K_/_J_]^^     move up/down         [_C-l_/_C-S-l_] clear/clear all output
 [_C-k_/_C-j_]^^ merge above/below    [_C-o_]^^       open console
 [_O_/_o_]^^     insert above/below   [_C-s_/_C-r_]   save/rename notebook
 [_y_/_p_/_d_]   copy/paste           [_x_]^^         close notebook
 [_u_]^^^^       change type          [_q_]^^         quit
 [_RET_]^^^^     execute
"
    ("q" nil :exit t)
    ("h" ein:notebook-worksheet-open-prev-or-last)
    ("j" ein:worksheet-goto-next-input)
    ("k" ein:worksheet-goto-prev-input)
    ("l" ein:notebook-worksheet-open-next-or-first)
    ("H" ein:notebook-worksheet-move-prev)
    ("J" ein:worksheet-move-cell-down)
    ("K" ein:worksheet-move-cell-up)
    ("L" ein:notebook-worksheet-move-next)
    ("t" ein:worksheet-toggle-output)
    ("d" ein:worksheet-kill-cell)
    ("R" ein:worksheet-rename-sheet)
    ("y" ein:worksheet-copy-cell)
    ("p" ein:worksheet-yank-cell)
    ("o" ein:worksheet-insert-cell-below)
    ("O" ein:worksheet-insert-cell-above)
    ("u" ein:worksheet-change-cell-type)
    ("RET" ein:worksheet-execute-cell-and-goto-next)
    ;; Output
    ("C-l" ein:worksheet-clear-output)
    ("C-S-l" ein:worksheet-clear-all-output)
    ;;Console
    ("C-o" ein:console-open :exit t)
    ;; Merge and split cells
    ("C-k" ein:worksheet-merge-cell)
    ("C-j" spacemacs/ein:worksheet-merge-cell-next)
    ("s" ein:worksheet-split-cell-at-point)
    ;; Notebook
    ("C-s" ein:notebook-save-notebook-command)
    ("C-r" ein:notebook-rename-command)
    ("x" ein:notebook-close :exit t)))
