;;; tools/ein/config.el -*- lexical-binding: t; -*-

(defvar +ein-notebook-dir "~/"
  "Default directory from where Jupyter notebooks are to be opened.")


;;
;; Plugins
;;

(after! ein
  (setq ein:notebook-modes
        '(ein:notebook-multilang-mode
          ein:notebook-python-mode
          ein:notebook-plain-mode)
        ;; Slice images into rows; easier to navigate around images
        ein:slice-image t)

  (set! :popup "\\*ein: .*" :ignore)
  (set! :popup "\\*ein:tb .*"
    '((side . bottom) (size . 0.3))
    '((quit . t) (transient) (select)))
  (set! :popup "\\*ein:notebooklist *"
    '((side . left) (size . 50))
    '((select)))

  (when (featurep! :completion company)
    ;; Code completion with company
    (setq ein:completion-backend 'ein:use-company-backend)
    (set! :company-backend '(ein:notebook-multilang-mode
                             ein:notebook-python-mode
                             ein:notebook-plain-mode)
      'ein:company-backend))

  (after! ein-jupyter
    (setq ein:jupyter-server-args '("--no-browser"))
    (unless ein:jupyter-default-notebook-directory
      (setq ein:jupyter-default-notebook-directory "~/")))

  (defun +ein-buffer-p (buf)
    (string-match-p "^\\*ein: .*" (buffer-name buf)))
  (add-to-list 'doom-real-buffer-functions #'+ein-buffer-p nil #'eq)

  ;; Ace-link on notebook list buffers
  (after! ein-notebooklist
    (define-key ein:notebooklist-mode-map "o" #'+ein/ace-link-ein))

  ;; add hydra
  (defhydra +ein/hydra (:hint t :color red)
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
