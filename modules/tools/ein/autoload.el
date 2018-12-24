;;; tools/ein/autoload.el -*- lexical-binding: t; -*-

;; FIXME obsolete :ein-notebook-dir
;;;###autoload
(def-setting! :ein-notebook-dir (dir)
  "Set the default directory from where to open Jupyter notebooks."
  `(setq ein:jupyter-default-notebook-directory ,dir))


;;
;; Library

(defun +ein--collect-ein-buffer-links ()
  (let ((end (window-end))
        points)
    (save-excursion
      (goto-char (window-start))
      (while (re-search-forward "~?/.+\\|\s\\[" end t)
        (push (+ (match-beginning 0) 1) points))
      (nreverse points))))

;;;###autoload
(defun +ein/ace-link-ein ()
  "Ace jump to links in ein notebooklist."
  (interactive)
  (require 'avy)
  (let ((res (avy-with +ein/ace-link-ein
               (avy--process
                (+ein--collect-ein-buffer-links)
                #'avy--overlay-pre))))
                ;(avy--style-fn avy-style)))))
    (when (numberp res)
      (goto-char (1+ res))
      (widget-button-press (point)))))

;;;###autoload (autoload '+ein-hydra/body "tools/ein/autoload" nil nil)
(defhydra +ein-hydra (:hint t :color red)
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
    ("x" ein:notebook-close :exit t))
