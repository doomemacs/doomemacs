;;; lang/latex/+modeline.el -*- lexical-binding: t; -*-


(def-modeline-segment! +latex-selection-info
  "Information about the current selection customized for LaTeX
buffers, such as how many characters, words and lines are
selected, or the NxM dimensions of a block selection."
  (when (and (active) (or mark-active (eq evil-state 'visual)))
    (let ((reg-beg (region-beginning))
          (reg-end (region-end)))
      (propertize
       (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max))))
             (words (count-words reg-beg (min (1+ reg-end) (point-max)))))
         (cond ((or (bound-and-true-p rectangle-mark-mode)
                    (eq 'block evil-visual-selection))
                (let ((cols (abs (- (doom-column reg-end)
                                    (doom-column reg-beg)))))
                  (format "%dx%dB" lines cols)))
               ((eq 'line evil-visual-selection)
                (format "%dL" lines))
               ((> lines 1)
                (format "%dC %dL %dW" (- (1+ reg-end) reg-beg) lines words))
               (t
                (format "%dC %dW" (- (1+ reg-end) reg-beg) words))))
       'face 'doom-modeline-highlight))))

(def-modeline! latex-main
  (bar matches " " buffer-info "  %l:%c %p  " +latex-selection-info)
  (buffer-encoding major-mode vcs flycheck))
