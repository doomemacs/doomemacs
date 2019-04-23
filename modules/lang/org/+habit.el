;;; lang/org/+habit.el -*- lexical-binding: t; -*-

(defvar +org-habit-graph-padding 2
  "The padding added to the end of the consistency graph")

(defvar +org-habit-min-width 30
  "Hides the consistency graph if the `org-habit-graph-column' is less than this value")

(defvar +org-habit-graph-window-ratio 0.3
  "The ratio of the consistency graphs relative to the window width")

(defun +org-habit|resize-graph()
  "Right align and resize the consistency graphs based on `+org-habit-graph-window-ratio'"
  (require 'org-habit)
  (let* ((total-days (float (+ org-habit-preceding-days org-habit-following-days)))
         (preceding-days-ratio (/ org-habit-preceding-days total-days))
         (graph-width (floor (* (window-width) +org-habit-graph-window-ratio)))
         (preceding-days (floor (* graph-width preceding-days-ratio)))
         (following-days (- graph-width preceding-days))
         (graph-column (- (window-width) (+ preceding-days following-days)))
         (graph-column-adjusted (if (> graph-column +org-habit-min-width)
                                    (- graph-column +org-habit-graph-padding)
                                  nil)))
    (setq-local org-habit-preceding-days preceding-days)
    (setq-local org-habit-following-days following-days)
    (setq-local org-habit-graph-column graph-column-adjusted)))

(add-hook 'org-agenda-mode-hook #'+org-habit|resize-graph)
