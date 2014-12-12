;;; hide-mode-line.el --- Hides the mode line when there is only one frame and
;;; one buffer.
;;
;; Filename: hide-mode-line.el
;; Description: Hides the mode line when there is only one frame and one
;;              buffer.
;; Author: Darren Embry
;; Copyright (c) 2008, 2011 Darren Embry
;; URL: http://webonastick.com/emacs-lisp/hide-mode-line.el
;; Keywords: mode line, writeroom
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.
;;
;; GPL 2 is available here:
;;   http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basically, automatically hides the mode-line if all of the following
;; are true:
;; - there is only one frame.
;; - there is only one window displayed in that frame.
;; - there is no minibuffer.
;; - the hide-mode-line variable is set.
;; and automatically shows the mode-line when any of the above isn't true.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HOW TO USE
;;
;; Just put this file in your Emacs library directory and add this line to
;; your ~/.emacs:
;;
;;   (autoload 'hide-mode-line "hide-mode-line" nil t)
;;
;; and use M-x hide-mode-line to toggle.  Setting the hide-mode-line variable
;; won't automatically update the buffers' mode-line visibilities.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MYSTERY BUG: every once in a while a few lines of text will be hidden
;; for some reason until you do a redraw-display.  See if you can
;; reproduce this in a reliable fashion!
;;
;; MYSTERY BUG: not specific to this module, but...  load linum, run M-x
;; linum-mode, then (setq mode-line-format nil) this triggers display
;; problems more reproducibly: sometimes the last line in the buffer
;; doesn't have the line number show up; and sometimes the cursor line
;; or the one after it doesn't have the line number show up.  May be
;; related to above bug.
;;
;; CAVEAT: this code does not instruct your window system to make the
;; window full-screen.
;;
;; TODO: briefly show modeline for (example) 2 seconds when the following
;; happens:
;; - hide-mode-line is about to be activated
;; - you switch to another buffer
;;
;; TODO: Emacs 21 does not implement window-tree.
;;
;; BUG: if the hide-mode-line-window-configuration-change-hook function
;; displays a (message "moo") before it does its work, the screen is blanked
;; when you resize the window until you hit C-l.
;;
;; BUG: if a frame is closed and there is only one frame remaining, and
;; there is only one buffer in that window, mode lines are not hidden.
;;
;; SEE ALSO:
;; http://www.emacswiki.org/cgi-bin/wiki/LineNumbers
;; http://www.emacswiki.org/cgi-bin/wiki/WriteRoom
;;
;;=============================================================================

;;; History:
;;
;; 2008-01-31 r3090 initial version
;; 2008-02-01 r3097 explicitly defint default for
;;                  hide-mode-line-saved-mode-line-format
;; 2008-02-01 r3100 implement hide-mode-line-unaffected-by-minibuffer
;; 2008-02-01 r3101 more robust handling of case where mode-line-format is
;;                  nil before this code runs
;; 2008-02-01 r3106 disable in emacs21: window-tree function not available
;; 2011-03-08 r5835 fix emacsw32 bug

;;; Code:

(defvar hide-mode-line-saved-mode-line-format nil)
(make-variable-buffer-local 'hide-mode-line-saved-mode-line-format)
; TODO: add a hook of some kind when setting mode-line-format.

(defvar hide-mode-line nil)
; TODO: add a hook to run hide-mode-line-update when setting hide-mode-line.
; [or just use M-x hide-mode-line for now]

(defcustom hide-mode-line-unaffected-by-minibuffer nil
  "If non-nil, a minibuffer by itself does not un-hide the modeline."
  :group 'hide-mode-line
  :type  'boolean)

(defun there-is-only-one-frame ()
  "Return non-nil if there is only one frame, nil otherwise."
  (let ((frames (frames-on-display-list)))
    (if (= (length frames) 1)
	(car frames)
      nil)))
(defun there-is-only-one-window-in (frame)
  "Return non-nil if there is only one window in the specified FRAME."
  (let ((root (car (window-tree frame)))) ;FIXME: does not work with emacs21
    (not (listp root))))
(defun there-is-only-one-frame-and-one-window ()
  "Return non-nil if there is only one frame and one window."
  (let ((the-only-frame (there-is-only-one-frame)))
    (and the-only-frame
	 (or hide-mode-line-unaffected-by-minibuffer
	     (= (minibuffer-depth) 0))
	 (there-is-only-one-window-in the-only-frame))))

(defun hide-mode-line-in (buffer)
  "Hide the specified BUFFER's mode line.

Saves the buffer's previous `mode-line-format' value if it's not
already hidden."
  (with-current-buffer buffer
    (if (and (not hide-mode-line-saved-mode-line-format)
	     ;; minibuffers don't have modelines :p
	     (not (minibufferp buffer)))
	(progn (setq hide-mode-line-saved-mode-line-format
		     (list mode-line-format))
	       (setq mode-line-format nil)
	       ;; bug workaround
	       (redraw-modeline)))))
(defun show-mode-line-in (buffer)
  "If the specified BUFFER's mode line is hidden, un-hides it.

Restores the buffer's `mode-line-format' from what was saved when
hide-mode-line-in was called."
  (with-current-buffer buffer
    (if (and hide-mode-line-saved-mode-line-format
	     ;; minibuffers don't have modelines :p
	     (not (minibufferp buffer)))
	(progn (setq mode-line-format
		     (car hide-mode-line-saved-mode-line-format))
	       (setq hide-mode-line-saved-mode-line-format nil)))))

(defun hide-mode-lines ()
  "Hide all buffers' mode lines using hide-mode-line-in."
  (mapcar 'hide-mode-line-in (buffer-list)))
(defun show-mode-lines ()
  "Show all buffers' mode lines using show-mode-line-in."
  (mapcar 'show-mode-line-in (buffer-list))
  (if (equal window-system 'w32)
      ;; bug workaround
      (redraw-display)))

(defun hide-mode-line-update ()
  "Update the state of all buffers' mode lines.

This uses hide-mode-lines or show-mode-lines."
  (if hide-mode-line
      (if (there-is-only-one-frame-and-one-window)
	  (hide-mode-lines)
	(show-mode-lines))
    (show-mode-lines)))

(defun hide-mode-line-minibuffer-setup-hook ()
  "Internal function."
  (hide-mode-line-update))
(defun hide-mode-line-minibuffer-exit-hook ()
  "Internal function."
  (hide-mode-line-update))
(defun hide-mode-line-make-frame-function (new-frame)
  "Internal function."
  (hide-mode-line-update))
(defun hide-mode-line-delete-frame-function (dead-frame-walking)
  "Internal function."
  (hide-mode-line-update))
(defun hide-mode-line-window-configuration-change-hook ()
  "Internal function."
  (hide-mode-line-update))

(defun hide-mode-line-add-hooks ()
  "Internal function."
  (interactive)
  (add-hook 'minibuffer-setup-hook
	    'hide-mode-line-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook
	    'hide-mode-line-minibuffer-exit-hook)
  (add-hook 'after-make-frame-functions
	    'hide-mode-line-make-frame-function)
  (add-hook 'delete-frame-functions
	    'hide-mode-line-delete-frame-function)
  (add-hook 'window-configuration-change-hook
	    'hide-mode-line-window-configuration-change-hook))

(defun hide-mode-line-remove-hooks ()
  "Internal function."
  (interactive)
  (remove-hook 'minibuffer-setup-hook
	       'hide-mode-line-minibuffer-setup-hook)
  (remove-hook 'minibuffer-exit-hook
	       'hide-mode-line-minibuffer-exit-hook)
  (remove-hook 'after-make-frame-functions
	       'hide-mode-line-make-frame-function)
  (remove-hook 'delete-frame-functions
	       'hide-mode-line-delete-frame-function)
  (remove-hook 'window-configuration-change-hook
	       'hide-mode-line-window-configuration-change-hook))

;;;###autoload
(defun hide-mode-line ()
  "Toggle the hide-mode-line functionality."
  (interactive)
  (if (functionp 'window-tree)
      (progn
        (if hide-mode-line
            (hide-mode-line-remove-hooks)
          (hide-mode-line-add-hooks))
        (setq hide-mode-line (not hide-mode-line))
        (hide-mode-line-update))
    (error (concat "Your Emacs does not provide the window-tree function.  "
                   "Please upgrade to GNU Emacs 22 "
                   "or to some other version of Emacs that provides it."))))

(provide 'hide-mode-line)

;;; hide-mode-line.el ends here

