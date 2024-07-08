;;; shortcuts-mode.el --- Minor mode providing a buffer shortcut bar    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Peter Amstutz

;; Author: Peter Amstutz <tetron@interreality.org>
;; Keywords: lisp
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/tetron/shortcuts-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a minor mode which adds a sticky window to the top of the
;; frame listing the last ten buffers that were accessed.  You can
;; then instantly switch the current window to one of the recent
;; buffers using C-1 through C-0.  Think of it as an extremely
;; lightweight tab bar oriented around keyboard navigation.
;;
;; The shortcut bar also supports basic mouse navigation.  Left click
;; switches the current window to the selected buffer, and middle
;; click kills the selected buffer.
;;
;; As a special case, certain utility buffers (*Buffer List*,
;; *Ibuffer*, the *shortcuts* buffer itself) are excluded from the top
;; bar.  Dired buffers are also filtered, because otherwise navigating
;; the filesystem through Dired (which creates a new buffer for each
;; directory) tends to fill up all the top slots.

;;; Code:

;; The following "sticky" function was borrowed from the
;; Emacs wiki:
;;
;; https://www.emacswiki.org/emacs/sticky-windows.el
;;
;; This is required so that "C-x 1" doesn't cause the shortcut window
;; to go away.
;;
;; Code posted to the wiki is GPL 2

;;;###autoload
(defun shortcuts-sticky-window-delete-other-windows ()
  "Delete all other windows unless marked with `window-dedicated-p`."
  (interactive)
  (mapcar (lambda (window)
			(if (not (window-dedicated-p window))
				(delete-window window)))
		  (cdr (window-list))))

(defun shortcuts-shrink-string (p n)
  "Shrink string P to be at most N characters in length.
Removes the middle of the string if necessary and replaces with ellipses."
  (let ((p2 (* 2 (/ p 2))))
    (if (> (length n) (1+ p2))
	(concat (substring n 0 (/ p 2)) "…" (substring n (/ p -2) nil))
      n)))

(defun shortcuts-filtered-buffer-list ()
  "Remove utility and Dired buffers from the list of buffers to be displayed."
  (cons nil (seq-remove (lambda (e)
		(or (string-prefix-p " " (buffer-name e))
		    (string= (buffer-name e) "*shortcuts*")
		    (string= (buffer-name e) "*Ibuffer*")
		    (string= (buffer-name e) "*Buffer List*")
		    (string= (buffer-local-value 'major-mode e) "dired-mode")))
	      (buffer-list))))

(defun shortcuts-switch-to (n)
  "Switch current window to buffer N in the shortcuts bar."
  (switch-to-buffer (elt (shortcuts-filtered-buffer-list) n)))

(defun shortcuts-goto (@click)
  "Process mouse event @CLICK to switch current window to the selected buffer."
  (interactive "e")
  (let ((shortcut nil))
    (with-current-buffer "*shortcuts*"
      (setq shortcut (get-text-property (posn-point (event-start @click)) 'shortcut-target)))
    (shortcuts-switch-to shortcut)))

(defun shortcuts-close (@click)
  "Process mouse event @CLICK to kill the selected buffer."
  (interactive "e")
  (let ((shortcut nil))
    (with-current-buffer "*shortcuts*"
      (setq shortcut (get-text-property (posn-point (event-start @click)) 'shortcut-target)))
    (kill-buffer (elt (shortcuts-filtered-buffer-list) shortcut))))

(defun shortcuts-insert (b num shortcuts-width cols)
  "Render a single shortcut on the bar for buffer B in shortcut position NUM.
SHORTCUTS-WIDTH is the width of the window and COLS in the number of columns."
  (let* ((md (buffer-local-value 'major-mode (elt b num)))
	 (width (/ shortcuts-width cols))
	 (avail (- width 6))
	 (modewidth (- (/ avail 3) 2))
	 (modename (substring (format "%s" md) 0 -5))
	 (mdstr (if (> (+ (length (buffer-name (elt b num))) (length modename)) avail)
		    (concat "(" (shortcuts-shrink-string modewidth modename) ")")
		  (format "(%s)" modename)))
	 (bufstr (shortcuts-shrink-string (- avail (length mdstr)) (buffer-name (elt b num))))
	 (txt (format "C-%d %s%s%s "
		      (% num 10)
		      bufstr
		      (make-string (- avail (length bufstr) (length mdstr) -1) ?\s)
		      mdstr))
	 (k (make-sparse-keymap))
	 (bufwin (get-buffer-window (elt b num))))
    (define-key k [mouse-1] 'shortcuts-goto)
    (define-key k [mouse-2] 'shortcuts-close)
    (put-text-property 0 (1- (length txt)) 'keymap k txt)
    (put-text-property 0 (1- (length txt)) 'mouse-face 'highlight txt)
    (put-text-property 0 (1- (length txt)) 'shortcut-target num txt)
    (if bufwin
	(put-text-property 0 (1- (length txt)) 'face 'bold txt))
    (insert txt)))

(defun shortcuts-update ()
  "Update the shortcuts buffer when something changed.  Executed as a hook."
  (let ((win (get-buffer-window "*shortcuts*")))
    (if win
	(let* ((shortcuts-buf (get-buffer-create "*shortcuts*"))
	       (b (shortcuts-filtered-buffer-list))
	       (num 0)
	       (shortcuts-width (window-body-width win))
	       (cols (min (/ shortcuts-width 25) 5)))
	  (with-current-buffer shortcuts-buf
	    (make-local-variable 'buffer-read-only)
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (setq num 1)
	    (while (and (< num (length b)) (<= num (* cols 2)))
	      (shortcuts-insert b num shortcuts-width cols)
	      (setq num (+ num 2)))
	    (insert ?\n)
	    (setq num 2)
	    (while (and (< num (length b)) (<= num (* cols 2)))
	      (shortcuts-insert b num shortcuts-width cols)
	      (setq num (+ num 2)))
	    (goto-char 0)
	    (setq buffer-read-only t))
	  (with-selected-window win
	    (shrink-window-if-larger-than-buffer))))))

(defun shortcuts-switch-to-1 ()
  "Switch to buffer in shortcut position 1."
  (interactive)
  (shortcuts-switch-to 1))
(defun shortcuts-switch-to-2 ()
  "Switch to buffer in shortcut position 2."
  (interactive)
  (shortcuts-switch-to 2))
(defun shortcuts-switch-to-3 ()
  "Switch to buffer in shortcut position 3."
  (interactive)
  (shortcuts-switch-to 3))
(defun shortcuts-switch-to-4 ()
  "Switch to buffer in shortcut position 4."
  (interactive)
  (shortcuts-switch-to 4))
(defun shortcuts-switch-to-5 ()
  "Switch to buffer in shortcut position 5."
  (interactive)
  (shortcuts-switch-to 5))
(defun shortcuts-switch-to-6 ()
  "Switch to buffer in shortcut position 6."
  (interactive)
  (shortcuts-switch-to 6))
(defun shortcuts-switch-to-7 ()
  "Switch to buffer in shortcut position 7."
  (interactive)
  (shortcuts-switch-to 7))
(defun shortcuts-switch-to-8 ()
  "Switch to buffer in shortcut position 8."
  (interactive)
  (shortcuts-switch-to 8))
(defun shortcuts-switch-to-9 ()
  "Switch to buffer in shortcut position 9."
  (interactive)
  (shortcuts-switch-to 9))
(defun shortcuts-switch-to-0 ()
  "Switch to buffer in shortcut position 10."
  (interactive)
  (shortcuts-switch-to 10))

;;;###autoload
(define-minor-mode shortcuts-mode
  "Toggle `shortcuts-mode`.
This is a minor mode which adds a sticky window to the top of the
frame listing the last ten buffers that were accessed.  You can
then instantly switch the current window to one of the recent
 buffers using C-1 through C-0."
  :global t
  :init-value nil
  :lighter nil
  :keymap '(([?\C-1] . shortcuts-switch-to-1)
	    ([?\C-2] . shortcuts-switch-to-2)
	    ([?\C-3] . shortcuts-switch-to-3)
	    ([?\C-4] . shortcuts-switch-to-4)
	    ([?\C-5] . shortcuts-switch-to-5)
	    ([?\C-6] . shortcuts-switch-to-6)
	    ([?\C-7] . shortcuts-switch-to-7)
	    ([?\C-8] . shortcuts-switch-to-8)
	    ([?\C-9] . shortcuts-switch-to-9)
	    ([?\C-0] . shortcuts-switch-to-0)
	    ([(control ?x) ?1] . shortcuts-sticky-window-delete-other-windows))
  :group 'convenience

  (if shortcuts-mode
      (let ((n (display-buffer-in-side-window (get-buffer-create "*shortcuts*") (list '(side . top)))))
	(set-window-dedicated-p n t)
	(set-window-parameter n 'no-other-window t)
	(with-selected-window n
	  (make-local-variable 'window-size-fixed)
	  (make-local-variable 'mode-line-format)
	  (setq mode-line-format (list "")))
	(add-hook 'buffer-list-update-hook #'shortcuts-update)
	(add-hook 'window-configuration-change-hook #'shortcuts-update))
    (progn
      (kill-buffer "*shortcuts*")
      (remove-hook 'buffer-list-update-hook #'shortcuts-update)
      (remove-hook 'window-configuration-change-hook #'shortcuts-update))))

(provide 'shortcuts-mode)

;;; shortcuts-mode.el ends here
