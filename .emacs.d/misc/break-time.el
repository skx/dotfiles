;;; break-time.el --- encourage regular breaks.

;;
;; Copyright (C) 2020 Steve Kemp
;;
;; Version: 0.1
;; Keywords: break, stretch
;; Author: Steve Kemp <steve@steve.fi>
;;

;; This file is not (YET) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

(defvar break-time-messages '("Take a break!"
                              "Take a break, and have a stretch!"
                              "Take a break, and drink more water!"))


(defvar break-time-interval (* 30 60)
  "The period in which to show a break, in seconds.")

(setq break-timer nil)

(defun break-time-announce ()
  "Announce a random message from the ‘break-time-messages‘ list to the user.

  We block for confirmation, via 'q', after the animated display."
  ;; kill the old buffer, because if not it will be in view-mode
  ;; which means it will be read-only and we cannot add the message there
  (if (get-buffer "*breaktime*") (kill-buffer "*breaktime*"))
  (switch-to-buffer (get-buffer-create "*breaktime*"))
  (erase-buffer)

  ;; pick a random message, and then prepend the time to it.
  (let ((msg (nth (random (length break-time-messages)) break-time-messages)))
    (setq msg (format "%s - %s" (format-time-string "%H:%M" (current-time)) msg))
    ;; animate, but make sure the result is centered.
    (animate-string msg  (/ (window-height) 2) (- (/ (window-width) 2) (/ (length msg) 2) ))
  (view-mode)))

(defun break-time-start()
  "Setup a timer to announce break-times on a regular schedule.

 By default these notifications repeat every 30 minutes, but the period may
be adjusted, via ‘break-time-interval‘."
  (interactive)
  ;; cancel any existing timer
  (if break-timer (break-time-end))
  ;; setup the new timer
  (setq break-timer (run-at-time t break-time-interval 'break-time-announce))
  (message "Break-notifications will repeat every %d minutes" (/ break-time-interval 60)))

(defun break-time-end()
  "Cancel any open break-timer, disabling the scheduled break-time messages."
  (interactive)
  ;; if the timer is active, cancel.  otherwise let the user know"
  (if break-timer
      (cancel-timer break-timer)
    (message "Break timer is not active!"))
  (setq break-timer nil))

(provide 'break-time)
