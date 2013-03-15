(require 'time-date)

(defvar emacs-up-time (current-time)
  "Time at which Emacs started up.")

(defun uptime ()
  "Displays the uptime of GNU Emacs."
  (interactive)
  (let* ((now (current-time))
         (second (floor (- (time-to-seconds now)
                           (time-to-seconds emacs-up-time))))
         (minute (floor second 60))
         (hour (floor minute 60))
         (day (floor hour 24)))
    (message "Emacs up %s day(s), %02d:%02d"
              day (% hour 24) (% minute 60))))


(provide 'uptime)