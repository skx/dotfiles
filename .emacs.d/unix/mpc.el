;;
;;  Steve's minor playlist.
;;


;;
;;  The path to the client.
;;
(defvar mpc-path "The path to our MPC client")
(setq mpc-path "/usr/bin/mpc" )

;; The keymap
(defvar mpc-mode-map () "Keymap used in `mpc` buffers.")

(when (not mpc-mode-map)
  (setq mpc-mode-map (make-sparse-keymap))
  (define-key mpc-mode-map [?q] 'bury-buffer)
  (define-key mpc-mode-map [?n] 'mpc-play-next)
  (define-key mpc-mode-map [?p] 'mpc-play-prev)
  (define-key mpc-mode-map [?r] 'mpc-refresh-display)
  (define-key mpc-mode-map "\C-m" 'mpc-play-cur)
  (define-key mpc-mode-map [?\+] 'mpc-louder)
  (define-key mpc-mode-map [?\-] 'mpc-quieter)
  (define-key mpc-mode-map [? ] 'mpc-pause)
)


;;
;;  Launch the client
;;
(defun mpc ()
  "Major mode to interact with mpd via the mpc client."
  (interactive)
  (mpc-insert)
)


;;
;; Insert the menu + playlist.
;;
(defun mpc-insert()
  "Insert the menu + current playlist"
  (pop-to-buffer (get-buffer-create "*mpc*"))
  (kill-all-local-variables)
  (let ((buffer-read-only nil))
      (delete-region (point-min) (point-max))
      (insert "mpc :")
      (insert "n == next | p == prev | spc = pause\n\n")
      (insert (mpc-get-playlist))
      )
  (setq major-mode 'mpc)
  (setq mode-name "MPC")
  (use-local-map mpc-mode-map)
  (beginning-of-buffer)
  (re-search-forward "^>" (point-max) t)
  (require 'hl-line)
  (hl-line-mode t)
  (toggle-read-only t)
  )


;;
;; Return the currently loaded playlist.
;;
(defun mpc-get-playlist()
  "Return the currently loaded playlist"
  (shell-command-to-string
   (format "%s playlist --format='%%position%% %%file%%'" mpc-path)))


;;
;; Refresh the display.
;;
(defun mpc-refresh-display()
  "Refresh the screen"
  (interactive)
  (mpc-insert))


;;
;;  Next
;;
(defun mpc-play-next()
  "Move to the next track in the playlist"
  (interactive)
  (shell-command
   (format "%s next" mpc-path))
  (mpc-insert)
  (message "Moved to the next track"))

;;
;;  Pause
;;
(defun mpc-pause()
  "Toggle the pause"
  (interactive)
  (shell-command
   (format "%s toggle" mpc-path))
  (mpc-insert)
  (message "Toggled pause"))

;;
;; Prev
;;
(defun mpc-play-prev()
  "Move to the previous track in the playlist"
  (interactive)
  (shell-command
   (format "%s prev" mpc-path))
  (mpc-insert)
  (message "%s" "Moved to the previous track"))

;;
;; Play current
;;
(defun mpc-play-cur()
  "Move to the current position in the playlist"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (list)
      (re-search-forward "[0-9]+" (- (point-at-eol) 1) t)
      (call-process-shell-command
       (format "%s play %s" mpc-path (match-string-no-properties 0)))))
  (mpc-insert)
  )


;;
;; Volume tweaking
;;
(defun mpc-louder()
  "Make the volume louder"
  (interactive)
  (shell-command
   (format "%s volume +5 >/dev/null" mpc-path))
  (message "%s" "Louder .."))


(defun mpc-quieter()
  "Make the volume quieter."
  (interactive)
  (shell-command
   (format "%s volume -5 >/dev/null" mpc-path))
  (message "%s" "Quieter .."))

;;
;;  All done
;;
(provide 'mpc)
