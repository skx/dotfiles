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
  (define-key mpc-mode-map [?R] 'mpc-toggle-random)
  (define-key mpc-mode-map "\C-m" 'mpc-play-cur)
  (define-key mpc-mode-map [?\+] 'mpc-louder)
  (define-key mpc-mode-map [?\-] 'mpc-quieter)
  (define-key mpc-mode-map [? ] 'mpc-pause)
  (define-key mpc-mode-map [??] 'mpc-help)
  (define-key mpc-mode-map [?h] 'mpc-help)
)


;;
;;  Launch the client
;;
(defun mpc ()
  "Major mode to interact with mpd via the mpc client."
  (interactive)
  (mpc-insert)
)


(defun mpc-help()
  "Show the keybindings in the current mode."
  (interactive)
  (describe-bindings)
  (pop-to-buffer (get-buffer-create "*Help*"))
  (search-forward "Major Mode Bindings"))

;;
;; Insert the menu + playlist.
;;
(defun mpc-insert()
  "Insert the menu + current playlist"
  (pop-to-buffer (get-buffer-create "*mpc*"))
  (kill-all-local-variables)
  (let ((buffer-read-only nil))
      (delete-region (point-min) (point-max))
      (insert (mpc-get-overview))
      (insert "\n\n")
      (save-excursion
        (insert (mpc-get-playlist)))
      (forward-line (mpc-currently-playing))
      (forward-line -1)
      )
  (setq major-mode 'mpc)
  (setq mode-name "MPC")
  (use-local-map mpc-mode-map)
  (require 'hl-line)
  (hl-line-mode t)
  (toggle-read-only t)
  )


(defun mpc-get-overview ()
  "Return the status of the server, this includes the current
volume the status of repeat, random, etc."
  (interactive)
  (nth 2 (split-string (shell-command-to-string (format "%s status" mpc-path)) "\n")))


;;
;; Return the currently loaded playlist.
;;
(defun mpc-get-playlist()
  "Return the currently loaded playlist"
  (shell-command-to-string
   (format "%s playlist --format='%%position%%\t%%file%%'" mpc-path)))


(defun mpc-currently-playing()
  "Return the number of the currently playing track"
  (string-to-number (shell-command-to-string
    (format "%s current --format='%%position%%'" mpc-path))))

;;
;; Refresh the display.
;;
(defun mpc-refresh-display()
  "Refresh the screen"
  (interactive)
  (mpc-insert))


(defun mpc-toggle-random()
  "Toggle the state of the random flag"
  (interactive)
  (shell-command-to-string(format "%s random" mpc-path))
  (mpc-refresh-display))

;;
;;  Next
;;
(defun mpc-play-next()
  "Move to the next track in the current playlist."
  (interactive)
  (shell-command
   (format "%s next" mpc-path))
  (mpc-insert)
  (message "Moved to the next track"))

;;
;;  Pause
;;
(defun mpc-pause()
  "Toggle the state of the play/pause flag."
  (interactive)
  (shell-command
   (format "%s toggle" mpc-path))
  (mpc-insert)
  (message "Toggled pause"))

;;
;; Prev
;;
(defun mpc-play-prev()
  "Move to the previous track in the current playlist."
  (interactive)
  (shell-command
   (format "%s prev" mpc-path))
  (mpc-insert)
  (message "%s" "Moved to the previous track"))

;;
;; Play current
;;
(defun mpc-play-cur()
  "Move to the current position in the playlist."
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
  "Make the volume louder."
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
