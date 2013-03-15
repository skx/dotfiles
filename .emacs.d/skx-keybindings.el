;;
;; M-g = goto-line
;;
(global-set-key "\M-g" 'goto-line)

;;
;;  Search by regexp
;;
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)

;;
;; C-x k == Kill current buffer, without prompting.
;;
(global-set-key "\C-xk"
		'(lambda ()
                   (interactive)
                   (if (and (fboundp 'gnuserv-buffer-p) (gnuserv-buffer-p (current-buffer)))
                       (gnuserv-buffer-done (current-buffer))
                     (kill-buffer (current-buffer)))))


;;
;; Prevent accidentally killing emacs.
;;
(global-set-key [(control x) (control c)]
		'(lambda ()
		   (interactive)
		   (if (y-or-n-p-with-timeout "Do you really want to exit Emacs ? " 10 nil)
                       (progn
                         (if window-system
                             (progn
                               (uptime)
                               (sleep-for 1)))
                         (save-buffers-kill-emacs)))
                     (message "emacs quit aborted")))





(provide 'skx-keybindings)