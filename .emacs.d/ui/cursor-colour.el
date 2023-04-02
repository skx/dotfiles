;;; cursor-colour.el - Change cursor colour intelligently

;; Once this package is loaded the cursor colour will change intelligently:
;;
;;  1.  By default the cursor is WHITE.
;;
;;  2.  If the buffer is read-only it will be RED.
;;
;;  3.  If overwrite mode is active it will be BLUE.
;;
;; Taken from here:
;;
;;    https://www.emacswiki.org/emacs/ChangingCursorDynamically
;;


(defvar my/cursor-colour "")
(defvar my/cursor-color-buffer "")

(defun my/set-cursor-color-intelligently ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((colour
         (if buffer-read-only "red"
           (if overwrite-mode "blue"
             "white"))))
        (unless (and
                 (string= colour my/cursor-colour)
                 (string= (buffer-name) my/cursor-colour-buffer))
          (set-cursor-color (setq my/cursor-colour colour))
          (setq my/cursor-colour-buffer (buffer-name)))))

;; After a command update things, if required.
(add-hook 'post-command-hook 'my/set-cursor-color-intelligently)

;; All done
(provide 'cursor-colour)
