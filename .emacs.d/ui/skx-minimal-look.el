;;
;; Hide the menu-bar & toolbar unless the pointer is at the top of the frame.
;;
;;



;; Disable the scroll-bar(s).
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable the tool-bar.
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))

;; Disable the menu.
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))


;;
;; If the point is at the top of the frame then show things.
;;
(defun menu-bar-show-hide-helper ()
  (if (< (cddr (mouse-pixel-position)) 20)
      (progn
        (tool-bar-mode 1)
        (menu-bar-mode 1))
    (menu-bar-mode 0)
    (tool-bar-mode 0)))

;;
;; Configure a timer to call the previous function.
;;
;;(defun menu-bar-show-hide ()
;;  (run-with-idle-timer 0.1 t 'menu-bar-show-hide-helper))

;;
;; Call once to set the initial state.
;;
(menu-bar-show-hide)



;;
;; Make us loadable
;;
(provide 'skx-minimal-look)
