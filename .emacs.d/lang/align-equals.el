
(defun align-equals (begin end)
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1))


(global-set-key (kbd "M-=") 'align-equals)

(provide 'align-equals)
