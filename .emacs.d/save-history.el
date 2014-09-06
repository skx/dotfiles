


;;
;; Save mini-buffer history if ~/.history.d exists
;;
;; We save to ~/.history.d/emacs.$USER
;;
(if (file-directory-p (expand-file-name "~/.history.d"))
    (progn
      (setq savehist-file (concat (expand-file-name "~/.history.d/") "emacs." (getenv "USER")))
      (savehist-mode 1)))

;;
;; Save recent files too.
;;
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


(provide 'save-history)