;;; mode-line-major.el -- Hide minor modes from the mode-line

;;; About
;;
;; Remove all minor-modes from the mode-line.
;;
;; This will ensure that only major modes are present, as well as any
;; line/columns the user prefers.
;;

;;; Installation
;;
;; Ensure this file is somewhere upon your load-path, then enable
;;
;;   (require 'mode-line-major)
;;
;; If you're using `use-package' then you can achieve the same effect
;; with:
;;
;;   (use-package mode-line-major)
;;
;; In either case that is all you need to do, there are zero configuration
;; options supported by this package.
;;


(defun mode-line-major--minor-modes ()
  "Get a list of which minor modes which are available."
  (let ($list)
    (mapc (lambda ($mode)
            (condition-case nil
                (if (symbolp $mode)
                    (setq $list (cons $mode $list)))
              (error nil)))
          minor-mode-list)
    (sort $list 'string<)))


(defun mode-line-major-cleanup ()
  "Remove all minor-modes from the mode-line.

The end result will be keeping only the major-modes."
  (interactive)
  (mapc
    (lambda (mode)
       (let* ((old-mode-str (cdr (assq mode minor-mode-alist))))
           (when old-mode-str
              (setcar old-mode-str ""))
           (when (eq mode major-mode)
             (setq mode-name mode-str))))
    (mode-line-major--minor-modes)))

;; Ensure that we're enabled
(add-hook 'after-change-major-mode-hook 'mode-line-major-cleanup)

;; Provide the package
(provide 'mode-line-major)
