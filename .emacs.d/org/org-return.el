;;; org-return.el - Follow links, and more, via RETURN

;;
;; org-mode supports the use of `org-return-follows-links' but this does
;; not work inside table cells.
;;
;; This package resolves that issue, and adds support for toggling the state
;; of checkboxes via RETURN too.
;;
;; Installation:
;;
;;  Add to your load-path then setup via use-package:
;;
;;   (use-package org-return
;;      :defer 2
;;      :after org
;;      :config
;;       (add-hook 'org-mode-hook
;;          (lambda ()
;;            (local-set-key (kbd "RET") 'org-return-handler))))
;;
;;
;; Or handle manually:
;;
;;  (require 'org-return)
;;  (add-hook 'org-mode-hook
;;          (lambda ()
;;            (local-set-key (kbd "RET") 'org-return-handler)))


;; RETURN will follow links in org-mode files
(setq org-return-follows-link t)



(defun org-return--has-text-property (val)
  "Return true if the point is over an item with the specified text-property."
  (let ((tprop (get-text-property (point) 'face)))
    (if (or (eq tprop val) (and (listp tprop) (memq val tprop)))
       t nil)))

(defun org-return-is-link ()
  "Is the point over a link?"
  (org-return--has-text-property 'org-link))

(defun org-return-is-checkbox()
  "Is the point over a checkbox?"
  (org-return--has-text-property 'org-checkbox))


(defun org-return-handler()
  "Allow following links, even inside tables, and toggling checkboxes via RET"
  (interactive)
  (cond
     ((org-return-is-checkbox) (org-toggle-checkbox))
     ((org-return-is-link)     (call-interactively 'org-open-at-point))
     (t (org-return))))


(provide 'org-return)
