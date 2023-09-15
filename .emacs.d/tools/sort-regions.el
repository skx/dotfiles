;;; sort-regions.el -- Sort regions between a pair of deliminators

;; This is a cute idea that came up in discussions internally.
;;
;; Update terraform configuration values to say:
;;
;;   users = [
;;     # sort-start
;;     "alex..",
;;     "bob..",
;;     # sort-end
;;   ]
;;
;; Then add the hook, or otherwise call the function, and the region will
;; be sorted.
;;


(defgroup sort-regions ()
  "Options for `sort-regions'."
  :prefix "sort-regions-")


(defcustom sort-regions-marker-start "sort-start"
  "The string marking the start of a region that can be auto-sorted."
  :type 'string
  :group 'sort-regions)

(defcustom sort-regions-marker-end "sort-end"
  "The string marking the end of a region that can be auto-sorted."
  :type 'string
  :group 'sort-regions)



(defun sort-regions()
  "Sort the text bound between two lines containing markers."
  (interactive)
  (let (start end)
    (save-excursion
      (goto-char (point-min))
      ;; Search for the starting marker
      (while (search-forward sort-regions-marker-start nil t)
        (setq start (line-beginning-position 2)) ; line after
        ;; Search for the ending marker
        (when (search-forward sort-regions-marker-end nil t)
          (setq end (line-beginning-position)) ; this line
          ;; Sort the lines between the start and end markers
          (sort-lines nil start end))))))



;;;###autoload
(define-minor-mode auto-sort-regions-mode
  "Automatically apply `sort-regions` to files in this mode."
  :lighter " SR"
  (if (eq auto-sort-regions-mode t)
      (add-hook 'write-contents-functions
                #'sort-regions)
    (remove-hook 'write-contents-functions
                 #'sort-regions)))

;; All done
(provide 'sort-regions)
