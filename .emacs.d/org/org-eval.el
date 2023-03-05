;;; org-eval.el - Execute named org-mode blocks on load/save

;;
;; This file contains code which can be used to ensure that named
;; blocks are executed when org-mode files are visited, or saved after
;; edits
;;
;; To use these facilities define blocks like so in your org-mode files:
;;
;;
;; #+NAME: org-eval-load
;; #+BEGIN_SRC emacs-lisp :results output silent
;;   (message "I like cakes - on document loads - do you?")
;; #+END_SRC
;;
;; #+NAME: org-eval-save
;; #+BEGIN_SRC emacs-lisp :results output silent
;;   (message "I like cakes - just before a save - do you?")
;; #+END_SRC
;;
;; By default `org-mode` will prompt you to confirm that you want
;; execution to happen, but we use `org-eval-prefix-list` to enable
;; whitelisting particular prefix-directories, which means there is
;; no need to answer `y` to the prompt.
;;
;; It is possible to change the names of the blocks to execute,
;; for example I use the following configuration:
;;
;;    (use-package org-eval
;;      :defer 2
;;      :init
;;        (setq org-eval-prefix-list (list (expand-file-name "~/Private/"))
;;              org-eval-loadblock-name "skx-startblock"
;;              org-eval-saveblock-name "skx-saveblock" ))
;;
;; This means blocks named `skx-startblock` will be executed when files
;; are loaded from beneath ~/Private, and on-save the block named skx-saveblock
;; will be executed.
;;


;;; Configuration

(defvar org-eval-prefix-list
  (list nil)
  "A list of directory-prefixes beneath which org-files can be evaluated without prompting.")

(defvar org-eval-loadblock-name
  "org-eval-load"
  "The name of the block to execute when an org-file is visited.")

(defvar org-eval-saveblock-name
  "org-eval-save"
  "The name of the block to execute when an org-file is saved.")



;;; Code

(defun org-eval-loadblock ()
  "Execute the block with name `org-eval-loadblock-name`, if present."
  (if org-eval-loadblock-name
      (org-eval-execute-named-block org-eval-loadblock-name)))

(defun org-eval-saveblock ()
  "Execute the block with name `org-eval-saveblock-name`, if present."
  (if org-eval-saveblock-name
      (org-eval-execute-named-block org-eval-saveblock-name)))


(defun org-eval-test-safe-file (path)
  "Return true if the current file is safe to evaluate code from within.

This is done by testing the path against the list of prefix-directories
stored within `org-eval-prefix-list`."
  (delq nil (mapcar (lambda(x) (string-match x path )) org-eval-prefix-list)))



(defun org-eval-execute-named-block(name)
  "Execute the named block, if it exists, within the current file."
  (save-excursion
    (org-save-outline-visibility t
      (if (member name (org-babel-src-block-names))
          (if (org-eval-test-safe-file (buffer-file-name))
              (progn
                (setq-local org-confirm-babel-evaluate nil)
                (org-babel-goto-named-src-block name)
                (org-babel-execute-src-block))
            (message "%s did not match against the entries in org-eval-prefix-list, not evaluating code-blocks" (buffer-file-name)))))))



;; Load the start-block on startup
(add-hook 'org-mode-hook 'org-eval-loadblock)

;; evaluation the save-block on save
(defun org-eval-before-save-hook-eval ()
  (when (eq major-mode 'org-mode)
    (org-eval-saveblock)))

(add-hook 'before-save-hook #'org-eval-before-save-hook-eval)

;; All done
(provide 'org-eval)
