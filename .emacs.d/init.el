;;
;; ~/.emacs.d/init.md
;;
;; This is the driver which attempts to load, parse, and evaluate the
;; content of several Markdown files from beneath ~/.emacs.d/
;;
;; I started using Markdown for a literate emacs configuration file
;; a few years ago, but I'm slowly transitioning to using `org-mode`
;; instead.
;;
;; This init.el file handles both files for the moment, though in
;; the future it will only load the `org-mode` version(s) and the
;; markdown support will be removed.
;;



;; We load `org-mode` files, sometimes.
(require 'org)

;; Helper function.
(defun hostname()
  "Return the (unqualified) hostname for this system."
  (car (split-string (system-name) "\\." )))

;; Helper function.
(defun domainname()
  "Return the domain-name for this system, if it is possible to do so."
  (let ((name (cdr (split-string (system-name) "\\.")))
        (res nil))
    (dolist (item name)
       (if res
           (setq res (concat res "." (format "%s" item)))
         (setq res (concat res (format "%s" item)))))
    res))


(defun load-markdown-init (path)
  "If the specified file exists then attempt to load it.

We treat the file as markdown, and we extract code-blocks from it which we
execute directly.
"
  ;; if the file exists
  (if (file-exists-p (expand-file-name path))
      (with-temp-buffer
        ;; insert into temporary buffer
        (insert-file-contents (expand-file-name path))

        ;; Go to the start of the file.
        (goto-char (point-min))

        ;; Until we're at the end of the file ...
        (while (not (eobp))

          ;; Jump forward a line.
          (forward-line 1)

          ;; Skip to the start of the code block.
          (re-search-forward "^```lisp$" (point-max) t)

          (let ((l (match-end 0)))
            ;; Find the end of the code-block
            (re-search-forward "^```$" (point-max) t)

            ;; Evaluate it, and then keep going.
            (eval-region l (match-beginning 0)))))
    (message "Skipping file that doesn't exist %s" path)))


(defun load-org-init (path)
  "If the specified file exists then attempt to load it as an org-file,
parsing and executing inline code-blocks"
  (if (file-exists-p (expand-file-name path))
      (org-babel-load-file (expand-file-name path))
    (message "Skipping file that doesn't exist %s" path)))


;; Load the global-file markdown file.
;;
;; We never supported any per-host, or local markdown files, so this is
;; simple and unconditional.
(load-markdown-init "~/.emacs.d/init.md")


;; Now we're going to start loading the new/updated org-mode files.
;;
;; We'll load:
;;
;;   ~/.emacs.d/init.org              - Should always exist
;;   ~/.emacs.d/init.$(hostname).org  - Optional.
;;   ~/.emacs.d/init.local.org        - Optional - never under revision control
;;
(load-org-init "~/.emacs.d/README.org")
(load-org-init (format "~/.emacs.d/init.%s.org" (hostname)))
(load-org-init "~/.emacs.d/init.local.org")
