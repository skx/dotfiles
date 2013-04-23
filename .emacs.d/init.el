;; When emacs launches it looks for one of:
;;
;;   * ~/.emacs
;;   * ~/.emacs.d/init.el
;;
;; We prefer the second form, because we store our dotfiles under
;; the GIT revision-control system.
;;




(require 'cl)

(defun noerr-require (feature)
  "`require' FEATURE, but don't invoke any Lisp errors.
If FEATURE cannot be loaded, this function will print an error
message through `message' and return nil. It otherwise behaves
exactly as `require'."
  (ignore-errors
    (require feature (symbol-name feature) t)))


(defmacro with-feature (feature &rest body)
  "Require FEATURE and execute BODY.
If FEATURE can't be loaded, don't execute BODY."
  (when (noerr-require (car feature))
    (push 'progn body)))


(defun skx-add-to-load-path( list )
  "Add each entry on the passed list to the load-path, if it exists as a directory."
  (while list
    (if (file-exists-p (expand-file-name (car list)))
	(add-to-list 'load-path (expand-file-name (car list))))
    (setq list (cdr list))))

;;
;; Setup the load-path
;;
(skx-add-to-load-path (list "~/.emacs.d" "~/.emacs.d/lang" "~/.emacs.d/unix" "~/.emacs.d/ui"))

;;
;; Load cfengine mode if available
;;
(with-feature (cfengine)
              (setq auto-mode-alist
                    (append '(("\\.cf$" . cfengine2-mode)) auto-mode-alist))
              (setq auto-mode-alist
                    (append '(("cf\\." . cfengine2-mode)) auto-mode-alist))
              (setq auto-mode-alist
                    (append '(("cfagent\\.conf" . cfengine2-mode)) auto-mode-alist)))

;;
;; Lua-mode
;;
(with-feature (lua-mode)
              (setq auto-mode-alist
                    (append '(("\\.lua$" . lua-mode)) auto-mode-alist))
              (setq interpreter-mode-alist
                    (append '(("lua" . lua-mode)) interpreter-mode-alist)))

;;
;; Perl is good, but we need to make tweaks.
;;
(noerr-require 'skx-perl)

;;
;; Ruby mode
;;
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode))
	      '(("\\.ruby$" . ruby-mode))
	      auto-mode-alist))
(setq interpreter-mode-alist
      (append interpreter-mode-alist
	      '(("rb"   . ruby-mode))
	      '(("ruby" . ruby-mode))))



;;
;;  Some unix-specific things:
;;
;;  * Check perl syntax when saving files.
;;  * Make files executable on write, if they have valid shebang lines.
;;  * Report uptime on exit.
;;  * MPC media client
;;
(noerr-require 'perl-syntax-check)
(noerr-require 'shebang)
(noerr-require 'uptime)
(noerr-require 'mpc)

;;
;;  pwsafe interface.
;;
;;  1. cache the password for the database.
;;  2. Pressing ret/enter will copy the username + password.
;;
(with-feature (pwsafe)
              (setq pwsafe-keep-passwd t)
              (define-key pwsafe-list-mode-map [(return)] 'pwsafe-copy-user-name-and-password)
              (define-key pwsafe-list-mode-map [(right)] 'pwsafe-info-current-item))



;;
;; User-interface tweaks
;;
(noerr-require 'skx-font-sizes)
(noerr-require 'skx-minimal-look)
(if (noerr-require 'color-theme)
    (color-theme-charcoal-black))
(if window-system
    (mouse-avoidance-mode 'cat-and-mouse))



;;
;; Delete trailing whitespace on save
;;
(require 'whitespace)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)


;;
;; Encodings
;;
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;
;; Show the time on the status bar.
;;
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; Show matching brackets easily.
(show-paren-mode t)

;; Avoid the annoying startup message.
(setq inhibit-startup-message t)

;;  Uncompress files.
(auto-compression-mode t)

;; Paste at point, not mouse position
(setq mouse-yank-at-point t)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; allow recursive editing in minibuffer
(setq enable-recursive-minibuffers t)

;; Highlight the region between point and mark at all times.
(transient-mark-mode t)

;; make searches case insensitive
(setq case-fold-search t)

;; Moving cursor down at bottom scrolls only a single line, not half page
(setq scroll-step 1)
(setq scroll-conservatively 5)

;; TAB characters are evil
(setq-default indent-tabs-mode nil)


;;  Show the file we've got loaded in the frame title.
(setq frame-title-format  (concat invocation-name "@" system-name ": %b %+%+ %f"))


;;
;; Auto-save
;;
;; Don't make backup files.
(setq make-backup-files nil)
(setq delete-auto-save-files t)

(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.trash.d/emacs.autosaves/\\1" t))))

;; create the autosave dir if necessary, since emacs won't.
(if (file-exists-p (expand-file-name "~/.trash.d/emacs.autosaves/"))
    ()
  (make-directory (expand-file-name "~/.trash.d/emacs.autosaves/" t)))


;;
;; Keybindings
;;
(require 'skx-keybindings)