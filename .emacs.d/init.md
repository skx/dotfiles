# Emacs Initialization Functions

This file contains the human-readable initialization configuration for emacs.


## Utility Functions

First of all we define some utility-functions to load packages.  The following
function will load a package and avoid raising an error if it isn't found:

```lisp
    (require 'cl)

    (defun add-to-load-path( list )
       "Add each entry to the load-path, if it exists as a directory."
        (while list
           (if (file-exists-p (expand-file-name (car list)))
               (add-to-list 'load-path (expand-file-name (car list))))
          (setq list (cdr list))))

    (add-to-load-path (list "~/.emacs.d/util" "~/.emacs.d/lang"
         "~/.emacs.d/unix" "~/.emacs.d/ui"))


    (defun noerr-require (feature)
        "`require' FEATURE, but don't invoke any Lisp errors.
        If FEATURE cannot be loaded, this function will print an error
        message through `message' and return nil. It otherwise behaves
        exactly as `require'."
        (ignore-errors
           (require feature (symbol-name feature) t)))
     ;; code-end
```


Now we can use that to execute a block of code if loading a module
is successful:

```lisp
    (defmacro with-feature (feature &rest body)
        "Require FEATURE and execute BODY.
        If FEATURE can't be loaded, don't execute BODY."
        (when (noerr-require (car feature))
            (push 'progn body)))
```



## Language Modes

Now that we've added our load-path setup, and configured some simple
things we'll load our language-modes

```lisp
	(with-feature (lua-mode)
        (setq auto-mode-alist
            (append '(("\\.lua$" . lua-mode)) auto-mode-alist))
		(setq interpreter-mode-alist
            (append '(("lua" . lua-mode)) interpreter-mode-alist)))

	(with-feature (markdown-mode)
		(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
		(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

    (noerr-require 'align-equals)

	(with-feature (puppet-mode)
        (setq auto-mode-alist
            (append '(("\\.pp$" . puppet-mode)) auto-mode-alist)))

	(setq auto-mode-alist
		(append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
	(setq interpreter-mode-alist
		(append interpreter-mode-alist '(("rb"   . ruby-mode))))

    ;; Coding formatting
    (setq c-default-style "linux")
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)
```

## Language Modes - Perl

I do a lot of my coding in Perl-mode, and this is configured here.

First of all we want to ensure that we use `cperl-mode`, rather `perl-mode`,
and we wish to ensure that `*.t` are formatted in this mode too - as these
are usually test-cases:

```lisp
    ;;  We always prefer CPerl mode to Perl mode.
    (fset 'perl-mode 'cperl-mode)

    ;; Load .t files as perl too - as these are usually test-cases
    (setq auto-mode-alist (append '(("\\.t$" . cperl-mode)) auto-mode-alist))
```

Now we want to make sure that the code is formatted according to my tastes:

```lisp
    ;;  BSD Style brace placement, with tab=4 spaces.
    (defun my-cperl-mode-hook ()
       (setq cperl-indent-level 4)
       (setq cperl-brace-offset -2)
       (setq cperl-label-offset 0))

    ;;  When starting load my hooks
    (add-hook 'cperl-mode-hook 'my-cperl-mode-hook t)
```



## Editing Nicities

We like to remove trailing whitespace, and treat our buffers as UTF-8

```lisp
	;; We want to see trailing whitespace
	(setq-default show-trailing-whitespace t)

    ;; We want to remove trailing whitespace when a file is saved.
	(require 'whitespace)
	(add-hook 'write-file-hooks 'delete-trailing-whitespace)

	(set-terminal-coding-system 'utf-8)
	(set-keyboard-coding-system 'utf-8)
	(prefer-coding-system 'utf-8)
```


## Configuring Backups

I'm annoyed by backups and similar.  So I disable them all:

```lisp
	;; create a directory to hold history
	(if (not (file-exists-p (expand-file-name "~/.trash.d/")))
		(make-directory (expand-file-name "~/.trash.d/" t)))

	(if (not (file-exists-p (expand-file-name "~/.trash.d/emacs.history/")))
		(make-directory (expand-file-name "~/.trash.d/emacs.history/" t)))

	;; Save our history there
	(setq savehist-file (concat (expand-file-name "~/.trash.d/emacs.history/") "emacs." (getenv "USER")))
	(savehist-mode 1)

	;disable backups
	(setq backup-inhibited t)
	(setq make-backup-files nil)

	;disable auto-save
	(setq auto-save-default nil)
	(setq auto-save-interval (* 60 60 24))
```



## User Interface Tweaks

I prefer to avoid menu-bars, tool-bars, and have a minimal look:

```lisp
    ;; Disable the scroll-bar(s).
    (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

    ;; Disable the tool-bar.
    (if (fboundp 'tool-bar-mode) (tool-bar-mode 0))

    ;; Disable the menu.
    (if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
```

The following section does that, as well as configures a reasonably neat colour-theme by default.

```lisp
    ;; Allow font resizing.
	(noerr-require 'skx-font-sizes)

	;; Load a colour-theme.
	(if (noerr-require 'color-theme) (color-theme-charcoal-black))

	;; Make sure our cursor doesn't get in the way.
	(if window-system
		(mouse-avoidance-mode 'cat-and-mouse))

	;; Show the time on the status bar.
	(setq display-time-24hr-format t)
	(setq display-time-day-and-date t)
	(display-time)

    ;; Show columns too
    (column-number-mode)

	;; Show matching brackets easily.
	(show-paren-mode t)

	;; Avoid the annoying startup message.
	(setq inhibit-startup-message t)

	;; Uncompress files.
	(auto-compression-mode t)

	;; Paste at point, not mouse position
	(setq mouse-yank-at-point t)

	;; Make all "yes or no" prompts show "y or n" instead
	(fset 'yes-or-no-p 'y-or-n-p)

	;; Highlight the region between point and mark at all times.
	(transient-mark-mode t)

	;; make searches case insensitive
	(setq case-fold-search t)

	;; Moving cursor down at bottom scrolls only a single line, not half page
	(setq scroll-step 1)
	(setq scroll-conservatively 5)

	;; TAB characters are evil
	(setq-default indent-tabs-mode nil)

	;; Show the file we've got loaded in the frame title.
	(setq frame-title-format  (concat invocation-name "@" system-name ": %b %+%+ %f"))

    ;; Copying in emacs should allow pasting into gnome-terminal, etc.
    (setq x-select-enable-clipboard t)

    (defun px-raise-frame-and-give-focus ()
        (when window-system
            (raise-frame)
            (x-focus-frame (selected-frame))
            (set-mouse-pixel-position (selected-frame) 40 40)
            (message "raised-window")))
    (add-hook 'server-switch-hook 'px-raise-frame-and-give-focus)

    ;; This sets the cursor colour neatly:
    ;;    read-only -> red
    ;;    insert    -> blue
    ;;    default   -> white
    (require 'cursor-colours)
```



## Keybindings

I try to avoid too many keybindings that are non-standard, but there are
some I've grown accustomed to:

```lisp
    (global-set-key "\M-g" 'goto-line)
    (global-set-key "\C-s" 'isearch-forward-regexp)
    (global-set-key "\C-r" 'isearch-backward-regexp)

    ;; kill the current-buffer with no prompting.
    (global-set-key "\C-xk"
        '(lambda ()
            (interactive)
                (if (and (fboundp 'gnuserv-buffer-p) (gnuserv-buffer-p (current-buffer)))
                    (gnuserv-buffer-done (current-buffer))
                (kill-buffer (current-buffer)))))


    ;; Prevent accidentally killing emacs.
    (global-set-key [(control x) (control c)]
        '(lambda ()
            (interactive)
                (if (y-or-n-p-with-timeout "Do you really want to exit Emacs ? " 10 nil)
                    (progn
                        (if window-system
                            (progn
                                (uptime)
                                    (sleep-for 1)))
                                        (save-buffers-kill-emacs)))
                                            (message "emacs quit aborted")))
```


## Unix-specific tweaks

The following section is really just a small collection of tools
which make life a little more nice for Unix users:

```lisp
	(noerr-require 'perl-syntax-check)
	(noerr-require 'shebang)
	(noerr-require 'uptime)
	(noerr-require 'mpc)
```
