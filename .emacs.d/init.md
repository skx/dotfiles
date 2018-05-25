# Emacs Initialization Functions

This file contains the human-readable initialization configuration for emacs.
When emacs launches it will attempt to load an init-file, and it will examine
each of the following files:

* `~/.emacs`
* `~/.emacs.el`
* `~/.emacs.d/init.el`

I've created the file [~/.emacs.d/init.el](init.el) which parses and executes
the contents of _this_ (markdown) file, allowing me to write my Emacs
configuration in a somewhat literate form.

Hopefully this is neat, and allows my configuration to be self-documenting,
and easily understood.


## Initial Functions

Common Lisp is required by some other later things, more detail here would
be nice but to get started we'll just require that library:

```lisp
    (require 'cl)
```

The first thing we need to do is make sure that the various subdirectories
beneath the `~/.emacs/` directory are added to the load-path.  This will
ensure that future use of `require` will find the files we're attempting
to load:

```lisp
    (defun add-to-load-path (d)
       "If the supplied parameter is a directory then add it to the load-path"
        (if (file-directory-p d)
            (add-to-list 'load-path d)))

    (mapc 'add-to-load-path
        (directory-files "~/.emacs.d/" t "[a-z]*"))
```

Now we define some utility-functions to load packages.

The following function will load a package and avoid raising an error
if it isn't found:

```lisp
    (defun noerr-require (feature)
        "`require' FEATURE, but don't invoke any Lisp errors.
        If FEATURE cannot be loaded, this function will print an error
        message through `message' and return nil. It otherwise behaves
        exactly as `require'."
        (ignore-errors
           (require feature (symbol-name feature) t)))
```

With the previous method in-place we can now ensure that if some package
is loaded we can conditionally execute some code:

```lisp
    (defmacro with-feature (feature &rest body)
        "Require FEATURE and execute BODY.
        If FEATURE can't be loaded, don't execute BODY."
        (when (noerr-require (car feature))
            (push 'progn body)))
```

The initial setup is now complete, such that we can start loading
packages, making configuration-changes & etc.



## Language Modes

Most of the time I spend in Emacs is for developing, and writing code.

Code I write tends to be in some combination of Lua, Ruby, Perl, or C++.
In addition to these _real_ programming languages I also use
[CFEngine](http://cfengine.com/) and [Puppet](https://puppetlabs.com/) for
automation - so I load modes for those too.

```lisp
    (with-feature (lua-mode)
        (setq auto-mode-alist
            (append '(("\\.lua$" . lua-mode)) auto-mode-alist))
        (setq interpreter-mode-alist
            (append '(("lua" . lua-mode)) interpreter-mode-alist)))

    (with-feature (markdown-mode)
        (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
        (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

    (with-feature (puppet-mode)
        (setq auto-mode-alist
            (append '(("\\.pp$" . puppet-mode)) auto-mode-alist)))

    (setq auto-mode-alist
        (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
    (setq interpreter-mode-alist
        (append interpreter-mode-alist '(("rb"   . ruby-mode))))

```

"`*.ino`" files are used by the Arduino IDE to compile code, and
this is really just C++ with magic-wrapping:

```lisp
    ;; Arduino input-files.
    (add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))
```

### Language Modes - C / C++


Now we can configure basic formatting, as well as setting up support
for code-folding (which allows you to toggle the display of function
bodies, etc).

```lisp
    (setq c-default-style "linux")
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)

    (defun my-c-mode-hook ()
      (hs-minor-mode 1))

    (add-hook 'c++-mode-hook 'my-c-mode-hook t)
    (add-hook 'c-mode-hook 'my-c-mode-hook t)

```




### Language Modes - Perl

I do a lot of my coding in Perl-mode, and this is configured here.

First of all we want to ensure that we use `cperl-mode`, rather than
`perl-mode`, and we wish to ensure that `*.t` are formatted in this mode
too - as these are usually test-cases:

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

I also install a post-save hook which shows if the perl we're writing
is well-formed.

**NOTE**: This can be abused as `perl -c ...` will evaluate code found
in `BEGIN{ .. }` blocks.

```lisp
    (noerr-require 'perl-syntax-check)
```

The last Perl-specific thing I have is `M-x perltidy` which will
invoke the `perltidy` command on the contents of the current buffer.
This works with the
[.perltidyrc](https://github.com/skx/dotfiles/blob/master/.perltidyrc)
configuration file I have stored in my dotfiles:


```lisp
(defun perltidy()
  "Tidy the contents of the current buffer via `perltidy'"
  (interactive)
  (shell-command-on-region
   ;; beginning and end of buffer
   (point-min)
   (point-max)
   ;; command and parameters
   "perltidy"
   ;; output buffer
   (current-buffer)
   ;; replace?
   t
   ;; name of the error buffer
   "*Error Buffer*"
   ;; show error buffer?
   t))
```


### Language Modes - golang

[golang](https://golang.org/) is a language I've recently started to use, and
there isn't a native Emacs Lisp mode for it yet.

Installation instead relies the code in [the official github repository](https://github.com/dominikh/go-mode.el):

```lisp
    (require 'go-mode)
    (add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode))
```

The following hook ensures that code is formatted prior to being saved.

If [goimports](https://godoc.org/golang.org/x/tools/cmd/goimports) is present it will be used, otherwise we'll fall back to the default formatter which uses `gofmt`.

Finally we can use [godef](https://github.com/rogpeppe/godef) to jump to method definitions, much like ctags:


```lisp
    (defun my-go-mode-hook ()
      ;; prefer goimports, if present
      (if (executable-find "goimports")
        (setq gofmt-command "goimports"))

      (add-hook 'before-save-hook 'gofmt-before-save)

      ;; esc-space to jump to definition
      (local-set-key (kbd "M-SPC") 'godef-jump)
      ;; escp-b to jump (b)ack
      (local-set-key (kbd "M-b") 'pop-tag-mark)
    )
    (add-hook 'go-mode-hook 'my-go-mode-hook)
```


### Language Modes - Utilities

A lot of programming environments allow you to setup variables via something
like this:

```
     int i = 1;
     int foo = 2;
```

Things look neater if they're aligned, thusly:

```
     int i   = 1;
     int foo = 2;
```

The following section of code lets us select a region and run `M-=` to
align the section based upon the `=` sign:

```lisp
    (defun align-equals (begin end)
      (interactive "r")
      (align-regexp begin end "\\(\\s-*\\)=" 1 1))


    (global-set-key (kbd "M-=") 'align-equals)
```


## Web Utilities

I don't often writen plain HTML these days, instead I use markdown
for most of my websites via the
[templer](http://github.com/skx/templer/) static-site generator.

There are times when I need to escape content though, and the
following  allows that to be done neatly - select the region and run
`M-x escape-html-region`:

```lisp
(defun escape-html-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&lt;")
      (goto-char (point-min))
      (replace-string ">" "&gt;")
      (goto-char (point-min))
      (replace-string "\"" "&quot;")
      )))
```


### Language Modes - Web Mode

One of the annoyances with writing HTML is that often it contains
extra things inline, such as Javascript and CSS.  To solve this
problem - of wanting to mix HTML-mode along with Javascript-mode for example,
I use [web-mode](http://web-mode.org/):

```lisp
    (require 'web-mode)
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
```

The default choices seem to have tags be almost invisible, so we'll
explicitly set them to pink, and configure the indentation too:

```lisp
    (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "Pink1")
    ;; Highlighting the current element helps the fight against divitus.
    (setq web-mode-enable-current-element-highlight t)

    ;; All modes should use the same indentation.
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)

```

## System Administration

The following snippet is useful for system-administration, allowing
you to open a file for reading via `sudo`:

```lisp
    (require 'tramp)
    (defun sudo-find-file (file-name)
        "Like find file, but opens the file as root."
        (interactive "FSudo Find File: ")
        (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
        (find-file tramp-file-name)))

    (global-set-key (kbd "C-x F") 'sudo-find-file)
```

Once you've opened the file it will be read-only, you can toggle that
with `Ctrl-x Ctrl-v`.

Because I tend to be working all day it is interesting to see how
long Emacs has been running.  The following function will show that,
and can be invoked via `M-x uptime`.

```lisp
    (require 'time-date)

    (defvar emacs-up-time (current-time)
    "Time at which Emacs started up.")

    (defun uptime ()
      "Displays the uptime of GNU Emacs."
      (interactive)
      (let* ((now (current-time))
         (second (floor (- (time-to-seconds now)
                           (time-to-seconds emacs-up-time))))
         (minute (floor second 60))
         (hour (floor minute 60))
         (day (floor hour 24)))
         (message "Emacs up %s day(s), %02d:%02d"
         day (% hour 24) (% minute 60))))
```


## Whitespace Handling

We like to remove trailing whitespace, and define a function to
collapse muliple newlines into one, across a region.

```lisp
    ;; We want to see trailing whitespace
    (setq-default show-trailing-whitespace t)

    ;; We want to remove trailing whitespace when a file is saved.
    (require 'whitespace)
    (add-hook 'write-file-hooks 'delete-trailing-whitespace)

    (defun collapse-blank-lines(start end)
     (interactive "r")
     (replace-regexp "^\n\\{2,\\}" "\n" nil start end))
```

When running Emacs upon a terminal, rather than graphically, lines that
are too long have a "`\`" character added to them.  This makes copying
and pasting to other terminal-applications annoying.  Disable that wrapping
behaviour here:

```lisp
    (set-display-table-slot standard-display-table 'wrap ?\ )
```


## UTF-8

UTF-8 is the future, we should greet it with open-arms.

```lisp
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

    ;; Ctrl +, or Ctrl - will change the text size.
    (global-set-key (kbd "C-+") 'text-scale-increase)
    (global-set-key (kbd "C--") 'text-scale-decrease)

    ;; Make sure our cursor doesn't get in the way.
    (if window-system
        (mouse-avoidance-mode 'cat-and-mouse))
```

Now we've tweaked the GUI we can setup the clipboard integration:

```lisp
    ;; Copying in emacs should allow pasting into gnome-terminal, etc.
    (setq x-select-enable-clipboard t)

    ;;
    (setq x-select-enable-primary t)
    (setq mouse-drag-copy-region t)
```

Once we've removed things that we don't like the next section is
responsible for configuring the colours - first of all the global
theme which is used for colours, and then secondly the colour of
the cursor:

```lisp
    ;; If we can load the colour-theme library, choose a dark theme.
    (with-feature (color-theme)
        (color-theme-initialize)
        (color-theme-charcoal-black))

    ;; Change cursor color according to mode.
    ;;  read-only -> red
    ;;  insert    -> blue
    ;;  default   -> white
    (defvar hcz-set-cursor-color-color "")
    (defvar hcz-set-cursor-color-buffer "")

    (defun hcz-set-cursor-color-according-to-mode ()
        "change cursor color according to some minor modes."
        ;; set-cursor-color is somewhat costly, so we only call it when needed:
        (let ((color
            (if buffer-read-only "red"
            (if overwrite-mode "blue"
             "white"))))
         (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
         (set-cursor-color (setq hcz-set-cursor-color-color color))
         (setq hcz-set-cursor-color-buffer (buffer-name)))))

    ;; After a command update things, if required.
    (add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)
```

The following section takes care of setting up other user-interface things
the way that I prefer them.

```lisp
    ;; Show the time on the status bar.
    (setq display-time-24hr-format t)
    (setq display-time-day-and-date t)
    (display-time)

    ; Ignore case when completing file names
    (setq read-file-name-completion-ignore-case 't)

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

    (defun px-raise-frame-and-give-focus ()
        (when window-system
            (raise-frame)
            (x-focus-frame (selected-frame))
            (set-mouse-pixel-position (selected-frame) 40 40)
            (message "raised-window")))
    (add-hook 'server-switch-hook 'px-raise-frame-and-give-focus)

    ; Text-mode is default mode
    (setq default-major-mode 'text-mode)

    ; auto-formatting in text-mode
    ;(add-hook 'text-mode-hook 'turn-on-auto-fill)
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
                        (if (fboundp 'uptime) (uptime))
                        (sleep-for 1)))
                  (save-buffers-kill-emacs)))
            (message "emacs quit aborted")))
```

One thing that often catches me out is the backgrounding on `Ctrl-z`, so
this is explicitly disabled here:


```lisp
    ;; get rid of pesky "Ctrl-z" and "Ctrl-x Ctrl-z" annoying minimize
    (global-set-key "\C-z" 'nil)
    (global-set-key "\C-x\C-z" 'nil)
```


## Unix-specific tweaks

The following section helper ensures that files are given `+x` permissions
when they're saved, if they contain a valid shebang line:

```lisp
    (noerr-require 'shebang)
```

Finally we allow Emacs to control our music playback, which is supplied
by [MPD](http://www.musicpd.org/).  There are several different MPD client-modes in Emacs, this
is my own:

```lisp
    (noerr-require 'mpc)
```


## Misc. Functions

The following section contains a small collection of utility functions.

The first allows the deletion of lines which match a particular pattern,
which is useful in more situations than you might expect.  An obvious
example would be to delete lines containing comments - which might match
the regular expression `^#`.



```lisp
    (defun kill-matching-lines (regexp &optional rstart rend interactive)
      "Kill lines containing matches for REGEXP.

    See `flush-lines' or `keep-lines' for behavior of this command.

    If the buffer is read-only, Emacs will beep and refrain from deleting
    the line, but put the line in the kill ring anyway.  This means that
    you can use this command to copy text from a read-only buffer.
    \(If the variable `kill-read-only-ok' is non-nil, then this won't
    even beep.)"
      (interactive
       (keep-lines-read-args "Kill lines containing match for regexp"))
      (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
        (with-current-buffer (clone-buffer nil nil)
          (let ((inhibit-read-only t))
            (keep-lines regexp rstart rend interactive)
            (kill-region (or rstart (line-beginning-position))
                         (or rend (point-max))))
          (kill-buffer)))
      (unless (and buffer-read-only kill-read-only-ok)
        ;; Delete lines or make the "Buffer is read-only" error.
        (flush-lines regexp rstart rend interactive)))
```
