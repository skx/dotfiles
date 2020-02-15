# GNU/Emacs Configuration

This file contains the human-readable initialization configuration for emacs.
When emacs launches it will attempt to load an init-file, and it will examine
each of the following files:

* `~/.emacs`
* `~/.emacs.el`
* `~/.emacs.d/init.el`

I've created the file [~/.emacs.d/init.el](init.el) which parses and executes
the contents of _this_ (markdown) file, allowing me to write my Emacs
configuration in a somewhat literate form.  It will also load some org-mode
files if they are present, which are written in a similar literate fashion.

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
       "If the supplied item is a directory then add it to the load-path"
        (if (file-directory-p d)
            (add-to-list 'load-path d)))

    (mapc 'add-to-load-path
        (file-expand-wildcards "~/.emacs.d/*"))
```

Now we define some utility-functions to load packages.  The following function will load a package and avoid raising an error if it isn't found:

```lisp
    (defun noerr-require (feature)
        "`require' FEATURE, but don't invoke any Lisp errors.
        If FEATURE cannot be loaded, this function will print an error
        message through `message' and return nil. It otherwise behaves
        exactly as `require'."
        (ignore-errors
           (require feature (symbol-name feature) t)))
```

With the previous method in-place we can now ensure that if a package is
successfully loaded we can then conditionally execute some code:

```lisp
    (defmacro with-feature (feature &rest body)
        "Require FEATURE and execute BODY.
        If FEATURE can't be loaded, don't execute BODY."
        (when (noerr-require (car feature))
            (push 'progn body)))
```

The initial setup is now complete, so we can start loading packages, making
configuration-changes & etc.


## Backup Files

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

    ;; Disable backups
    (setq backup-inhibited t)
    (setq make-backup-files nil)

    ;; Disable auto-save
    (setq auto-save-default nil)
    (setq auto-save-interval (* 60 60 24))
```


## File Handling

Emacs has a built-in file/directory browser which is available via `M-x dired`, and which I use very often.

One irritation is that by default "dotfiles" are shown, I usually prefer these to be hidden by default.  The following section does two things:

* Hides dotfiles by default.
* Allows them to be toggled via `M-TAB`.

```lisp
    (require 'dired-x)
    (setq dired-omit-files "^\\...+$")
    (add-hook 'dired-mode-hook (lambda ()
      (local-set-key (kbd "M-TAB") 'dired-omit-mode)
      (dired-omit-mode 1)))

```

The `dired-git-info` package updates `dired` to allow you to view git commit information.  I bind that to `)`, which matches the toggling of detailed-information bound to `(` by default:

```lisp
     (with-feature (dired-git-info)
       (add-hook 'dired-mode-hook (lambda ()
         (local-set-key (kbd ")") 'dired-git-info-mode))))
```


## Language Modes

Most of the time I spend in Emacs is for developing, and writing code.

Code I write tends to be in some combination of Lua, Ruby, Perl, Go, or C++.

In addition to _real_ programming languages I also use [CFEngine](http://cfengine.com/) and [Puppet](https://puppetlabs.com/) for automation - so I load modes for those too.

**Note** The updates to `auto-mode-alist` end with `\\'`, which matches the end of the string (i.e. filename).  You might expect to see that written as "`.txt$`", but that would not be exactly the same, for filenames that contain a newline.  (`$` would match the newline, but `'` would match the end of the filename itself).


```lisp
    ;; Lua
    (with-feature (lua-mode)
        (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
        (add-to-list 'interpreter-mode-alist '(("lua"   . lua-mode))))

    ;; Markdown
    (with-feature (markdown-mode)
        (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
        (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

    ;; CFEngine
    (with-feature (cfengine)
      (add-to-list 'auto-mode-alist '("\\.cf\\'" . cfengine-auto-mode)))

    ;; Puppet
    (with-feature (puppet-mode)
        (add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode)))

    ;; Ruby setup doesn't require loading a mode
    (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
    (add-to-list 'interpreter-mode-alist '(("rb"   . ruby-mode)))
```

The monkey programming language was introduced (and implemented!)
in the book "[Writing An Interpreter In Go](https://interpreterbook.com/)".

[My monkey implementation](https://github.com/skx/monkey/) contains several
enhancements, and comes complete with an emacs mode which we'll load here:

```lisp
    (with-feature (monkey)
        (setq auto-mode-alist
            (append '(("\\.mon$" . monkey-mode)) auto-mode-alist)))

```

"`*.ino`" files are used by the Arduino IDE to compile code, and
these files are C++ with magic-wrapping to make compilation happen:

```lisp
    ;; Arduino input-files.
    (add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))
```


### Language Modes - C / C++

Now we can configure basic formatting for C/C++:

```lisp
    (setq c-default-style "linux")
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)

```

Note that I also setup [code-folding](#language-mode-helpers---code-folding) later in this file.


### Language Modes - Emacs Lisp

In emacs-lisp-mode we can enable eldoc-mode to display information about a function or a variable in the echo area.

```lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
```

### Language Modes - golang

[golang](https://golang.org/) is a language I use a fair bit, but there isn't a mode for it included in the releases of Emacs as packaged for Debian GNU/Linux, so installation instead relies upon the code in [the official github repository](https://github.com/dominikh/go-mode.el).

(Obviously my [dotfiles](https://github.com/skx/dotfiles/) contain a copy of the appropriate files.)

Once installed we can now configure the basic setup, ensuring that the mode is loaded for the editing of `*.go` files:

```lisp
    (require 'go-mode)
    (add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode))
```

More interestingly we can add a hook to ensure that code is formatted prior to being saved.  If [goimports](https://godoc.org/golang.org/x/tools/cmd/goimports) is present it will be used, otherwise we'll fall back to the default `gofmt`-based formatting.

In this hook we'll also allow [godef](https://github.com/rogpeppe/godef) to be used to jump to method definitions, via `M-Space` much like you'd expect with CTAGS (see the later note on using [using tags](#tags-support) directly):


```lisp
    (defun my-go-mode-hook ()
      ;; prefer goimports, if present
      (if (executable-find "goimports")
        (setq gofmt-command "goimports"))

      ;; Format code when we save
      (add-hook 'before-save-hook 'gofmt-before-save)

      ;; esc-space to jump to definition
      (local-set-key (kbd "M-SPC") 'godef-jump)
      ;; esc-b to jump (b)ack
      (local-set-key (kbd "M-b") 'pop-tag-mark)
    )
    (add-hook 'go-mode-hook 'my-go-mode-hook)
```

Note that I also setup [code-folding](#language-mode-helpers---code-folding) later in this file.



### Language Modes - Perl

I do a lot of my coding in Perl, and this is configured here.

First of all we want to ensure that we use `cperl-mode`, rather than
`perl-mode`, and we wish to ensure that `*.t` are formatted in this mode
too - as these files almost always contain perl test-cases in my
experience:

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

Using the function we've just defined we can now make sure that we tidy
our perl-buffers just prior to saving, if we have a `perltidy` executable:

```lisp
    (add-hook 'cperl-mode-hook
        (lambda ()
            (if (executable-find "perltidy")
                (add-hook 'before-save-hook 'perltidy nil t))))
```

Note that I also setup [code-folding](#language-mode-helpers---code-folding) later in this file.



### Language Modes - Z80 Assembly

I'm having fun doing "retro" things with a [Z80 processor](https://en.wikipedia.org/wiki/Zilog_Z80), so this mode loads the appropriate mode for that.

```lisp
    (require `z80-mode)
    (add-to-list 'auto-mode-alist (cons "\\.z80\\'" 'z80-mode))
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
    (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
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

### Language Mode Helpers - Code Folding

I define a hook which will setup the toggling of code-blocks via HideShow,
this will be enabled for C, C++, Golang & Perl-modes.

This also binds `Esc-TAB` to toggle the block under the point, and `Esc--`
and `Esc-+` to hide/show all:

```lisp
    (defun enable-hs-mode-hook()
      (hs-minor-mode 1)
      (local-set-key (kbd "M-TAB") 'hs-toggle-hiding)
      (local-set-key (kbd "M--") 'hs-hide-all)
      (local-set-key (kbd "M-+") 'hs-show-all))

    ;; Enable code-folding for the common languages I use.
    (add-hook 'c++-mode-hook 'enable-hs-mode-hook t)
    (add-hook 'c-mode-hook 'enable-hs-mode-hook t)
    (add-hook 'go-mode-hook 'enable-hs-mode-hook t)
    (add-hook 'perl-mode-hook 'enable-hs-mode-hook t)
    (add-hook 'web-mode-hook 'enable-hs-mode-hook t)
    (add-hook 'python-hook 'enable-hs-mode-hook t)
```



### Language Mode Helpers - Utilities

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


## Git

`git` setup is pretty much outside the scope of this document, but the least we can do is to configure a suitable mode for the `~/.gitconfig` file:

```lisp
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))
```



## Long Lines

Using [column-enforce-mode](https://github.com/jordonbiondo/column-enforce-mode) we can view lines that are "too long", in a clear fashion:

```lisp
   (unless (string= system-name "localhost.localdomain")
    (with-feature (column-enforce-mode)
        (global-column-enforce-mode t)))
```

The above section is enabled for all hosts, except the one system I have which has a hostname of `localhost.localdomain` - this is a system which is not configured for _real_ use..


## Markdown Index

The following snippet is useful when you're working with large markdown-files,
although it is superceded somewhat by the inline menu which `imenu-list` provides, as noted later:

```lisp
    (defun markdown-index()
     "Show (clickable) headings in the current buffer"
     (interactive)
     (occur "^#+"))
```

Now we can ensure that this is bound to `M-i` when `markdown-mode` is active:

```lisp
    (add-hook 'markdown-mode-hook
     (lambda ()
      (local-set-key (kbd "M-'") 'imenu-list-smart-toggle)
      (local-set-key (kbd "M-i") 'markdown-index)))
```

## Org-Mode

`org-mode` is a wonderful thing which allows Emacs to hold tables, TODO-lists, and much much more.  For the moment I'm keeping document-specific lisp and configuration within the appropriate document, but there are some things that make working with `org-mode` nicer which will live _here_.

One of the nice things about org-mode is that it lets you contain embedded snippets of various programming languages which can be evaluated, executed and otherwise processed.  The following snippets ensure that these blocks can be highlighted and indented as expected:

```lisp
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
```

The next thing that is globally useful is to allow searches for internal links to match sub-strings of headlines, rather than requiring complete matches:

```lisp
  (setq org-link-search-must-match-exact-headline nil)
```

As noted above it is possible to evaluated blocks of script from within `org-mode`, but shell-scripting is disabled by default so we need to enable this explicitly:

```lisp
;; This works with the older-version of org-mode, as installed upon frodo.home
(with-feature (ob-sh)
              (org-babel-do-load-languages 'org-babel-load-languages '((sh . t))))

;; This is the preferred approach, which works on modern release of emacs and org-mode.
(with-feature (ob-shell)
              (org-babel-do-load-languages 'org-babel-load-languages '((shell . t))))
```

Another useful change to org-mode is allowing the ability to execute the Emacs lisp contained within a particular block when a file is loaded.

The following configuration enables the contents of a block named `skx-startblock` to be executed automatically when the file is loaded:

```lisp
(defvar safe-skx-org-eval-startblock (list (concat (getenv "HOME") "/Repos/git.steve.fi/") (concat (getenv "HOME") "/Repos/git.steve.org.uk/") (concat (getenv "HOME") "/Org") )
 "A list of filename patterns which will have their contents evaluated with no prompting.")

(defun regexp-match-list(regexp list)
  "Return nil unless the regexp matches at least one of the list items"
  (delq nil (mapcar (lambda(x) (string-match x regexp )) list)))

(defun skx-org-eval-startblock ()
  "If there is a code-block named 'skx-startblock' in the current
  org-document then evaluate the content within it.

  Emacs will prompt for permission as a safety precaution,
  unless the buffer is associated with a filename matching any
  of the patterns inside the list safe-skx-org-eval-startblock.
  "
  (if (member "skx-startblock" (org-babel-src-block-names))
      (save-excursion
      (save-restriction
        (if (regexp-match-list (buffer-file-name) safe-skx-org-eval-startblock)
            (setq-local org-confirm-babel-evaluate nil))
        (org-babel-goto-named-src-block "skx-startblock")
        (org-babel-execute-src-block)))
    nil))
(add-hook 'org-mode-hook 'skx-org-eval-startblock)
```

To use this define a block like so in your org-mode files:

```
#+NAME: skx-startblock
#+BEGIN_SRC emacs-lisp :results output silent
  (message "I like cakes - do you?")
#+END_SRC
```

You'll note that by default org-mode will prompt you whether the execution should be permitted, we use the `safe-skx-org-eval-startblock` to enable whitelisting particular file-patterns - if there is a match there will be no need to answer `y` to the prompt.

We'll enable line-wrapping and spell-checking when we enter org-mode:

```lisp
(add-hook 'org-mode-hook
    (lambda()
        (flyspell-mode)
        (toggle-truncate-lines)))
```

Now we're done with the general setup so we'll handle the more specific things here:

```lisp

;; Store our org-files beneath ~/Org.  Scan all of them for agenda-items & etc.
(custom-set-variables
  '(org-directory "~/Org")
  '(org-agenda-files (list org-directory)))

;; Log when we're completing things.
(setq org-log-done t)

;; Setup TODO-workflow, and colouring.
(setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))

;; Indentation in org-buffers matches the header-level
(setq org-startup-indented 'f)

;; Ctrl-a & Ctrl-e (for start/end of line) behave "magically" inside headlines
;; this is what I think most people would expect
(setq org-special-ctrl-a/e 't)

;; Bring up the agenda.
(global-set-key "\C-ca" 'org-agenda)

```


## Spell-Checking

I use `flyspell` as a spell-checker when editing text and org-mode files.  Sometimes it decides that words are errors, even when I know best.

The following Lisp allows the word at the point to be added to my personal dictionary:

```lisp

(eval-when-compile (require 'cl))

(defun append-aspell-word (new-word)
  (let ((header "personal_ws-1.1")
        (file-name (substitute-in-file-name "$HOME/.aspell.en.pws"))
        (read-words (lambda (file-name)
                      (let ((all-lines (with-temp-buffer
                                         (insert-file-contents file-name)
                                         (split-string (buffer-string) "\n" t))))
                        (if (null all-lines)
                            ""
                          (split-string (mapconcat 'identity (cdr all-lines) "\n")
                                        nil
                                        t))))))
    (when (file-readable-p file-name)
      (let* ((cur-words (eval (list read-words file-name)))
             (all-words (delq header (cons new-word cur-words)))
             (words (delq nil (remove-duplicates all-words :test 'string=))))
        (with-temp-file file-name
          (insert (concat header
                          " en "
                          (number-to-string (length words))
                          "\n"
                          (mapconcat 'identity (sort words #'string<) "\n"))))))
    (unless (file-readable-p file-name)
      (with-temp-file file-name
        (insert (concat header " en 1\n" new-word "\n")))))
  (ispell-kill-ispell t) ; restart ispell
  (flyspell-mode)
  (flyspell-mode))

(defun append-aspell-current ()
  "Add current word to aspell dictionary"
  (interactive)
  (append-aspell-word (thing-at-point 'word)))
```

Move the point over the "error", then run `M-x append-ispell-current`.  The word will be added to `~/.aspell.en.p*`, or whatever the appropriate file is (based upon your language).

If a word appears multiple times in your document adding it to the ignored-list won't correct all the other errors:

```lisp
(defun flyspell-buffer-after-pdict-save (&rest _)
  (flyspell-buffer))

(advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save)
```

Flyspell offers on-the-fly spell checking. We can enable flyspell for all text-modes with this snippet.

```lisp
(add-hook 'text-mode-hook 'turn-on-flyspell)
```

To use flyspell for programming there is `flyspell-prog-mode`, that only enables spell checking for comments and strings. We can enable it for all programming modes using the prog-mode-hook.

```lisp
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
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

Since we're living in the future nowadays a lot of system-administration is moving towards a cloud-based setup.

One of the tools I use most frequently for that is [Hashicorp](https://www.hashicorp.com/)'s [terraform](https://www.terraform.io/), and here we'll configure our buffers to be auto-formatted when we save them:

```lisp
    (require 'terraform-mode)
    (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
```




## Tags Support

Many lanagues have support for tags/etags/ctags

For example on a Python project you might run something like this to create/update a `TAGS` file:

      $ find . -name "*.py" | xargs ctags -e

If you don't have `ctags` then `etags` with no arguments will also do the right thing:

      $ find . -name '*.el' | xargs etags

```lisp
;; Default (fedora)
(setq ctags-cmd "/usr/bin/ctags -e")

;; Debian
(if (file-exists-p "/usr/bin/etags")
    (setq ctags-cmd "/usr/bin/etags"))

(defun create-tags (dir-name)
  "Create tags file"
  (interactive "DDirectory: ")
  (shell-command (format "/bin/sh -c \"cd %s && %s\"" dir-name ctags-cmd)))
```

Once you've done that the following will allow the `TAGS`-file to be located, walking up to 10 directories above the location of your currently-open file:


```lisp
    (require 'etags-table)
    (setq etags-table-search-up-depth 10)
```

With all that stuff out of the way it should now be possible to use `M-.` to jump to the definition of the _thing_ under the point.



## Unix Setup

The following section helper ensures that files are given `+x` permissions
when they're saved, if they contain a valid shebang line:

```lisp
    (noerr-require 'shebang)
```

Finally we allow Emacs to control our music playback, which is supplied
by [MPD](http://www.musicpd.org/).  There are several different MPD
client-modes in Emacs, this is my own:

```lisp
    (noerr-require 'mpc)
```


## User Interface Setup

I prefer to avoid menu-bars, tool-bars, and have a minimal look:

```lisp
    ;; Disable the scroll-bars, tool-bar, and menu-bar
    (dolist (mode
        '(scroll-bar-mode
          tool-bar-mode
          menu-bar-mode))
     (funcall mode 0))

    ;; Ctrl +, or Ctrl - will change the text size.
    (global-set-key (kbd "C-+") 'text-scale-increase)
    (global-set-key (kbd "C--") 'text-scale-decrease)

    ;; Make sure our cursor doesn't get in the way.
    (require 'avoid)
    (mouse-avoidance-mode 'cat-and-mouse)
```

Now we've tweaked the GUI we can setup the clipboard integration:

```lisp
    ;; Copying in emacs should allow pasting into gnome-terminal, etc.
    (setq x-select-enable-clipboard t)
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
            (color-theme-ld-dark))

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

    ; Ignore case when completing file names, buffer names,
    ; and completions generally.
    (setq read-file-name-completion-ignore-case t)
    (setq read-buffer-completion-ignore-case t)
    (setq case-fold-search t)
    (setq completion-ignore-case  t)

    ;; Show column-numbers too
    (column-number-mode)

    ;; Show matching brackets easily.
    (show-paren-mode t)

    ;; Avoid the annoying startup message.
    (setq inhibit-startup-message t)

    ;; Uncompress files as they're loaded.
    (auto-compression-mode t)

    ;; Paste at point, not mouse position
    (setq mouse-yank-at-point t)

    ;; Make all "yes or no" prompts show "y or n" instead
    (fset 'yes-or-no-p 'y-or-n-p)

    ;; Highlight the region between point and mark at all times.
    (transient-mark-mode t)


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
```


## User Interface Sidebar

There is a neat package `imenu-list` which allows you to view a sidebar in the current frame, containing index-entries.

This is used in the markdown-mode setup earlier to show you a list of headings.  Load it here:

```lisp
    (with-feature (imenu-list)
      (setq imenu-list-focus-after-activation t
            imenu-list-auto-resize t
            imenu-list-position 'left))
```

## UTF-8

UTF-8 is the future, we should greet it with open-arms.

```lisp
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)
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



## WorkLog

I maintain a log of what I do every day, which is saved in the file `~/Work.MD`.  This is a markdown file, as the suffix would suggest, and each day I create a new section for that days work.

This utility function finds today's entry, or creates it by appending to the buffer:

```lisp
(defun worklog-today()
  "If you have a work-log then move to today's date.  If it isn't found
 then create it"
  (interactive "*")
  (beginning-of-buffer)
  (if (not (search-forward (format-time-string "# %d-%m-%Y") nil t 1))
      (progn
        (end-of-buffer)
        (insert (format-time-string "\n\n# %d-%m-%Y\n")))))
```

Now I need to execute that function every time I visit a file named `Work.MD`:

```lisp
(defun worklog_hook ()
  (when (equalp (file-name-nondirectory (buffer-file-name)) "work.md")
    (worklog-today)
    )
)

(add-hook 'find-file-hook 'worklog_hook)
```

Finally here is a quick function to load the file which has a quick binding:

```lisp
(defun worklog()
  (interactive "*")
  (find-file "~/Work.MD"))

(global-set-key (kbd "C-x w") 'worklog)

```


## XXX - Keybindings

**NOTE** This file is largely grouped into settings, ordered alphabetically.  The keybindings go "last" though, because that makes sense.

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

One thing that often catches me out is the backgrounding behaviour on `Ctrl-z`,
especially on a terminal, so this is explicitly disabled here:


```lisp
    ;; get rid of pesky "Ctrl-z" and "Ctrl-x Ctrl-z" annoying minimize
    (global-set-key "\C-z" 'nil)
    (global-set-key "\C-x\C-z" 'nil)
```

Finally since I'm in Finland I've found that I'm using foreign keyboard layouts a lot of the time.  One problem I have is the backtick character doesn't always work the way that I want - which is a particular problem in `markdown-mode`.

```lisp
   ;; Allow the backtick key to work as I expected.
   (global-set-key [dead-grave] "`")
```



## XXXX - TODO

A small section of things that might be nice to explore in the future.

This looks nice:

* https://github.com/syohex/emacs-git-gutter-fringe

Usage via:

>(require 'git-gutter-fringe)
>
>(dolist (p '((git-gutter:added    . "#0c0")
>             (git-gutter:deleted  . "#c00")
>             (git-gutter:modified . "#c0c")))
>  (set-face-foreground (car p) (cdr p))
>  (set-face-background (car p) (cdr p)))

* Keybindings via a minor-mode
  * https://github.com/larstvei/dot-emacs#key-bindings
  * https://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
  * See also
    * https://github.com/Atman50/emacs-config/blob/master/README.org#i-use-ctrl-z-for-personal-bindings
