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

This is a handy function I use if I need to edit this file:

```lisp
(defun skx-load-init()
  "Load my init.md file."
  (interactive)
    (find-file (expand-file-name "~/.emacs.d/init.md")))
```

Having easy access to a new lisp buffer is also useful, for experimentation:

```lisp
(defun skx-scratch-buffer()
  "Create a unique scratch-buffer, and swithc to it"
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*scratch*"))
  (insert "; Scratch buffer, kill it when you're done\n\n")
  (lisp-mode))
```

## Initial Functions

Common Lisp is required by some other later things, more detail here would
be nice but to get started we'll just require that library:

```lisp
    (require 'cl)
```

We also want to operate as a server, so we'll make sure that we start that
before we go any further:

```lisp
(server-start)
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

Finally for interactive evaluation this is a handy function to evaluate
either the current expression, or the current selection:

```lisp
(defun eval-region-or-last-sexp ()
  (interactive)
  (if (region-active-p) (call-interactively 'eval-region)
    (call-interactively 'eval-last-sexp)))
```

We'll bind this to a key, later.


## Basic History

I like to keep history of various tools beneath a transient directory, which I can remove whenever I like, rather than scattered around the filesystem.

First of all we define a helper to make a directory, if it is missing:

```lisp
(defun mkdir-if-missing (path)
  (if (not (file-exists-p (expand-file-name path)))
    (make-directory (expand-file-name path t))))
```

Now we can configure the history:

```lisp
;; Ensure we have ~/.trash.d which is the directory I use for transient things
;; As well as a subdirectory for emacs.
(mapcar (lambda (directory)
          (mkdir-if-missing directory))
          '("~/.trash.d/" "~/.trash.d/emacs.history/"))

;; Save our history into this new directory
(setq savehist-file
  (format "%s/emacs.history.%s"
    (expand-file-name "~/.trash.d/emacs.history")
    (getenv "USER")))

;; Enable history saving.
(savehist-mode 1)
```

## Backup Files

I'm annoyed by backups and similar.  So I disable them all:

```lisp

    ;; Disable backups
    (setq backup-inhibited t)
    (setq make-backup-files nil)

    ;; Disable auto-save
    (setq auto-save-default nil)
    (setq auto-save-interval (* 60 60 24))

    ;; Remove lockfiles
    (setq create-lockfiles nil)

```


## Bell: Disabled

Emacs has an annoying habit of making a beep if you attempt to scroll beyond the bottom of a buffer, cancel an operation, or carry out other everyday tasks.

We disable that behaviour here, in preference to a visual-flash:

```lisp
(setq visible-bell 1)
```


## Breaks

Regular breaks are good, so we configure an alarm to go off every thirty
minutes to suggest one to the user:

```lisp
(with-feature (break-time)
  (break-time-start))
```


## Buffers

If multiple buffers use the same filename we'll prefix with the parent directory:

```lisp
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
```


### Calendar setup

`M-x calendar` will show a calendar with Finnish names:

```lisp
(setq calendar-week-start-day 1
      calendar-day-name-array
      ["sunnuntai" "maanantai" "tiistai" "keskiviikko"
      "torstai" "perjantai" "lauantai"]
      calendar-month-name-array
      ["tammikuu" "helmikuu" "maaliskuu" "huhtikuu" "toukokuu"
      "kesäkuu" "heinäkuu" "elokuu" "syyskuu"
      "lokakuu" "marraskuu" "joulukuu"])

(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-set-date-style 'european)))
```

### Calc Mode

`literate-calc-mode` is a cute mode which allows constantly-updating calculations.  You can edit sums inside markdown, org-mode, and similar files, and see the results as overlays.

```lisp
(require 'literate-calc-mode)
```


## Completion

There are several packages out there providing "completion".  The most common are `ivy`, `helm`, and `ido`.  I'm using the latter:

```lisp
(ido-mode 1)
(setq ido-enable-flex-matching t)
(ido-everywhere)
```

## Custom Variables

By default, Emacs stores any configuration you make through its UI by writing custom-set-variables invocations to your init file, or to the file specified by custom-file. Though this is convenient, it’s also an excellent way to cause aggravation when the variable you keep trying to modify is being set in some custom-set-variables invocation. We can’t disable this behavior, and the custom-file variable can’t be nil, but we can make it look in a different place every time.

```lisp
(setq custom-file (make-temp-file ""))
```


## Docker

There is a handy [dockerfile-mode](https://github.com/spotify/dockerfile-mode) which allows highlighting docker-files.

Here we load it, and we can use `C-x C-b` to build the Dockerfile in the current buffer.

```lisp
(require 'dockerfile-mode)
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

    ;; CFEngine
    (with-feature (cfengine)
      (add-to-list 'auto-mode-alist '("\\.cf\\'" . cfengine-auto-mode)))

    ;; Groovy
    (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
    (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))

    ;; Lua
    (with-feature (lua-mode)
        (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
        (add-to-list 'interpreter-mode-alist '(("lua"   . lua-mode))))

    ;; Markdown
    (with-feature (markdown-mode)
        (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
        (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

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

Beyond the basic support for golang installed via that mode I've also configured LSP for this language, which provides smart completion & etc.

To complete this setup I have (manually) executed the following:

#+NAME: install-go-lsp-stuff
#+BEGIN_SRC sh
$ sudo apt-get install elpa-lsp-mode elpa-company-lsp elpa-lsp-ui
$ go install golang.org/x/tools/gopls@latest
#+END_SRC

For python:

#+NAME: install-python-lsp
#+BEGIN_SRC sh
$ sudo apt-get install python3-pyls
#+END_SRC

Once the dependencies are present the following configures LSP, including a helper to format code on save & etc:

```lisp
;; Define a save-hook to format buffers on-save
(defun skx/lsp-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; Define local keymappings for lsp-using modes
(defun skx/lsp-go-setup-bindings ()
  ; go to definition
  (local-set-key (kbd "M-SPC") 'lsp-find-definition)
  ; go back
  (local-set-key (kbd "M-b")    'pop-tag-mark))

;; Define a function to setup the LSP configuration I want.
(defun skx/setup-lsp ()
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq lsp-auto-guess-root t)
    (add-hook 'go-mode-hook #'skx/lsp-install-save-hooks)
    (add-hook 'go-mode-hook #'skx/lsp-setup-bindings)
    (add-hook 'python-mode-hook #'skx/lsp-install-save-hooks)
    (add-hook 'python-mode-hook #'skx/lsp-setup-bindings))

;; If we have `gopls` on our $PATH AND we have `lsp-mode` available ..
;; Then setup LSP, and add the hooks for go-mode to use it.
(if (executable-find "gopls")
    (with-feature (lsp-mode)
        (skx/setup-lsp)
        (add-hook 'go-mode-hook #'lsp-deferred)
        (add-hook 'python-mode-hook #'lsp-deferred)

        (add-hook 'python-mode-hook #'yas-minor-mode)
        (add-hook 'go-mode-hook #'yas-minor-mode)))
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
  (setq temp-point (point))
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
   t)
  (goto-char temp-point))
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

(add-hook 'z80-mode-hook (lambda ()
       (set (make-local-variable 'comment-start) "//")
       (set (make-local-variable 'comment-end) "")
       (set (make-local-variable 'hs-block-start-regexp) "^; ?{")
       (set (make-local-variable 'hs-block-end-regexp) "^; ?}")
       (hs-minor-mode t)
       (set 'hs-block-start-regexp "^; ?{{")
       (set 'hs-block-end-regexp "^; ?}}")
       (local-set-key (kbd "M-C-i") 'hs-toggle-hiding)
       (local-set-key (kbd "M--") 'hs-hide-all)
       (local-set-key (kbd "M-+") 'hs-show-all)))

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

### Language Modes - YAML Mode

YAML is used in Gitlab CI, and similar places.

```lisp
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
```

### Language Mode Helpers - Code Folding

I define a hook which will setup the toggling of code-blocks via HideShow,
this will be enabled for C, C++, Golang & Perl-modes.

This also binds `Esc-TAB` to toggle the block under the point, and `Esc--`
and `Esc-+` to hide/show all:

```lisp
    (defun enable-hs-mode-hook()
      (hs-minor-mode 1)
      (local-set-key (kbd "M-C-i") 'hs-toggle-hiding)
      (local-set-key (kbd "M--") 'hs-hide-all)
      (local-set-key (kbd "M-+") 'hs-show-all))

    ;; Enable code-folding for the common languages I use.
    (add-hook 'prog-mode-hook 'enable-hs-mode-hook)
```


### Language Mode Helpers - Templates

It is often useful to auto-insert content when visiting a particular kind of file, for example when creating a perl-file it would be good to ensure good practice by inserting:

```
     use strict;
     use warnings;
```

The `skx-template` package allows that to be done in a simple fashion; when visiting a file for the first time it will insert a snippet if it exists.

```lisp
(require 'skx-template)
```

### Language Mode Helpers - TODO Highlighting

The following snippet of code ensures that `TODO` comments/lines are shown
easily:

```lisp
(with-feature (hl-todo)
  (setq hl-todo-keyword-faces
      '(("FIXME"   . "#ffff00")
        ("TODO"   . "#ffff00")
        ("XXX"   . "#ffff00")))
    (global-hl-todo-mode t))
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

I put together the [org-nested](https://github.com/skx/org-nested) package to allow refining links easily.  Allowing links to be augmented by refinements.  This is now loaded:

```lisp
(require 'org-nested)
```

As noted above it is possible to evaluated blocks of script from within `org-mode`, but shell-scripting is disabled by default so we need to enable this explicitly:

```lisp
;; This works with the older-version of org-mode, as installed upon frodo.home
(with-feature (ob-sh)
              (org-babel-do-load-languages 'org-babel-load-languages '((sh . t))))

;; This is the preferred approach, which works on modern release of emacs and org-mode.
(with-feature (ob-shell)
              (org-babel-do-load-languages 'org-babel-load-languages '((shell . t))))

;; Ensure that we can export org-blocks
;; This is done for the CSS & Javascript export blocks
(require 'ob-org)

```

We'll enable line-wrapping and spell-checking when we enter org-mode:

```lisp
(add-hook 'org-mode-hook
    (lambda()
        (flyspell-mode)
        (toggle-truncate-lines)))
```

We'll also improve the default list-management functionality:

```lisp
(with-feature (org-autolist)
   (add-hook 'org-mode-hook (lambda () (org-autolist-mode))))
```

When following links `C-RETURN` moves back:

```lisp
(add-hook 'org-mode-hook (lambda ()
  (local-set-key (kbd "<C-return>") 'org-mark-ring-goto)))
```

On the topic of links, sometimes org-mode things that things are links
which are not, and kills exporting.  We want to ignore that behaviour:

```lisp
;; I just want an export.
;; If I wanted a link check, I'd ask for one.
(setq org-export-with-broken-links t)
```

`org-mode` is __all__ about lists!  So one thing that is nice is to visually update the display of the list-prefixes, via unicode characters.  We'll use `org-bullets` for that:

```lisp
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda ()
    (org-bullets-mode 1)))
```


Now we're done with the general setup so we'll handle the more specific things here:

```lisp

;; Store our org-files beneath ~/Private/Org.
(custom-set-variables  '(org-directory "~/Private/Org"))

;; Don't track org-id-locations globally, as this creates
;; a ~/.emacs.d/.org-id-locations file which is annoying.
(setq org-id-track-globally nil)

;; Populate the agenda from ~/Private/Org + ~/Private/Worklog/
(setq org-agenda-files (apply 'append
	(mapcar
		(lambda (directory)
			(if (file-directory-p directory)
			   (directory-files-recursively directory org-agenda-file-regexp)))
			       '("~/Private/Org" "~/Private/Worklog"))))

;; Add a custom org-agenda command
;;
;; Show TODOs, except those that are 100% complete, or which have
;; `:noexport:` in their text.  Note that this isn't a tag-match,
;; just a literal match as used in my worklog(s).
;;
(defun skx/org-agenda-skip-complete ()
  (org-agenda-skip-entry-if 'regexp ":noexport:\\|100%"))

(with-feature (org-agenda)
  (setq org-agenda-custom-commands
     '(("wi" "List of items closed in the past week."
        tags "+CLOSED>\"<-7d>\"/DONE")
       ("wq" "Quick (log) view"
        agenda ""
          ((org-agenda-start-with-log-mode t)))
       ("wt" "Show today's stories"
        search (format-time-string "%Y-%m-%d"))
       ("wo" "Outstanding items."
        todo ""
        ((org-agenda-skip-function 'skx/org-agenda-skip-complete))))))

;;
;; Our agenda-view will span two weeks by default.
(setq org-agenda-span 14)

;; But the agenda will start on the current day.
(setq org-agenda-start-on-weekday nil)

;; We don't show tasks that are complete
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

;; RETURN will follow links in org-mode files
(setq org-return-follows-link  t)

;; When exporting code then we get highlighting
(setq org-latex-listings t)

;; Don't hide leading stars
(setq org-hide-leading-stars nil)

;; Log when we're completing things.
(setq org-log-done t)

;; Setup TODO-workflow, and colouring.
(setq org-todo-keywords '((sequence "TODO(!)" "INPROGRESS" "|" "DONE(!)" "CANCELED" "SPILLOVER")))
(setq org-todo-keyword-faces '(
    ("TODO" . (:foreground "blue" :weight bold))
    ("INPROGRESS" . (:foreground "purple" :weight bold))
    ("SPILLOVER" . (:foreground "red" :weight bold))
    ("CANCELED"   . (:foreground "pink" :weight bold))))


;; When showing TODO we show the headline from which the item came.
(setq org-agenda-prefix-format "%-12:c %b")

;; Indentation in org-buffers matches the header-level
(setq org-startup-indented t)

;; This sets the indention-depth for child-entries
(setq org-indent-indentation-per-level 2)

;; Ctrl-a & Ctrl-e (for start/end of line) behave "magically" inside headlines
;; this is what I think most people would expect
(setq org-special-ctrl-a/e 't)

;; This hides the "*bold*", "/italic/" and "=preformatted=" markers:
(setq org-hide-emphasis-markers t)

;; Instead of showing ".." after folded-areas show the symbol.
(setq org-ellipsis " ▼")
```

Since we're hiding the emphasis markers it can be hard to edit text which is formatted.  To handle that we use [org-appear](https://github.com/awth13/org-appear):

```lisp
; load the library
(require 'org-appear)

; Ensure that we toggle link-markup as well as bold/italic/etc.
(setq org-appear-autolinks t)

; enable it in the modes we care about.
(add-hook 'org-diary-mode-hook 'org-appear-mode)
(add-hook 'org-mode-hook 'org-appear-mode)
```

## Org-Mode Diary

I keep a work-log where I write down tasks and notes about my working-life
every day.

This is handled by my [org-diary](https://github.com/skx/org-diary) package, and here we load it and allow quick access to my journal:

```lisp
;; Load the library-file.
(require 'org-diary)

;; Add a new tag to all entries, after creating them.
(add-hook 'org-diary-after-new-entry-hook
          (lambda()
            (org-set-tags (format-time-string "%Y_week_%V"))))

;; Create a helper to load the diary.
(defun skx-load-diary()
  "Load my diary/work-log, and scroll to today's entry."
  (interactive)
    (find-file (expand-file-name "~/Private/Worklog/Diary.org"))
    (org-diary-today))
```

## Org-Mode Code Execution

Another useful change to org-mode is allowing the ability to execute the Emacs lisp contained within a particular block when a file is loaded.

The following configuration enables the contents of a block named `skx-startblock` to be executed automatically when the file is loaded, and the block `skx-saveblock` to be evaluated once _before_ a file is saved:

```lisp
(defvar skx-org-eval-safe-list
    (list
        (expand-file-name "~/Private/"))
"A list of directories beneath which org-files can be evaluated with no prompting.")

(defun regexp-match-list(regexp list)
  "Return nil unless the regexp matches at least one of the list items."
  (delq nil (mapcar (lambda(x) (string-match x regexp )) list)))

(defun skx-org-eval-startblock ()
  "Evaluate the content of a code-block named 'skx-startblock' in the current
  org-document, if present.

  Emacs would usually prompt for permission as a safety precaution,
  but if the buffer is associated with a filename matching any
  of the patterns inside the list skx-org-eval-safe-list we
  just allow it.
  "
  (skx-org-eval-named-block "skx-startblock"))

(defun skx-org-eval-saveblock ()
  "Evaluate the content of a code-block named 'skx-saveblock' in the current
  org-document, if present.

  Emacs would usually prompt for permission as a safety precaution,
  but if the buffer is associated with a filename matching any
  of the patterns inside the list skx-org-eval-safe-list we
  just allow it.
  "
  (skx-org-eval-named-block "skx-saveblock"))


(defun skx-org-eval-named-block(name)
  "Execute the named block, if it exists, from within the current file."
  (save-excursion
    (org-save-outline-visibility t
      (if (member name (org-babel-src-block-names))
          (if (regexp-match-list (buffer-file-name) skx-org-eval-safe-list)
              (progn
                (setq-local org-confirm-babel-evaluate nil)
                (org-babel-goto-named-src-block name)
                (org-babel-execute-src-block)))))))


;; Load the start-block on startup
(add-hook 'org-mode-hook 'skx-org-eval-startblock)

;; evaluation the save-block on save
(defun skx-org-mode-before-save-hook-eval ()
  (when (eq major-mode 'org-mode)
    (skx-org-eval-saveblock)))

(add-hook 'before-save-hook #'skx-org-mode-before-save-hook-eval)
```

To use these facilities define blocks like so in your org-mode files:

```
#+NAME: skx-startblock
#+BEGIN_SRC emacs-lisp :results output silent
  (message "I like cakes - on document loads - do you?")
#+END_SRC
```

```
#+NAME: skx-saveblock
#+BEGIN_SRC emacs-lisp :results output silent
  (message "I like cakes - just before a save - do you?")
#+END_SRC
```

By default `org-mode` will prompt you to confirm that you want execution to happen, but we use `safe-skx-org-eval-startblock` to enable whitelisting particular file-patterns - if there is a match there will be no need to answer `y` to the prompt.


### Org-Mode tag cloud

I put together a simple tag-cloud helper package, which we'll now load:

```lisp
(require 'org-tag-cloud)
```

To make it useful we'll ensure that we disable warnings about eval which would otherwise be shown when following such a link.

```lisp
; Allow eval-links
(defun skx-org-mode-hook-eval-ok ()
    (make-variable-buffer-local 'org-confirm-elisp-link-function)
    (setq org-confirm-elisp-link-function nil))
```


### Org-Mode table navigation

There are no built-in functions for jumping around tables, so these two functions add the ability to go to the next/previous ones:

```lisp
(defun org-next-table (&optional arg)
  "Jump to the next table.

With a prefix argument ARG, jump forward ARG many tables."
  (interactive "p")
  (dotimes (n arg)
    (let ((pt (point)))
      (when (org-at-table-p)
        (goto-char (org-table-end)))
      (if (re-search-forward org-table-line-regexp nil t)
          (when (org-invisible-p)
            (org-reveal t)
            (org-show-entry)
            (unless (org-at-table-p)
              (org-next-table 1)))
        (goto-char pt)))))

(defun org-previous-table (&optional arg)
  "Jump to the previous table.

With a prefix argument ARG, jump backward ARG many tables."
  (interactive "p")
  (dotimes (n arg)
    (let ((pt (point)))
      (when (org-at-table-p)
        (goto-char (org-table-begin)))
      (if (re-search-backward org-table-line-regexp nil t)
          (progn
            (when (org-invisible-p)
              (org-reveal t)
              (org-show-entry)
              (unless (org-at-table-p)
                (org-previous-table 1)))
            (goto-char (1+ (org-table-begin))))
        (goto-char pt)))))

```


### Org-Mode and Table Links

If you press `RET` on a link inside a table it doesn't work as expected.

```lisp
(defun org-clicky()
   "Allow following links, even inside tables"
  (interactive)
  (if (eq 'org-link (get-text-property (point) 'face))
     (org-open-at-point)
  (org-return)))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'org-clicky)))
```


### Org-Mode and Blank Lines

Blank lines keep getting inserted in between headlines and I don't want to see them in collapsed (contents) views. When I use TAB to fold (cycle) tasks I don't want to see any blank lines between headings.

The following setting hides blank lines between headings which keeps folded view nice and compact.

```lisp
(setq org-cycle-separator-lines 0)
```

I find extra blank lines in lists and headings a bit of a nuisance. To get a body after a list you need to include a blank line between the list entry and the body – and indent the body appropriately. Most of my lists have no body detail so I like the look of collapsed lists with no blank lines better.

The following setting prevents creating blank lines before headings but allows list items to adapt to existing blank lines around the items:

```lisp
(setq org-blank-before-new-entry (quote ((heading)
    (plain-list-item . auto))))
```

The following setting prevents accidentally editing hidden text when the point is inside a folded region. This can happen if you are in the body of a heading and globally fold the org-file with S-TAB

```lisp
(setq org-catch-invisible-edits 'error)
```


### Org-Mode Utility Functions

The following function allows extracting the value of a global header from the current document:

```lisp
  (defun skx/org-global-prop( name )
    "Get the value from the global property with the given name, e.g. 'AUTHOR', 'TITLE', etc."
    (save-excursion
      (outline-show-all)
      (goto-line 0)
      (if (re-search-forward (concat "^#\\+" name ":") nil t)
          (progn
            (setq start (point))
            (re-search-forward "$")
            (setq end (point))
            (string-trim (buffer-substring-no-properties start end))))
      ))
```


### Org-Mode Viewing Exported Documents

Typically when I export documents I work with them elsewhere, but the export options (accessible via `C-c C-e`) have options for exporting and viewing:

* Export to HTML and Open
* Export to PDF and Open

I want those exports to open in `firefox` and `evince` respectively, the following snippet makes that happen:

```lisp
(defun org-file-apps-html (file link)
  (interactive)
  (shell-command (format "%s %s"
                         (if (file-exists-p "/usr/bin/firefx")
                             "/usr/bin/firefox"
                           "/opt/firefox/firefox")
                         file)))

(setq org-file-apps
    (quote
        ((auto-mode . emacs)
         ("\\.x?html?\\'" . (lambda (file link) (org-file-apps-html file link)))
         ("\\.pdf\\'" . "/usr/bin/evince %s") )))
```


### Org-Mode LaTex & PDF Export

When exporting `org-mode` files to PDF it is nicer if new sections start on a new page.  To do that I insert a faux `\\clearpage` section before all top-level headlines:

```lisp
  (defun skx/pdf-add-clearpage (backend)
    (when (org-export-derived-backend-p backend 'latex)
        (save-excursion
            (outline-show-all)
            (goto-line 0)

            ;; search for "* XXXXXX"
            (while (re-search-forward "^\\(\\* .*$\\)" nil t)
                (replace-match "\n\\\\clearpage\n\\1"))
            )))

  (add-hook 'org-export-before-parsing-hook 'skx/pdf-add-clearpage)

```

One other problem is that code blocks don't export neatly.  To resolve that you need this:

```lisp
(require 'org)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-src-fontify-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (latex . t)))
```

In addition to the block you'll need `apt-get install python-pygments`.


### Org-Mode Timestamping

The following allows any `#+LAST_MODIFIED` headers to be updated on file-save:

```lisp
(defun skx/update-org-modified-property ()
  "If a file contains a '#+LAST_MODIFIED' property update it to contain
  the current date/time"
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+LAST_MODIFIED:" (point-max) t)
      (progn
        (kill-line)
        (insert (format-time-string " %d/%m/%Y %H:%M:%S") )))))

```

It is made available like so:

```lisp
(defun skx-org-mode-before-save-hook ()
  (when (eq major-mode 'org-mode)
    (skx/update-org-modified-property)))

(add-hook 'before-save-hook #'skx-org-mode-before-save-hook)
```

### Org-Mode Secrets

Sometimes org-mode files contain secrets, things that you don't want to make visible to other people.  One common solution is to encrypt the contents of particular regions with GPG.

You can run `M-x org-decrypt-entries` to make them visible, but re-encrypt any time you save:

```lisp
(require 'org-crypt)
(add-hook 'org-mode-hook
          (lambda()
            (add-hook 'before-save-hook 'org-encrypt-entries nil t)
            (setq org-tags-exclude-from-inheritance (quote ("crypt")))
            (setq org-crypt-key "root@localhost")
            (setq auto-save-default nil)))
```

The downside to encrypting contents is that you'll have a random GPG-message in your exported document.  There are two solutions here:

* Remove the section on export.
* Wrap it, so that it looks pretty.

I prefer to make it obvious there is an encrypted section, because you could add `:crypt:noexport:` to allow both encryption and no exporting if you wish.

Here we wrap all GPG_messages with "`#+BEGIN_EXAMPLE`" to format them neatly on export:

```lisp
(defun skx/html-quote-pgp (backend)
  "Wrap GPG messages when exporting to HTML"
  (when (or (org-export-derived-backend-p backend 'html)
            (org-export-derived-backend-p backend 'latex))
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "^\\(\s*-+BEGIN PGP MESSAGE-+\\)" nil t)
        (replace-match "\n#+BEGIN_EXAMPLE\n\\1"))
      (goto-char 0)
      (while (re-search-forward "^\\(\s*-+END PGP MESSAGE-+\\)" nil t)
        (replace-match "\n\\1\n#+END_EXAMPLE\n"))
      )))

(add-hook 'org-export-before-parsing-hook 'skx/html-quote-pgp)
```

### Org-Mode UTF

```lisp
(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
```


## Search

I set "search-default-mode" to allow me to match `äiti` when searching for `aiti`, for example.

```lisp
(setq search-default-mode 'char-fold-to-regexp)
```


## Spell-Checking

I use `flyspell` as a spell-checker when editing text and org-mode files.  Sometimes it decides that words are errors, even when I know best.

The following Lisp allows the word at the point to be added to my personal dictionary:

```lisp

(setq flyspell-use-meta-tab nil)

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
    (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
```

Finally we allow Emacs to control our music playback, which is supplied
by [MPD](http://www.musicpd.org/).  There are several different MPD
client-modes in Emacs, this is my own:

```lisp
    (noerr-require 'mpc)
```


## User Interface Setup

I prefer to keep a reasonably minimal look, so I disable the toolbar and scroll-bars.

The menu-bar is somewhat useful as I'm slowly learning more about `org-mode`, so I'll leave that enabled unless I'm running in a terminal.

```lisp
    (require 'scroll-bar)

    ;; Disable the scroll-bars, and the tool-bar.
    (dolist (mode
        '(scroll-bar-mode tool-bar-mode))
      (funcall mode 0))

    ;; Show the menubar only when running with graphics
    (menu-bar-mode (display-graphic-p))

    ;; Make sure our cursor doesn't get in the way.
    (require 'avoid)
    (mouse-avoidance-mode 'cat-and-mouse)
```

Once the basics have been setup the next step is to configure some colours:

```lisp
(load-theme 'wombat)
(set-face-background 'default "#111")
(set-face-background 'cursor "#c96")
(set-face-background 'isearch "#c60")
(set-face-foreground 'isearch "#eee")
(set-face-background 'lazy-highlight "#960")
(set-face-foreground 'lazy-highlight "#ccc")
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
(setq-default cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
              cursor-type '(hbar . 4)            ; Underline-shaped cursor
              cursor-intangible-mode t           ; Enforce cursor intangibility
              x-stretch-cursor t)                ; Stretch cursor to the glyph width

(blink-cursor-mode 1)                            ; We want to blink

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

Lisp famously uses a lot of parenthesis, but so does Python, Perl,
and many other languages.  The following section highlights expressions
inside parenthesis in a cute way:

```lisp
    (setq show-paren-style 'expression)
    (setq show-paren-when-point-in-periphery t)
    (setq show-paren-when-point-inside-paren t)
    (setq show-paren-ring-bell-on-mismatch t)
    (setq show-paren-delay 0)
    (show-paren-mode t)
```

The following section takes care of setting up other basic and
global things the way that I prefer them.

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

    ;; Show non-existent lines with a special glyph in the left fringe:
    (setq-default indicate-empty-lines t)

    ;; We want to remove trailing whitespace when a file is saved.
    (require 'whitespace)
    (add-hook 'write-file-hooks 'delete-trailing-whitespace)

    ;; But many Unix system-files require a trailing newline to work
    ;; correctly, for example `crontab` files.  So make sure that's OK
    (setq require-final-newline t)

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



## XXX - Keybindings

**NOTE** This file is largely grouped into settings, ordered alphabetically.  The keybindings go "last" though, because that makes sense.

I try to avoid too many keybindings that are non-standard, but there are
some I've grown accustomed to:

```lisp
;; Create the keymap for all my bindings.
(defvar steve-mode-map (make-keymap)
  "Keymap for steve-mode")

;; Next, create the minor mode, switch it on by default, make it global,
;; and assign the keymap to it.
(define-minor-mode steve-mode
  "Minor mode for my personal keybindings."
  :init-value t
  :global t
  :keymap steve-mode-map)

;; Next, add the keymap to `emulation-mode-map-alists'
(add-to-list 'emulation-mode-map-alists
             `((steve-mode . ,steve-mode-map)))

;; Finally, bind the keys
(define-key steve-mode-map (kbd "M-g")   'goto-line)
(define-key steve-mode-map (kbd "C-c i") 'skx-load-init)
(define-key steve-mode-map (kbd "C-c s") 'skx-scratch-buffer)
(define-key steve-mode-map (kbd "C-c w") 'skx-load-diary)
(define-key steve-mode-map (kbd "M-=")   'align-equals)
(define-key steve-mode-map (kbd "C-x F") 'sudo-find-file)
(define-key steve-mode-map (kbd "C-+") 'text-scale-increase)
(define-key steve-mode-map (kbd "C--") 'text-scale-decrease)
(define-key steve-mode-map (kbd "C-c a") 'org-agenda)
(define-key steve-mode-map (kbd "C-s") 'isearch-forward)
(define-key steve-mode-map (kbd "C-r") 'isearch-backward)

;; org-mode & markdown-mode sidebar
(define-key steve-mode-map (kbd "M-'") 'imenu-list-smart-toggle)

; Allow the backtick key to work as I expected.
(define-key steve-mode-map [dead-grave] "`")

; kill buffer
(define-key steve-mode-map (kbd "C-x k") '(lambda () (interactive)
  (kill-buffer (current-buffer))))

; exit emacs
(define-key steve-mode-map (kbd "C-x C-c") '(lambda ()
            (interactive)
            (if (y-or-n-p-with-timeout "Do you really want to exit Emacs ? " 10 nil)
                (progn
                  (if window-system
                      (progn
                        (if (fboundp 'uptime) (uptime))
                        (sleep-for 1)))
                  (save-buffers-kill-emacs)))
            (message "emacs quit aborted")))

; unset things
(define-key steve-mode-map (kbd "C-z")     '(lambda () (interactive)))
(define-key steve-mode-map (kbd "C-c C-z") '(lambda () (interactive)))
```


## XXXX - TODO

A small section of things that might be nice to explore in the future.

* https://github.com/integral-dw/org-superstar-mode
* A massive `.emacs` file with good commentary:
  * https://svn.red-bean.com/repos/kfogel/trunk/.emacs


```lisp
(message "init.md loaded")
```
