# GNU/Emacs Configuration

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

Multiple packages are loaded from beneath the various subdirectorires of `~/.emacs`, the files are stored within _this_ repository to ensure they continue to be available if the upstream location vanishes, however there is a simple package here which will update them from their remote sources:

* [tools/resync-packages.el](tools/resync-packages.el)
  * Fetch the remote packages we use within this repository, updating them appropriately.

We use `use-package` to configure/load packages where possible, and we've configured `straight` to install a small number of packages from external repositories:

* [magit](https://magit.vc/)
  * The `git` client and interface.
* company/lsp-ui/lsp
  * Language Server Protocol magic, to get completion, help on hover, etc.
  * Configured for golang and python.



## System Information

The following section defines some constants relating to the local system, they might be useful in the future.

```lisp
(defconst EMACS29+   (> emacs-major-version 28))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
```



## Initial Functions

We want to operate as a server, so we'll make sure that we start that before we go any further - of course if it is already running then we skip it:

```lisp
(require 'server)
(unless (server-running-p) (server-start))
```

Operating as a server means that we can reuse the single Emacs instance, without having to worry about restarting new copies (and the potential speed-hit that would cost).

With the server-startup out of the way the first real we need to do is make sure that the various subdirectories beneath the `~/.emacs/` directory are added to the load-path.  This will ensure that future use of `require` will find the files we're attempting to load:

```lisp
(defun add-to-load-path (dir)
  "If the supplied item is a directory then add it to the load-path"
  (if (file-directory-p dir)
    (add-to-list 'load-path dir)))

(mapc 'add-to-load-path (file-expand-wildcards "~/.emacs.d/*"))
```

The initial setup is now complete, so we can start loading packages, making configuration-changes & etc.

[use-package](https://github.com/jwiegley/use-package) is a helpful library which allows you to keep all configuration related to a single package in a self-contained block, and do so much more.

We're going to also use [straight](https://github.com/radian-software/straight.el) as a package manager (i.e installer) so we need to bootstrap it:

```lisp
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
```

TLDR:

* `straight.el` is a package-manager, and will be used to install packages.
  * Most of the packages we use are already included within this repository.
* `use-package` will be used to configure packages, and so they need to be integrated
  * `:straight t` will cause a package to be installed, if missing.


```lisp
;; configure error-handling for use-package, before we load it.
(setq use-package-verbose t
      use-package-expand-minimally nil
      use-package-compute-statistics t
      debug-on-error t)

;;
;; load use-package - unless it is already available
;; (Because Emacs 29.x includes it natively.)
;;
(unless (and (fboundp 'package-installed-p) (package-installed-p 'use-package))
  (straight-use-package 'use-package))

;; And ensure that straight is loaded by the use-package library.
(use-package straight)
```

Now we actually load use-package, which will be installed by straight.el.  This might be confusing, but I found the following guide useful:

* https://francopasut.netlify.app/post/emacs_portable_use-package_straight/

To ensure we can update the packages bundled within this repository, not those installed via `straight.el`, we'll load our package-refresher.  This must be triggered manually.

```lisp
(use-package resync-packages
 :defer 2)
```



## Initial Macros

As noted if we want to cause a package to be installed via `straight.el` we need to add `:straight t` to the `(use-package ..)` invocation.

* Most of the packages we use are already included within this repository.
* However there are a couple that we load externally.

To be explicit we'll use a little macro here to configure things for us:

```lisp
(defmacro use-package-straight (name &rest args)
  "Like `use-package' but with `straight' enabled.
NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight t
     ,@args))
```

This allows finding the packages we load remotely via a grep for `use-package-straight`.



## Initial PATH (MacOS)

On Unix systems the emacs process inherits the shell environment which was used to start it up.

On MacOS there are some niggles, and a similar problem occurs upon GNU/Linux if you launch Emacs from the GNOME menu:

```lisp
(use-package exec-path-from-shell
   :config
   (exec-path-from-shell-initialize))
```



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
;; Ensure we have ~/.trash.d which is the directory I use for transient things.
(mkdir-if-missing "~/.trash.d/")

;; Save our history into this new directory
(setq savehist-file
  (format "%s/emacs.history.%s"
    (expand-file-name "~/.trash.d")
    (getenv "USER")))

;; Enable history saving.
(savehist-mode t)
```

A good use of history is the ability to go to the last change in a buffer:

```lisp
(use-package goto-last-change
  :defer 2
  :bind
    (
    ("M-m" . #'goto-last-change)
    ))
```



## Backup Files

I'm annoyed by backups and similar.  So I disable them all:

```lisp
;; Disable backups
(setq backup-inhibited t)
(setq make-backup-files nil)

;; Prevent the creation of the auto-save-list directory
(setq auto-save-list-file-prefix nil)

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



## Buffers

If multiple buffers use the same filename we'll prefix with the parent directory:

```lisp
(use-package uniquify
  :defer 2
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 2))
```

When opening the list of buffers I want to highlight the current line and
switch to the list immediately:

```lisp
(add-hook 'Buffer-menu-mode-hook #'(lambda ()
    ; Highlight the current line
    (hl-line-mode 1)
    ; Switch to the buffer immediately.
    (switch-to-buffer "*Buffer List*")))
```

There are times when you've been running Emacs for a few weeks and you have too many buffers, here's a quick function to kill them all:

```lisp
(defun kill-all-buffers ()
  "Kill all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (delete-other-windows))
```



## Case Sensitivity

Ignore case when completing file names, buffer names, and running searches.

```lisp
; completion
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case  t)

; searches
(setq case-fold-search t)
```



## Calendar setup

`M-x calendar` will show a calendar with Finnish names:

```lisp
(setq calendar-week-start-day 1
      calendar-day-name-array
      [
          "Sunnuntai"
          "Maanantai"
          "Tiistai"
          "Keskiviikko"
          "Torstai"
          "Perjantai"
          "Lauantai"
      ]
      calendar-month-name-array
      [
          "Tammikuu[JAN)"
          "Helmikuu(FEB)"
          "Maaliskuu(MAR)"
          "Huhtikuu(APR)"
          "Toukokuu(MAY)"
          "Kesäkuu(JUN)"
          "Heinäkuu(JUL)"
          "Elokuu(AUG)"
          "Syyskuu(SEP)"
          "Lokakuu(OCT)"
          "Marraskuu(NOV)"
          "Joulukuu(DEC)"
      ]

      ;; M-x world-clock
      world-clock-list
      '(("Europe/Helsinki" "Helsinki")
        ("CET" "Berlin")
        ("Europe/London" "London")
        ))


(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-set-date-style 'european)))
```



## Completion

There are several packages out there providing "completion".  The most common are `ivy`, `helm`, and `ido`.

I'm using `ido` - the only thing to note for myself is that when running `C-x C-f`, to load a file, you can use keybindings to edit things:

* `C-d` - Edit in dired-mode.
* `C-e` - Edit the whole prompt.
* See the help for `ido-find-file` for a complete list.

```lisp
(use-package ido
  :defer 2
  :init
  (setq
    ido-everywhere                         t
    ido-use-filename-at-point              'guess
    ido-use-url-at-point                   nil
    ido-virtual-buffers                    t
    ido-flex-matching                      t
    ido-use-faces                          t
    ido-default-buffer-method              'selected-window
    ido-auto-merge-work-directories-length -1
    ido-save-directory-list-file           (expand-file-name "~/.trash.d/emacs.ido.history")
    ido-file-extensions-order              '(".org" ".md" ".txt" ".html" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf" ".py")
    ido-enable-flex-matching               t

    ;;; Ignore files defined in variable completion-ignored-extensions
    ido-ignore-extensions t
    )
  :config
  (ido-mode))
```

When `ido-ignore-extensions` is set to a true value then files are ignored from the general purpose `completion-ignored-extensions` list.  This covers most things, but we can add to it:

```lisp
(dolist (suffix
   '(".last" ".odt" ".pdf" ".tex" ".txt"))
    (push suffix completion-ignored-extensions))
```

In addition to the completion provided by `ido` above we can also help ourselves by making "M-x ..:" easier to use, via smex:

```lisp
(use-package smex
    :init
      (setq smex-save-file (expand-file-name "~/.trash.d/emacs.smex.history"))
    :config
      (global-set-key (kbd "M-x") 'smex)
      (smex-initialize))
```



## Custom Variables

By default, Emacs stores any configuration you make through its UI by writing custom-set-variables invocations to your init file, or to the file specified by custom-file. Though this is convenient, it’s also an excellent way to cause aggravation when the variable you keep trying to modify is being set in some custom-set-variables invocation. We can’t disable this behavior, and the custom-file variable can’t be nil, but we can make it look in a different place every time.

```lisp
(setq custom-file (make-temp-file ""))
```



## Dialog Boxes

Dialog boxes are disabled, we'll answer all questions via the minibuffer:

```lisp
(setq use-dialog-box nil)
```



## Dired / File & Directory Browsing

Emacs has a built-in file/directory browser which is available via `M-x dired`, and which I use very often.

On MacOS I see issues with the display of directory listing, which are resolved like so:

```lisp
(if IS-MAC
  (setq dired-use-ls-dired nil))
```

We highlight the current line when we're using dired:

```lisp
(add-hook 'dired-after-readin-hook 'hl-line-mode)
```

Dired typically opens a new buffer when you navigate through directories, in the past I used to dislike this behaviour but I have become a convert.

However to cleanup is a simple matter:

```lisp
(defun kill-dired-buffers ()
  (interactive)
   (mapc (lambda (buffer)
             (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
                (kill-buffer buffer)))
           (buffer-list)))
```

My only other irritation with `dired` is that by default "dotfiles" are shown, I usually prefer these to be hidden by default.  The following section does a bunch of things:

* Allows killing all dired buffers, via `q`.
* Hides dotfiles by default.
* Allows them to be toggled via `TAB` which is more memorable than the default binding
  * `C-x M-o`.
* Binds delete to navigating up to a parent directory.

```lisp
(use-package dired-x
  :defer 2
  :bind (:map dired-mode-map
           ("q"   . kill-dired-buffers)   ; Kill all dired buffers.
           ("TAB" . dired-omit-mode)      ; Toggle hiding dotfiles.
           ("DEL" . dired-jump))          ; Go up a directory.
  :hook ((dired-mode . dired-omit-mode))
  :config
    (setq dired-omit-verbose nil)
    (setq dired-omit-files "^\\...+$"))
```


### Navigation

This is a neat thing to navigate easily, it is bound to `C-c j` later in the keybinding setup.

```lisp
(use-package-straight ace-jump-mode
)
```


### NeoTree

[neotree](https://github.com/jaypei/emacs-neotree) presents a navigator for files/directories in a sidebar.

I'm experimenting with it:

```lisp
(use-package-straight neotree
  :defer 2
  :init
  (setq-default neo-smart-open  t
                neo-autorefresh t)
)

(defun skx-neotree()
  "Open the `neotree' sidebar, showing the directory containing the current buffer.

  Since `neo-smart-open' seems to silently fail for me, we also attempt to find the
  current file when we open the sidebar, and select it manually via `neotree-find'."
  (interactive)

  (let* ((dir  (if buffer-file-name (file-name-directory buffer-file-name) default-directory))
         (file (if buffer-file-name buffer-file-name)))
     (if (neo-global--window-exists-p)
      (neotree-hide)
      (progn
          (neotree-dir dir)
          (if file
              (neotree-find file))))))

```



## Docker

There is a handy [dockerfile-mode](https://github.com/spotify/dockerfile-mode) which allows highlighting docker-files.

Here we load it, and we can use `C-x C-b` to build the Dockerfile in the current buffer.

```lisp
(use-package dockerfile-mode
  :defer 2
  :mode
  ("Dockerfile\\'" . dockerfile-mode))
```



## Git

`git` setup is pretty much outside the scope of this document, but the least we can do is to configure a suitable mode for the `~/.gitconfig` file:

```lisp
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))
```

Of course we also wish to install/use magit which is the emacs git package:

```lisp
;; install magit via straight
(use-package-straight magit
  :defer 2
  :bind (("C-x g" . magit-status)))
```

`magit` allows easily switching branches, committing, etc, but the final step will be to open the browser to create a pull-request.  The following snippet binds that to `v` which is a synonym for `(v)iew`.

```lisp
(defun skx/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-current-remote)
                       "url"))
           (magit-get-current-branch))))

(eval-after-load 'magit
  '(define-key magit-mode-map "v"
     #'skx/visit-pull-request-url))
```



## Help

When using help-functions we want to make their buffers active by default:

```lisp
(setq help-window-select t)
```

Currently Apropos does not honour this setting, nor does the result of compilation.  Hopefully in the future this will change, for the moment we workaround the issue by making these two functions switch buffer via a mode-hook:

```lisp
(dolist (mode-hook '(apropos-mode-hook compilation-mode-hook))
  (add-hook mode-hook (lambda ()  (pop-to-buffer (current-buffer)))))
```



## Language Modes

Most of the time I spend in Emacs is for developing, and writing code.

Code I write tends to be in some combination of Lua, Ruby, Perl, Go, or C++.

**Note** The updates to `auto-mode-alist` end with `\\'`, which matches the end of the string (i.e. filename).  You might expect to see that written as "`.txt$`", but that would not be exactly the same, for filenames that contain a newline.  (`$` would match the newline, but `'` would match the end of the filename itself).


```lisp
;; Lua
(use-package lua-mode
  :defer 2
  :mode ("\\.lua\\'" . lua-mode))

;; Markdown
(use-package markdown-mode
  :defer 2
  :commands (markdown-mode gfm-mode)
  :mode  (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; Ruby
(use-package ruby-mode
  :defer 2
  :mode ("\\.rb" . ruby-mode))
```

The monkey programming language was introduced (and implemented!)
in the book "[Writing An Interpreter In Go](https://interpreterbook.com/)".

[My monkey implementation](https://github.com/skx/monkey/) contains several
enhancements, and comes complete with an emacs mode which we'll load here:

```lisp
(use-package monkey
  :defer 2
  :mode ("\\.mon" . monkey-mode))
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

(Obviously my [dotfiles](https://github.com/skx/dotfiles/) contain a copy of the appropriate files, see the [tools/resync-packages.el](tools/resync-packages.el) library which is what copies it from the upstream source into _this_ repository.)

The golang setup here is a bit special, because I use the LSP stuff to get completion, etc.  First of all we load that:

```lisp
(use-package-straight company
  :after yas
  :config
    (setq
       company-echo-delay 0
       company-idle-delay 0.2
       company-minimum-prefix-length 1
       company-tooltip-align-annotations t
       company-tooltip-limit 10
       company-tooltip-flip-when-above t
       company-dabbrev-downcase nil
       company-require-match nil
       company-begin-commands '(self-insert-command))
    (global-company-mode 1)
    (add-to-list 'company-backends 'company-yasnippet)
   (setq company-backends '((company-capf :with company-yasnippet)))
  )

;; Along with the LSP-modes
(use-package-straight lsp-mode)
(use-package-straight lsp-ui)

(use-package lsp-mode
  :config
    (setq lsp-auto-guess-root t))

(use-package-straight yasnippet
  :defer 2
  :config
    (setq yas-prompt-functions '(yas-ido-prompt))
    (yas-global-mode 1)
    (yas-reload-all))

(use-package-straight yasnippet-snippets
  :defer 2
  :after yas)

```

Once installed we can now ensure that the mode is loaded for the editing of `*.go` files, we also load company-mode and the LSP setup too:

```lisp
(use-package go-mode
  :defer 2
  :mode ("\\.go" . go-mode)
  :hook ((go-mode) . lsp-deferred))


;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (local-set-key (kbd "M-.") 'lsp-find-definition)
  (local-set-key (kbd "M-RET")    'pop-tag-mark))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
```

Note that I also setup [code-folding](#language-mode-helpers---code-folding) later in this file.


### Language Modes - JSON

JSON is pretty common-place, so we have to load this, and configure it for `*.json` files:

```lisp
(use-package-straight json-mode
   :defer 2
   :mode ("\\.json\\'" . json-mode))
```


### Language Modes - HCL

One of the tools I use most frequently for that is [Hashicorp](https://www.hashicorp.com/)'s [terraform](https://www.terraform.io/), and here we'll configure our buffers to be auto-formatted when we save them:

```lisp
(use-package terraform-mode
  :defer 2
  :config
    (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))
```


### Language Modes - Perl

I used to do a lot of my coding in Perl, and this is configured here.

First of all we want to ensure that we use `cperl-mode`, rather than `perl-mode`, and we wish to ensure that `*.t` are formatted in this mode too - as these files almost always contain perl test-cases in my experience:

```lisp
(use-package cperl-mode
  :defer 2
  :config
    ;;  We always prefer CPerl mode to Perl mode.
    (fset 'perl-mode 'cperl-mode)

    ;; Load .t files as perl too - as these are usually test-cases
    (setq auto-mode-alist (append '(("\\.t$" . cperl-mode)) auto-mode-alist))

    (setq cperl-indent-level 4)
    (setq cperl-brace-offset -2)
    (setq cperl-label-offset 0)
)
```

I've also created a simple utility package which contains a pair of helpers for Perl buffers:

* Reformat buffers, with `perltidy`, before saving.
  * This works with the [.perltidyrc](https://github.com/skx/dotfiles/blob/master/.perltidyrc) configuration file I have stored in my dotfiles.
* Run a syntax-check after saving.

**NOTE**: This can be abused as `perl -c ...` will evaluate code found in `BEGIN{ .. }` blocks.

```lisp
(use-package perl-utilities
  :defer 2
  :if (and (executable-find "perl") (not IS-MAC))
)
```

Note that I also setup [code-folding](#language-mode-helpers---code-folding) later in this file.


### Language Modes - Python

Here we perform the very minimal Python & LSP setup - however this is only configured if `pylsp` is present upon the system $PATH.

```lisp
(use-package python-mode
  :defer 2
  :if (locate-file "pylsp" exec-path)
  :mode ("\\.py" . python-mode)
  :hook ((python-mode) . lsp-deferred))

(defun lsp-python-install-save-hooks ()
  "When saving the buffer (re)format the imports, and also setup some keybindings"
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (local-set-key (kbd "M-.") 'lsp-find-definition)
  (local-set-key (kbd "M-RET")    'pop-tag-mark))

;; Only add the hook if pylsp exists.
(if (locate-file "pylsp" exec-path)
  (add-hook 'python-mode-hook #'lsp-python-install-save-hooks)
  (message "pylsp not found, ignoring hook for LSP"))
```

Jumping to definitions works at least, once you've installed the tooling:

```sh
% pip3 install python-lsp-server
```


### Language Modes - Web Mode

One of the annoyances with writing HTML is that often it contains extra things inline, such as Javascript and CSS.  To solve this problem - of wanting to mix HTML-mode along with Javascript-mode for example, I use [web-mode](http://web-mode.org/):

```lisp
(use-package web-mode
  :defer 2
  :mode (("\\.html\\'" . web-mode)
         ("\\.php\\'"  . web-mode)
         ("\\.erb\\'"  . web-mode))
  :init
   (setq web-mode-enable-current-element-highlight t)
   (setq web-mode-markup-indent-offset 2)
   (setq web-mode-css-indent-offset 2)
   (setq web-mode-code-indent-offset 2)
 )
```


### Language Modes - YAML Mode

YAML is used in Gitlab CI, Kubernetes, and other similar places.

```lisp
(use-package yaml-mode
  :defer 2
  :mode (("\\.yml\\'"   . yaml-mode)
         ("\\.yaml\\'"  . yaml-mode)))
```


### Language Modes - Z80 Assembly

I'm having fun doing "retro" things with a [Z80 processor](https://en.wikipedia.org/wiki/Zilog_Z80), so this mode loads the appropriate mode for that.

```lisp
(defun skx/z80-mode()
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'hs-block-start-regexp) "^; ?{")
  (set (make-local-variable 'hs-block-end-regexp) "^; ?}")
  (hs-minor-mode t)
  (set 'hs-block-start-regexp "^; ?{{")
  (set 'hs-block-end-regexp "^; ?}}")
  (local-set-key (kbd "M-C-i") 'hs-toggle-hiding)
  (local-set-key (kbd "M--") 'hs-hide-all)
  (local-set-key (kbd "M-+") 'hs-show-all))

(use-package z80-mode
  :defer 2
  :mode ("\\.z80$" . z80-mode)
  :hook ((z80-mode . skx/z80-mode)))

```


### Language Mode Helpers - Code Folding

I define a hook which will setup the toggling of code-blocks via HideShow, this will be enabled for C, C++, Golang & Perl-modes.

This also binds `Esc-TAB` to toggle the block under the point, and `Esc--` and `Esc-+` to hide/show all:

```lisp
(use-package hs-minor-mode
  :defer 2
  :hook prog-mode
  :bind
    (
    ("C-M-i" . #'hs-toggle-hiding)
    ("M--" . #'hs-hide-all)
    ("M-+" . #'hs-show-all)
    ))
```


### Language Mode Helpers - TODO Highlighting

The following snippet of code ensures that `TODO` comments/lines are shown easily, this also includes other examples such as:

* FIXME: blah, blah.
* TODO: here's some more stuff.
* XXX Blah, blah.


```lisp
(use-package my-todo
  :defer 2
  :config
    (add-hook 'text-mode-hook #'my/todo-fontify)
    (add-hook 'prog-mode-hook #'my/todo-fontify))

```


### Language Mode Helpers - Utilities

A lot of programming environments allow you to setup variables via something like this:

```
     int i = 1;
     int foo = 2;
```

Things look neater if they're aligned, thusly:

```
     int i   = 1;
     int foo = 2;
```

The following section of code lets us select a region and run `M-=` to align the section based upon the `=` sign:

```lisp
(defun align-equals (begin end)
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1))
```



## Link Following

The following allows following links, via RET, in all text/programming modes.

This is particularly useful for Markdown mode, and similar, where you'd otherwise want to use `C-c C-o`:

```link
(use-package goto-addr
  :bind
  (:map goto-address-highlight-keymap
        ("RET" . goto-address-at-point))
  :hook ((prog-mode . goto-address-prog-mode)
         (text-mode . goto-address-mode)))
```



## Mac OS

Macs are weird, so I've had to make some changes so that keybindings work as expected.


```lisp
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)
```

On top of that I wanted to make sure that the default font-sizes are "big":

```lisp
(defun td/adapt-font-size (&optional frame)
  "Attempt to make the font sizes bigger on larger displays."
  (let* ((attrs (frame-monitor-attributes frame))
         (size (alist-get 'mm-size attrs))
         (geometry (alist-get 'geometry attrs))
         (ppi (/ (caddr geometry) (/ (car size) 25.4))))
    (if (> ppi 120)
        (set-face-attribute 'default frame :height 200)
      (set-face-attribute 'default frame :height 150))))

(add-hook 'emacs-startup-hook (lambda () (td/adapt-font-size)))
(add-hook 'after-make-frame-functions (lambda (x) (td/adapt-font-size)))
```



## Org-Mode

`org-mode` is a wonderful thing which allows Emacs to hold tables, TODO-lists, and much much more.  For the moment I'm keeping document-specific lisp and configuration within the appropriate document, but there are some things that make working with `org-mode` nicer which will live _here_.

First of all we load the mode, and make some basic setup happen:

```lisp
(use-package org
  :defer 2
  :config
    ;; Don't track org-id-locations.
    ;; We don't want to see ~/.emacs.d/.org-id-locations
    (setq org-id-track-globally nil)

    ;;
    ;; A new link-type "copy" to copy things to the clipboard.
    ;; https://sachachua.com/blog/2024/01/org-mode-custom-link-copy-to-clipboard/
    ;;
    (org-link-set-parameters
        "copy"
            :follow (lambda (link) (kill-new link))
            :export (lambda (_ desc &rest _) desc))

    ;; indention should match headers.
    (setq org-adapt-indentation t)

    ;; When exporting code then we get highlighting
    (setq org-latex-listings t)

    ;; Hides blank lines between headings
    (setq org-cycle-separator-lines 0)

    ;; Avoid creating blank lines before headings.
    (setq org-blank-before-new-entry (quote ((heading) (plain-list-item . auto))))

    ;; Editing invisible text, inside folded regions, is an error.
    (setq org-catch-invisible-edits 'error)

    ;; Don't hide leading stars
    (setq org-hide-leading-stars nil)

    ;; Log when we're completing things.
    (setq org-log-done t)

    ;; All sections indented by default
    (setq org-startup-indented t)

    ;; All sections collapsed by default
    (setq org-startup-folded t)

    ;; This sets the indention-depth for child-entries
    (setq org-indent-indentation-per-level 2)

    ;; Ctrl-a & Ctrl-e (for start/end of line) behave "magically"
    ;; inside headlines this is what I think most people would expect
    (setq org-special-ctrl-a/e 't)

    ;; This hides the "*bold*", "/italic/" and "=preformatted=" markers:
    (setq org-hide-emphasis-markers t)

    ;; Ensure source blocks work naturally:
    (setq org-src-tab-acts-natively t)
    (setq org-src-fontify-natively t)

    ;; Header searches can match on substrings.
    (setq org-link-search-must-match-exact-headline nil)

    ;; Tags on items should be sorted alphabetically:
    (setq org-tags-sort-function 'org-string-collate-lessp)

    ;; Instead of showing ".." after folded-areas show the symbol.
    (setq org-ellipsis " ▼")

    ;; disable org-persist caching
    (setq org-element-cache-persistent nil)

  :hook
    ;; When I'm opening a document all code/example blocks will be hidden
    (org-mode . org-hide-block-all)
)
```

Lines will be wrapped to the width of the buffer too:

```lisp
(add-hook 'text-mode-hook 'visual-line-mode)
```

However when line-wrapping is used the `Ctrl-k` binding changes behaviour, to kill to the end of the visual line, not the real line.  We resolve this by undoing that:

```lisp
;; Ctrl-k for "kill-line" is remapped, unconditionally, in visual-line-mode.
;;
;; We want to restore the expected behaviour, so that C-k kills to the end of
;; the REAL line, not the VISUAL line.
(fset 'kill-visual-line 'kill-line)
```

I put together the [org-nested-links](https://github.com/skx/org-nested-links) package to allow refining links easily.  Allowing links to be augmented by refinements.  This is now loaded:

```lisp
(use-package org-nested-links
  :after org
  :defer 2)
```

As noted above it is possible to evaluated blocks of script from within `org-mode`, but shell-scripting is disabled by default so we need to enable this explicitly:

```lisp
(use-package ob-shell
  :defer 2
  :after org
  :commands
  org-babel-execute:sh
  org-babel-expand-body:sh
  org-babel-execute:bash
  org-babel-expand-body:bash)
```

Ensure that we can export org-blocks.  This is necessary for the CSS & Javascript export blocks in my various dairy-files.

```lisp
(use-package ob-org
  :after org
  :defer 2)
```

We'll also improve the default list-management functionality:

```lisp
(use-package org-autolist
  :after org
  :defer 2
  :hook (org-mode . org-autolist-mode))
```

`org-mode` is __all__ about lists!  So one thing that is nice is to visually update the display of the list-prefixes, via unicode characters.  We'll use `org-bullets` for that:

```lisp
(use-package org-bullets
  :after org
  :defer 2
  :hook (org-mode . org-bullets-mode))
```

Org examples are useful, here we define a function to wrap the selection with source-block

```lisp
(defun skx-org-src (beg end name)
  (interactive "r\nsName for this source block?")
  (save-excursion
    (narrow-to-region beg end)
    (set-mark nil)
    (goto-char (point-min))
    (if (not (= 0 (length name)))
        (insert (concat "\n#+NAME:" name)))
    (insert "\n#+BEGIN_SRC\n")
    (goto-char (point-max))
    (insert "\n#+END_SRC\n")
    (widen)))
```

A similar thing can be carried out by entering three graves:

```lisp
(use-package dig-my-grave
  :defer 2
  :after org)
```

Now we're done with the general setup so we'll handle the more specific agenda things here:

```lisp
(use-package org-agenda
  :defer 2
  :after org
  :bind
    ("C-c a" . org-agenda)
  :config
    ;; We store our org-files beneath ~/Private/Org.
    (custom-set-variables  '(org-directory "~/Private/Org"))

    ;; Populate the agenda from ~/Private/Org + ~/Private/Worklog/
    (setq org-agenda-files '("~/Private/Org" "~/Private/Worklog"))

  :custom
    ;; Our agenda-view will span two weeks by default.
    (org-agenda-span 14)

    ;; Don't split the window use the full frame
    (org-agenda-window-setup 'only-window)

    ;; But the agenda will start on the current day.
    (org-agenda-start-on-weekday nil)

    ;; We don't show tasks that are complete
    (org-agenda-skip-deadline-if-done t)
    (org-agenda-skip-scheduled-if-done t)

    ;; When showing TODO we show the headline from which the item came.
    (org-agenda-prefix-format "%-12:c %b")

    (org-agenda-custom-commands
     '(
        ("wi" "List of items closed in the past week."
          tags "+CLOSED>\"<-7d>\"/DONE")
        ("wq" "Quick (log) view"
          agenda ""
          ((org-agenda-span 14)
           (org-agenda-start-day "-7d")
           (org-agenda-start-with-log-mode t)
          ))
        ("wt" "Show today's stories"
          search (format-time-string "%Y-%m-%d"))
       ("wo" "Outstanding items - except those that are done, or unexported"
        todo ""
        ((org-agenda-skip-function (lambda () (org-agenda-skip-entry-if 'regexp ":noexport:\\|100%"))))
    ))))


;; Setup TODO-workflow, and colouring.
(setq org-todo-keywords '((sequence "TODO(!)" "INPROGRESS" "|" "DONE(!)" "BLOCKED" "CANCELED" "SPILLOVER")))
(setq org-todo-keyword-faces '(
    ("BLOCKED"    . (:foreground "violet" :weight bold))
    ("CANCELED"   . (:foreground "pink" :weight bold))
    ("INPROGRESS" . (:foreground "purple" :weight bold))
    ("SPILLOVER"  . (:foreground "red" :weight bold))
    ("TODO"       . (:foreground "blue" :weight bold))
    ))
```

Since we're hiding the emphasis markers it can be hard to edit text which is formatted.  To handle that we use [org-appear](https://github.com/awth13/org-appear):

```lisp
(use-package org-appear
  :after org
  :defer 2
  :config
    (setq org-appear-autolinks t)
  :hook
    ((org-mode . org-appear-mode)))
```

Since we're living in the future we can use `org-mouse` for checking boxes, etc:

```lisp
(use-package org-mouse
  :defer 2
  :after org)
```


### Org-Mode Code Execution

Another useful change to org-mode is allowing the ability to execute the Emacs lisp contained within a particular block when a file is loaded.

The following configuration enables the contents of a block named `skx-startblock` to be executed automatically when the file is loaded, and the block `skx-saveblock` to be evaluated once _before_ a file is saved:

```lisp
(use-package org-eval
  :defer 2
  :after org
  :init
    (setq org-eval-prefix-list     (list (expand-file-name "~/Private/"))
          org-eval-loadblock-name "skx-startblock"
          org-eval-saveblock-name "skx-saveblock" ))
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

By default `org-mode` will prompt you to confirm that you want execution to happen, but we use `org-eval-prefix-list` to enable whitelisting particular prefix-directories, which means there is no need to answer `y` to the prompt.


### Org-Mode Diary

I keep a work-log where I write down tasks and notes about my working-life every day.

Within my diary I want quick access to Jira (ugh), so I have a package to turn references into links:

```lisp
(use-package linkifier
  :defer 2)
```

The diary itself is handled by my [org-diary](https://github.com/skx/org-diary) package, and here we load it and allow quick access to my journal:

```lisp
(use-package org-diary
  :defer 2
  :after (org linkifier)
  :autoload org-diary-mode
  :init
    ;; ensure we're loaded
    (add-to-list 'auto-mode-alist
             '("[dD][iI][aA][rR][yY]\\.[oO][rR][gG]" . org-diary-mode))

    ;; Add a new tag to all entries, after creating them.
    (add-hook 'org-diary-after-new-entry-hook
                (lambda()
                  (org-set-tags (format-time-string "%Y_week_%V"))))

    ;; Hide all diary-blocks
    (add-hook 'org-diary-mode-hook (lambda () (interactive) (org-hide-block-all))))

;; Show the current section, with children expanded.
(defun org-show-current-heading-tidily ()
  "Show the current section, with children expanded."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (org-show-subtree)))

;; Create a helper to load my diary, show today's entry, and unfold nicely.
(defun skx-load-diary()
  "Load my diary/work-log, and scroll to today's entry."
  (interactive)
    (require 'org-diary)
    (find-file (expand-file-name "~/Private/Worklog/Diary.org"))
    (if (org-diary-today)
      (org-show-current-heading-tidily)))
```


### Org-Mode Link Following

There exists a setting for following links by pressing `RETURN`, `org-return-follows-link`, however this doesn't work inside table-cells.

To resolve this, and add extra features I created `org-return` allows following links, even inside tables, and toggling the state of checkboxes:

```lisp
(use-package org-return
  :defer 2
  :after org
  :config
       (add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'org-return-handler))))
```

When following links `C-RETURN` moves backwards, after following links:

```lisp
(add-hook 'org-mode-hook (lambda ()
  (local-set-key (kbd "<C-return>") 'org-mark-ring-goto)))
```

On the topic of links, sometimes org-mode things that things are links which are not, and kills exporting.  We want to ignore that behaviour:

```lisp
;; I just want an export.
;; If I wanted a link check, I'd ask for one.
(setq org-export-with-broken-links t)
```


### Org-Mode Tag Cloud

I put together a simple tag-cloud helper package, which we'll now load:

```lisp
(use-package org-tag-cloud
  :after org
  :disabled
  :defer 2
  :config
   (add-hook 'org-mode-hook 'skx-org-mode-hook-eval-ok))
```

To make it useful we'll ensure that we disable warnings about eval which would otherwise be shown when following such a link.

```lisp
; Allow eval-links
(defun skx-org-mode-hook-eval-ok ()
    (make-variable-buffer-local 'org-link-elisp-skip-confirm-regexp)
    (setq org-link-elisp-skip-confirm-regexp "org-tags-view"))
```


### Org-Mode Tagging Tasks Easily

I put together a simple helper to auto-tag TODO-tasks, using tags from within the current document:

```lisp
(use-package org-auto-tag
  :after org
  :defer 2
  :config
     (add-hook 'org-after-todo-state-change-hook 'org-auto-tag)
)
```


### Org-Mode Utility Functions

I've put together a simple collection of utility-functions for org-mode files:

* org-utils-header-prop - Return a named property from an org-file header.
* org-utils-random-headline - Jump to a random headline.
* org-utils-table-next - Jump to the next table.
* org-utils-table-prev - Jump to the previous table.

These can be found within the `org-utils.el` package:

```lisp
(use-package org-utils
  :after org
  :defer 2)
```


### Org-Mode Viewing Exported Documents

Typically when I export documents I work with them elsewhere, but the export options (accessible via `C-c C-e`) have options for exporting and viewing:

* Export to HTML and Open
* Export to PDF and Open

I want those exports to open in `firefox` and `evince` respectively, the following snippet makes that happen:

```lisp
(defun skx/find-browser ()
  (cond
    ((file-exists-p "/usr/bin/firefox")     "/usr/bin/firefox")
    ((file-exists-p "/opt/firefox/firefox") "/opt/firefox/firefox")
    ((file-exists-p "/Applications/Firefox.app/Contents/MacOS/firefox") "/Applications/Firefox.app/Contents/MacOS/firefox")
    ((executable-find "firefox")            (executable-find "firefox"))
  ))

(defun org-file-apps-html (file link)
  (interactive)
  (shell-command (format "%s %s" (skx/find-browser) file)))

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

```txt




#




# NOTE: STEVE: TODO: This is broken for the moment




#
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


### Org-Mode Secrets

Sometimes org-mode files contain secrets, things that you don't want to make visible to other people.  One common solution is to encrypt the contents of particular regions with GPG.

You can run `M-x org-decrypt-entries` to make them visible, but re-encrypt any time you save:

```lisp
(use-package org-crypt
  :defer 2
  :after org
  :config
    (org-crypt-use-before-save-magic)
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  :custom
    (org-crypt-key "root@localhost"))
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



## Quick File Access

There are some files that are accessed more than others, so here I setup a couple of helpers for those.


### Quick File Access - Emacs Init File

This is a handy function I use if I need to edit _this_ initialization file:

```lisp
(defun skx-load-init()
  "Load my init.md file."
  (interactive)
    (find-file (expand-file-name "~/.emacs.d/init.md")))
```


### Quick File Access - Github Projects

Opening a github project is something I do often, and in my case I have all my repositories cloned beneath ~/Repos/github.com, for example:

* `~/Repos/github.com/skx/foo`
* `~/Repos/github.com/skx/bar`
* `~/Repos/github.com/user/one`
* `~/Repos/github.com/user/two`



```lisp
(defvar github-prefix "~/Repos/github.com/" "Root of github projects.")

(defun skx-github-project ()
  "Open a github project, with completion."
  (interactive)
  (let ((pr nil))
    (setq pr (ido-completing-read "Select project: " (skx-github-projects-available github-prefix)))
    (if pr
        (dired (concat github-prefix pr))
      (message "Nothing selected"))))

(defun skx-github-projects-available (prefix)
  (mapcar
     (lambda (x) (substring x (length prefix)))
     (file-expand-wildcards (concat prefix "*/*"))))
```


### Quick File Access - Lisp Scratch Buffer

Having easy access to a new lisp buffer is useful, so I've defined the following to give me a new "scratch" buffer:

```lisp
(defun skx-scratch-buffer()
  "Create a unique scratch-buffer, and switch to it"
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*scratch*"))
  (insert "; Scratch buffer, kill it when you're done\n\n")
  (lisp-mode))
```


### Quick File Access - Recent Files

We keep track of recent files that have been opened, we prune the list every time we launch and we ensure that
we remove the `ido.last` file which is populated by the ido completion-framework:

```lisp
(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  :init
  (setq recentf-max-menu-items  100
        recentf-max-saved-items 300
        recentf-save-file (expand-file-name "~/.trash.d/emacs.recent.files")
        recentf-exclude         '("/auto-install/" "/.trash.d/" "COMMIT_EDITMSG" ".gz" "~$" "/tmp/" "/ssh:" "/sudo:" "/scp:" "go.mod" "go.sum")
        recentf-auto-cleanup 600))
```

Here's a function that allows opening a recent file from the minibuffer, with completion:

```lisp
(defun recentf-open-with-completion ()
  (interactive)
    (let* ((tocpl (mapcar (lambda (x) (cons (file-name-nondirectory x) x))
                            recentf-list))
             (fname (completing-read "File name: " tocpl nil nil)))
        (when fname
          (find-file (cdr (assoc-string fname tocpl))))))
```

Now we can view a list of recently-opened files via `C-c r`:

```lisp
(use-package recentf-buffer
  :defer 2
  :ensure nil
  :bind
    (("C-c r"   . recentf-open-files-in-simply-buffer)
     ("C-c C-r" . recentf-open-with-completion)))
```

If we don't have recentf-find-file we'll fake it:

```lisp
(if (not (fboundp 'recentf-find-file))
  (fset 'recentf-find-file 'find-file))

```



## Save Place In Visited Files

This ensures we're restored to where we were previously, if possible.

```lisp
(setq save-place-file
    (expand-file-name "~/.trash.d/emacs.place.history"))

(if (fboundp #'save-place-mode)
  (save-place-mode +1)
  (setq-default save-place t))
```



## Searching

I set "search-default-mode" to allow me to match `äiti` when searching for `aiti`, for example.

```lisp
(setq search-default-mode 'char-fold-to-regexp)
```



## Spell-Checking

I use `flyspell` as a spell-checker when editing text and programming-mode files.

Typos and errors will be underlined, and `M-TAB` or middle-click can be used to correct errors which are shown.

```lisp
(use-package flyspell
  :if (executable-find "ispell")
  :ensure nil
  :defer 2
  :init
  (progn
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)
    )
  :config
  ;; ispell should not check code blocks in org mode
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_example " . "#\\+end_example$"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE " . "#\\+END_EXAMPLE$"))
  )

```



## Syntax Checks

The [save-check.el](https://github.com/skx/save-check.el/) package allows simple syntax-checks to be made _after_ files have been saved, here we load it:

```lisp
(use-package save-check
  :config
    ; Show lisp errors, if present
    (setq  save-check-show-eval t)
    (global-save-check-mode t))
```

I've found that sometimes kubernetes doesn't like duplicated keys, more specifically kustomize.  To attempt to deal with that we'll define a function to alert on those:

```lisp
(defun save-check-custom-yaml ()
  "Alert if there are duplicate top-level keys in the current buffer.

This function is primarily written to be invoked by `save-check' on YAML buffers,
but the code is actually not specific."
  (interactive)
  (let ((matches))
    ;; get the matches - i.e. top-level keys
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (goto-char 1)
          (while (search-forward-regexp "^\\([a-zA-Z0-9]+\\):" nil t 1)
            (push (match-string-no-properties 0) matches)))))
    ;; now matches contains a list of all top-level keys - look for any duplicate entries
    (setq matches (delete-dups (seq-filter
                                (lambda (el) (member el (cdr (member el matches))))
                                matches)))

    ;; remove false-positives which might occur more than once even without error.
    (dolist (known '("apiVersion:" "kind:" "metadata:" "spec:"))
      (setq matches (delete known matches)))

    ;; if we have duplicates - format the response based on one vs. multiple duplicates.
    (if matches
        (if (= 1 (length matches))
            (format "There is a duplicate top-level key %s" matches)
          (format "There are %d duplicate top-level keys %s" (length matches) matches))
      nil)))
```

Now we can add that function and configure it to be enabled for all YAML files:

```lisp
(add-to-list 'save-check-config
      '(:mode yaml-mode
        :eval (save-check-custom-yaml)))

```



## Unix Setup

The following section helper ensures that files are given `+x` permissions when they're saved, if they contain a valid shebang line:

```lisp
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
```

Finally we allow Emacs to control our music playback, which is supplied by [MPD](http://www.musicpd.org/).  There are several different MPD client-modes available for Emacs, including one built into emacs, this is my own and has a more minimal / less confusing UI:

```lisp
(use-package mpc
  :ensure nil
  :defer 2)
```



## User Interface Setup

I prefer to keep a reasonably minimal look, so I disable the toolbar and scroll-bars.

The menu-bar is somewhat useful as I'm slowly learning more about `org-mode`, so I'll leave that enabled unless I'm running in a terminal.

```lisp
(defun skx/almost-fullscreen()
  "Make the display almost full-screen, leaving some padding."
  (when window-system
    (let ((offset 50))
      (set-frame-position (selected-frame) 0 0)
      (set-frame-size (selected-frame) (- (display-pixel-width) (* 3 offset))  (- (display-pixel-height) (* 3 offset)) t))))

;; Now make it live
(skx/almost-fullscreen)

;; Disable the scroll-bars, and the tool-bar.
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; Give the fringe some more space
(set-fringe-mode 20)

;; Show the menubar only when running with graphics
(menu-bar-mode (display-graphic-p))

;; Make sure our cursor doesn't get in the way.
(use-package avoid
  :defer 2
  :ensure nil
  :config
    (mouse-avoidance-mode 'cat-and-mouse))
```

Once the basics have been setup the next step is to configure our theme:

```lisp
(load-theme 'wombat)
```

Now we've tweaked the GUI we can setup the clipboard integration:

```lisp
;; Copying in emacs should allow pasting into gnome-terminal, etc.
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)
```

Once we've removed things that we don't like the next section is responsible for configuring the colours - first of all the global theme which is used for colours, and then secondly the colour of the cursor:

```lisp
(setq-default cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
              cursor-type '(hbar . 4)            ; Underline-shaped cursor
              cursor-intangible-mode t           ; Enforce cursor intangibility
              x-stretch-cursor t)                ; Stretch cursor to the glyph width

(blink-cursor-mode 1)                            ; We want to blink

(use-package cursor-colour
  :defer 2)
```

Lisp famously uses a lot of parenthesis, but so does Python, Perl, and many other languages.  The following section highlights expressions inside parenthesis in a cute way:

```lisp
(use-package paren
  :defer 2
  :ensure nil
  :config
    (setq show-paren-style 'expression)
    (setq show-paren-when-point-in-periphery t)
    (setq show-paren-when-point-inside-paren t)
    (setq show-paren-ring-bell-on-mismatch t)
    (setq show-paren-delay 0)
    (show-paren-mode t))
```

The following section takes care of setting up other basic and global things the way that I prefer them.

```lisp
;; Avoid the annoying startup message.
(setq-default
      inhibit-startup-screen t               ; Disable start-up screen
      inhibit-startup-message t              ; Disable startup message
      inhibit-startup-echo-area-message t    ; Disable initial echo message
      )

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

;; Show the file, and only the file, we've got loaded in the frame title.
(setq frame-title-format "%b")

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


### User Interface Setup - Mode Line

We'll change the colours here to make things stand out:

```lisp
(set-face-background 'mode-line           "red")
(set-face-background 'mode-line-inactive  "black")
```

We want to remove the percentage display on the mode-line:

```lisp
;; Don't show the percentage (or "All"/"Top"/"Bot") on the mode-line
(setq mode-line-percent-position nil)
```


### User Interface Setup - Mode Line Cleanup

We'll prevent **all** minor-modes from cluttering the mode-line, by
removing their settings from it (by replacing the output with "").

```lisp
(use-package mode-line-major)
```



## User Interface Sidebar

There is a neat package `imenu-list` which allows you to view a sidebar in the current frame, containing index-entries.

Here I configure it to be used for both general programming modes, as well as `org-mode`:

```lisp
(use-package imenu-list
  :defer 2
  :ensure nil
  :init
    (setq imenu-list-focus-after-activation t
          imenu-list-auto-resize t
          imenu-list-position 'left)
  :bind
    (("M-i" . #'imenu-list-smart-toggle))
)
```



## UTF-8

UTF-8 is the future, we should greet it with open-arms.

```lisp
(use-package emacs
  :init
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))
```



## Whitespace Handling

We like to remove trailing whitespace when we save files, and we make it visible by default:

```lisp
(use-package whitespace
  :defer  2
  :ensure nil
  :config

   ; show trailing whitespace
   (setq-default show-trailing-whitespace t)

   ; remove trailing whitespace on-save
   (add-hook 'write-file-hooks 'delete-trailing-whitespace)

   ; but make sure we keep a final newline
   (setq require-final-newline t))
```

When running Emacs upon a terminal, rather than graphically, lines that are too long have a "`\`" character added to them.  This makes copying and pasting to other terminal-applications annoying.  Disable that wrapping behaviour here:

```lisp
(set-display-table-slot standard-display-table 'wrap ?\ )
```



## Whitespace Handling - Vertical Space Handling

In addition to the general whitespace handling I also prefer to have consistent vertical spacing between headers/blocks in `org-mode` and `markdown-mode` files:

```lisp
(use-package vertical-space-cleanup
  :defer 2
  :config
    (add-hook 'org-mode-hook
                (lambda()
                   (add-hook 'before-save-hook #'vertical-space-cleanup t t)))

    (add-hook 'markdown-mode-hook
                (lambda()
                   (add-hook 'before-save-hook #'vertical-space-cleanup t t)))
  )
```

This ensures that headings are seperated from any previous content by a consistent amount.



## XXX - BugFixes

Weirdly I noticed that `rmail-after-save-hook` was always present in the `after-save-hook` list, and that feels wrong:

```lisp
;; Remove the hook, if it is present
(if (member 'rmail-after-save-hook after-save-hook)
    (remove-hook 'after-save-hook #'rmail-after-save-hook))
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

;; utilities
(define-key steve-mode-map (kbd "M-g")   'goto-line)
(define-key steve-mode-map (kbd "M-=")   'align-equals)

;; Open specific files; init, scratch, github-repo, diary
(define-key steve-mode-map (kbd "C-c e") 'skx-org-src)
(define-key steve-mode-map (kbd "C-c g") 'skx-github-project)
(define-key steve-mode-map (kbd "C-c i") 'skx-load-init)
(define-key steve-mode-map (kbd "C-c s") 'skx-scratch-buffer)
(define-key steve-mode-map (kbd "C-c w") 'skx-load-diary)
(define-key steve-mode-map (kbd "C-c d") 'skx-neotree)

;; ace-jump-mode
(define-key steve-mode-map (kbd "C-c j") 'ace-jump-mode)
(define-key steve-mode-map (kbd "<C-return>") 'ace-jump-mode-pop-mark)

;; change sizes
(define-key steve-mode-map (kbd "C-+") 'text-scale-increase)
(define-key steve-mode-map (kbd "C--") 'text-scale-decrease)
(define-key steve-mode-map (kbd "C-s") 'isearch-forward)
(define-key steve-mode-map (kbd "C-r") 'isearch-backward)

;; agenda
(define-key steve-mode-map (kbd "C-c a") 'org-agenda)

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
                        (if (fboundp 'emacs-uptime) (message "Emacs uptime %s" (emacs-uptime)))
                        (sleep-for 1)))
                  (save-buffers-kill-terminal)))
            (message "emacs quit aborted")))

(define-key steve-mode-map (kbd "C-v")
    '(lambda ()
        (interactive)
        (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t)))

;; HOME will go to the start of the indent or the start of the line.
;; Ctrl-HOME for beginning of buffer
; then the start of the buffer.
(define-key steve-mode-map (kbd "<home>")
    '(lambda ()
        (interactive)
        (if (bolp)
           (skip-chars-forward " \t")
           (beginning-of-line))))

;; END will move to the end of the line, or the last non-whitespace character
;;
;; Ctrl-END will go to end of the buffer
(define-key steve-mode-map (kbd "<end>")
    '(lambda ()
        (interactive)
        (if (eolp)
           (skip-chars-backward " \t")
         (end-of-line))))

; unset things
(define-key steve-mode-map (kbd "C-z")     '(lambda () (interactive)))
(define-key steve-mode-map (kbd "C-c C-z") '(lambda () (interactive)))
```
