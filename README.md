# dotfiles

This is my _dotfiles_ repository.

## Emacs Setup

Visitors to this repository seem to be primarily interested in my GNU/Emacs setup, which is handled in a somewhat "literate" fashion, by virtual of being written in Markdown format:

* [.emacs.d/init.md](.emacs.d/init.md)
  * This is a markdown file which contains most of my emacs setup.
* [.emacs.d/init.el](.emacs.d/init.el)
  * This file is read at startup-time by Emacs, and executes the content of the markdown file.
  * It will also load some org-mode files, if they exist.

The contents of the emacs directory are laid out in a logical collection of sub-directories.  Some specific files are highlighted here, but there might be others of interest located beneath the given directories.

* [Language modes](.emacs.d/lang/)
  * cfengine, docker, go, hcl, etc, etc.
* [Org-Mode Stuff](.emacs.d/org)
  * [org-eval.el](.emacs.d/org/org-eval.el) Execute the contents of named blocks on file-load or save
* [Misc. stuff](.emacs.d/misc)
  * Show recent files, report when I should take a break, etc.
* [User interface](.emacs.d/ui)
  * [ui/my-todo.el](.emacs.d/ui/my-todo.el) - Highlight TODO-comments.
  * [ui/linkifier.el](.emacs.d/ui/linkifier.el) - Turn text matching regular expressions into links/buttons
* [Unix stuff](.emacs.d/unix)
  * MPC (music player control)
  * Autoformat, and lint, perl files on-save.
* Use Package
  * A clone of the [use-package repository](https://github.com/jwiegley/use-package), to keep this repository self-contained.


### Shell Setup

The `bash` setup is pretty minimal, the startup files just source every file located beneath [~/.bash/](.bash/) (and `~/.bash.local` if it exists).  This allows me to group setup in a small number of files which is easier to organize.

# Using The Repository

This repository was created as a "bare" repository, so that it can track files in my home directory.  Because of this the checkout process is a little different than usual.

Here is my recipe for deployment on a new host:

Step one: checkout the repository:

If you wish to get read-only access, and you're never going to push updates:

     git clone --bare git://github.com/skx/dotfiles.git ~/dotfiles.git

If you have permission you can pull the repository, and gain access to push it:

     git clone --bare git@github.com:skx/dotfiles.git ~/dotfiles.git

Step two: setup a sane alias

     alias .DOTFILES="git --git-dir=$HOME/dotfiles.git --work-tree=$HOME/"

Step three: See which files will be over-written in your current home directory:

     .DOTFILES status -s -uno

Step four:  If no files will be corrupted, or you're happy to replace them, do the necessary:

     .DOTFILES checkout -b  original_files -f
     .DOTFILES commit -a  -m 'original files'
     .DOTFILES checkout master

Now you're done.

It is perhaps more sane to merely use the github browser to cherry-pick the files and functions you want, but each to their own.

# Removing README.md

Once you've clone the repository to your local system you'll find that you have a README.md file in your home-directory, which some might find annoying.  Ideally I'd not have this in the repository but without the instructions the repository would be hard to find/understand for github users.

You can remove the file like so:

    $ .DOTFILES update-index --assume-unchanged README.md
    $ rm README.md

If you're curious you can see other files that are being ignored:

    $ .DOTFILES ls-files -v | grep ^h

If you ever want to restore the file:

    $ .DOTFILES update-index --no-assume-unchanged README.md
