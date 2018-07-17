dotfiles
========

This is my "dotfile" repository.  Visitors seem to most interested in the setup
of GNU Emacs:

* [.emacs.d/init.md](.emacs.d/init.md)
  * This is a markdown file which contains all my emacs setup.
* [.emacs.d/init.el](.emacs.d/init.el)
  * This file is read at startup-time by Emacs, and executes the content of the markdown file.



Using The Repository
--------------------

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
