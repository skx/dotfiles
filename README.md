dotfiles
========

Yet another dotfile-repository of shell files, and similar.


Using The Repository
--------------------

This repository was created as a "bare" repository, so that it can track files in my home directory.

Because of this the checkout process is a little different than usual.  Here is my recipe:


Step one: checkout the repository:

     git clone --bare git://github.com/skx/dotfiles.git ~/dotfiles.git

Step two: setup a sane alias

     alias .G="git --git-dir=$HOME/dotfiles.git --work-tree=$HOME/"

Step three: See which files will be over-written in your current home directory:

     .G status -s -uno

Step four:  If no files will be corrupted, or you're happy to replace them, do the necessary:

     .G checkout -b -f original_files 
     .G commit -a  -m 'original files'
     .G checkout master 

Now you're golden.  You can update as often as you like, and that will fetch the files and changes I commit.
