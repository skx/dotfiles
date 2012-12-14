"
" We don't need to be compatible
"
set nocompatible
filetype off

"
" Call Pathogen to load our bundles
"
"   https://github.com/tpope/vim-pathogen
"
"
" This loads things from beneath ~/.vim/bundles/
"
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

"
" Now we can re-enable file-type detection
"
filetype plugin indent on

"
" Any local file will be included, if it exists.
"
if filereadable(glob("~/.vimrc.local"))
    source ~/.vimrc.local
endif
