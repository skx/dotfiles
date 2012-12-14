"============================================================================
" Functions
"----------------------------------------------------------------------------
function! TextMode()            " Stolen from David Hand
    set nocindent               " nocin:  don't use C-indenting
    set nosmartindent           " nosi:  don't "smart" indent, either
    set autoindent              " ai:  indent to match previous line
    set noshowmatch             " nosm:  don't show matches on parens, brackets, etc.
    set comments=n:>,n:#,fn:-   " com: list of things to be treated as comments
    set textwidth=72            " tw:  wrap at 72 characters
    set formatoptions=tcrq      " fo:  word wrap, format comments
    set dictionary+=/usr/local/dict/*  " dict:  dict for word completion
    set complete=.,w,b,u,t,i,k  " cpt:  complete words

"
"  British Spelling is the good thing.
"
"  Keys:
"
"     ]s  -> Next spelling error
"     [s  -> Previous spelling error.
"     z=  -> Suggestions on spelling.
"
        set spelllang=en_gb
        set spell

endfunction

"============================================================================
" Autocommands
"----------------------------------------------------------------------------
" For all text files set 'textwidth' to 78 characters.
au FileType text setlocal tw=78

au FileType text call TextMode()
au FileType mail call TextMode()

au BufNewFile,BufRead *.txt             setf text
