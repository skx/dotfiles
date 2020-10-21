" *.t files are Perl test files
au FileType pl,pm,t set filetype=perl
au FileType perl set makeprg=perl\ -c\ %\ $*
au FileType perl set errorformat=%f:%l:%m
au FileType perl call PerlMode()
au FileType perl set autowrite
au FileType perl :noremap K :!perldoc <cword> <bar><bar> perldoc -f <cword><cr>

au BufNewFile,BufRead *.pl,*.pm,*.t     setf perl

function! PerlMode()            " Stolen from David Hand
    set shiftwidth=4            " sw:  a healthy tab stop
    set textwidth=72            " tw:  wrap at 72 characters
    set autoindent              " ai:  indent to match previous line
    set cindent                 " cin:  Use C-indenting
    set cinkeys=0{,0},!^F,o,O,e " cink:  Perl-friendly reindent keys
    set cinoptions=t0,+4,(0,)60,u0,*100  " cino:  all sorts of options
    set cinwords=if,else,while,do,for,elsif,sub
    set comments=n:#            " com:  Perlish comments
    set formatoptions=crql      " fo:  word wrap, format comments
    set nosmartindent           " nosi:  Smart indent useless when C-indent is on
    set showmatch               " show matches on parens, bracketc, etc.
endfunction

