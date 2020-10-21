"========================
" Syntaxhiglighting
"========================
let color = "true"

if has("syntax")
    if color == "true"
        " This will switch colors ON
        so ${VIMRUNTIME}/syntax/syntax.vim

        " This will highlight trailing whitespace
        highlight RedundantSpaces ctermbg=red guibg=red
        match RedundantSpaces /\s\+$\| \+\ze\t/

    else
        " this switches colors OFF
        syntax off
        set t_Co=0
    endif
endif

" Use brighter colors if your xterm has a dark background.
if &term =~ "xterm"
   set background=dark
endif
