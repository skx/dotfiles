"
" Make things 100% explicit if a buffer is read-only.
"
" This comes from here:
"
"      http://www.reddit.com/r/vim/comments/eljev/avoid_trying_to_edit_readonly_files/
"
au BufNew,BufAdd,BufWrite,BufNewFile,BufRead,BufEnter,FileChangedRO * :if &ro | hi StatusLine guifg=Red guibg=black ctermbg=black ctermfg=red | :else | hi StatusLine guibg=White guifg=blue ctermbg=white ctermfg=blue | endif
