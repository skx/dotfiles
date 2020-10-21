" continue searching at top when hitting bottom
set wrapscan

" searches are case insensitive
set ignorecase

" searches are incremental
set incsearch

" highlight searches, but allow them to be disabled via ctrl-l
set hlsearch
nnoremap <silent> <C-l> :nohl<CR><C-l>
