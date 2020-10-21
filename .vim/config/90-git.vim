"
" When editing a git-commit file always move the cursor to the start
" of the file.
"
autocmd FileType gitcommit call setpos('.', [0, 1, 1, 0])
