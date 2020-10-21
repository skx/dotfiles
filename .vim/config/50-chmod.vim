
"
" automatically give executable permissions if file begins with #! and contains
" '/bin/' in the path
"
function ModeChange()
  if getline(1) =~ "^#!"
    if getline(1) =~ "/bin/"
      silent !chmod a+x <afile>
    endif
  endif
endfunction

au BufWritePost * call ModeChange()
set autoread
