fun! DeleteTrailingWhitespace()
    let _s=@/
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
    let @/=_s 
endfun
command! -nargs=0 DeleteTrailingWhitespace call DeleteTrailingWhitespace()
