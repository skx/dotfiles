# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Source local files.
if [ -d ~/.bash/ ]; then
   for i in ~/.bash/*; do
       . $i
   done
fi

