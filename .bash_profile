# If not running interactively, don't do anything
[ -z "$PS1" ] && return

if [ -d ~/.bash/ ]; then
   for i in ~/.bash/*; do
       echo "Sourcing $i"
       . $i
   done
fi
