#
# NOTE: ~/.bash_logout is only run by a login shell.
#



#
# Clear the screen when logging out
#
if [ "$SHLVL" = 1 ]; then
    [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
else
    ~/bin/clear
fi



#
# Expire any active sudo-sessions
#
which sudo >/dev/null && sudo -k



