#
# NOTE: ~/.bash_logout is only run by a login shell.
#



#
# Clear the screen when logging out
#
# $SHLVL is set by bash, and incremented once for each child
# shell that is being used.
#
if [ "$SHLVL" = 1 ]; then
    [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
    [ -x /usr/bin/clear ]         && /usr/bin/clear
fi


#
# Expire any active sudo-sessions
#
which sudo >/dev/null && sudo -k 2>/dev/null >/dev/null
