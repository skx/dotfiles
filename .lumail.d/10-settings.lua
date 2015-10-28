--
-- This setting is used to specify the prefix for your maildir hierarchy.
--
-- (The directory specified will be processed recursively.)
--
prefix = os.getenv( "HOME" ) .. "/Maildir"

--
-- Ensure the directory exists.
--
if ( is_directory(prefix) ) then
   maildir_prefix(prefix)
else
   abort( "The specified Maildir prefix isn't a directory: " .. prefix )
end

--
-- Specify the outgoing mail binary.
--
-- Prefer /usr/sbin/sendmail, but use /usr/lib/sendmail if that exists
--
if ( executable( "/usr/sbin/sendmail" ) ) then
   sendmail_path( "/usr/sbin/sendmail -t" )
end
if ( executable( "/usr/lib/sendmail" ) ) then
   sendmail_path( "/usr/lib/sendmail -t" )
end

--
-- Show all folders by default
--
-- When called with no arguments this function will return the current
-- setting, otherwise it will update it.  This is a common pattern for lumail,
-- each of the functions which can get or set a value works in the same way.
--
-- The maildir limit can be set to three values:
--
--    all  -> Show all maildir folders.
--    new  -> Show all maildir folders which contain unread messages.
--   "pat" -> Show all maildir folders which match the regular expression "pat".
--
-- See "all_folders" , "new_folders", and "livejournal_folders" functions for
-- example of use.
--
maildir_limit( "all" )


--
-- The maildir format controls how maildirs are displayed in maildir-mode.
--
-- Valid options are currently limited to:
--
--   $CHECK  - "[x]" if the folder is selected, "[ ]" otherwise.
--   $TOTAL  - The total number of messages in the maildir.
--   $NEW    - The number of unread/new messages in the maildir.
--   $UNREAD - The number of unread/new messages in the maildir.
--   $READ   - The number of read/old messages in the maildir.
--   $PATH   - The maildir path.
--   $NAME   - The maildir name (i.e. dirname($PATH)).
--
maildir_format( "$CHECK - $UNREAD/$TOTAL - $PATH" )


--
-- When opening folders we'll show all messages.
--
-- Valid options are:
--
--        all  -> Show all message.
--        new  -> Show all unread messages.
--       "pat" -> Show all messages which match the regular expression "pat".
--
index_limit( "all" )


--
-- The index format controls how messages are displayed inside folder lists.
--
-- Valid options are currently limited to:
--
--   $DAY   (day of week)
--   $MONTH (month name)
--   $MON   (month abbreviation)
--   $YEAR  (year)
--   $DATE
--   $FLAGS
--   $FROM
--   $SUBJECT
--   $TO
--
--
index_format( "[$FLAGS] $DAY/$MONTH/$YEAR $FROM - $SUBJECT" )





--
-- The headers we show when displaying a mail.
--
-- (The headers listed are shown in the order they are specified here.)
--
headers = { "$TO", "$CC", "$FROM", "$DATE", "$SUBJECT", "$X-Remote-Host", "$X-Remote-IP", "$X-DKIM-RESULT" }

--
-- The maildir format controls how maildirs are displayed in maildir-mode.
--
-- Valid options are currently limited to:
--
--   $CHECK  - "[x]" if the folder is selected, "[ ]" otherwise.
--   $TOTAL  - The total number of messages in the maildir.
--   $NEW    - The number of unread/new messages in the maildir.
--   $UNREAD - The number of unread/new messages in the maildir.
--   $READ   - The number of read/old messages in the maildir.
--   $PATH   - The maildir path.
--   $NAME   - The maildir name (i.e. dirname($PATH)).
--
maildir_format( "$CHECK - $UNREAD/$TOTAL - $PATH" );

--
-- There is only one folder which is special, and that is the one where
-- lumail will record copies of outgoing mail(s).
--
if ( is_maildir( maildir_prefix() .. "/sent-mail" ) ) then
   sent_mail( maildir_prefix() .. "/sent-mail" )
end



view_inline_attachments = false
