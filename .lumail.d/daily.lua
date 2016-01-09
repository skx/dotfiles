--
-- Mark all backups as read - if they succeeded.
--
function mark_read_in_folder( path, pattern )

   if ( not is_maildir( path ) ) then
      msg( "The specified path is not a maildir : " .. path )
      return
   end

   saved = save_state()

   -- Open the maildir and limit to new mails only.
   global_mode( "maildir" )
   clear_selected_folders()
   set_selected_folder( path )
   global_mode( "index" )
   index_limit( "new" )

   --
   -- Iterate over every message
   --
   max = count_messages()
   cur = 0
   handled = 0

   while( cur < max ) do

      jump_index_to( cur )

      -- get the header of the message.
      subject = header( "subject" )
      if ( string.find( subject, pattern, 1, true ) ) then
         mark_read()
         handled = handled + 1
      else
         msg( "Ignoring message " .. subject )
      end
      cur = cur + 1
   end

   if ( handled > 0 ) then
      msg( "Marked " .. handled .. " messages as read" )
   end

   load_state( saved )
end

function daily()
   mark_read_in_folder( os.getenv( "HOME" ) .. "/Maildir/Automated-backups-ok",
                        "backup2l: success" );

   mark_read_in_folder( os.getenv( "HOME" ) .. "/Maildir/machines-spotlight",
                        "/usr/local/cpanel/scripts/upcp --cron" )

   mark_read_in_folder( os.getenv( "HOME" ) .. "/Maildir/machines-spotlight",
                        "lfd on swd02.spotlightwebsites.com" )

   mark_read_in_folder( os.getenv( "HOME" ) .. "/Maildir/machines-spotlight",
                        "SSH login alert for" )

   mark_read_in_folder( os.getenv( "HOME" ) .. "/Maildir/machines-spotlight",
                        "/usr/sbin/csf" )

   mark_read_in_folder( os.getenv( "HOME" ) .. "/Maildir/machines-spotlight",
                        "swd02.spotlightwebsites.com: blocked" )


   mark_read_in_folder( os.getenv( "HOME" ) .. "/Maildir/machines-spotlight",
                        "/opt/dns/make-slave.sh" )
end
