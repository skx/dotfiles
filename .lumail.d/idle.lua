--
-- The 'on_idle()' function is called approximately once a second,
-- when the client is idle.
--
-- Here we wrap that function up a little such that we can regularly
-- shell out to sync our mail from the remote location it is delivered.
--
-- Every second we also update the status-area to show a message.
--
function on_idle()
   m = global_mode()
   m = string.lower( m );

   str = ""

   -- Set the message we'll display
   if ( string.find( m, "maildir" ) ) then
      str = "mode:" .. m ..  " limit:" .. maildir_limit();
   else
      str = "mode:" .. m ;
   end

   -- Show the message & the time.
   msg( str .. " time:" .. os.date("%X" ) );

end

