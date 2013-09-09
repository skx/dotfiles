--
-- If a message is spam:
--
--   1.  Get the IP of the sending host.
--   2.  Create the file /etc/blacklist.d/$ip
--
function spam()
   mode = global_mode()
   mode = string.lower( mode );
   val  = ""

   if ( string.find( mode, "message" ) ) then
      val = header( "X-Remote-IP" )
   elseif (string.find(mode, "index" ) ) then
      val = header( "X-Remote-IP" )
   else
      msg( "spam() not present for mode:" .. mode );
   end

   if ( val == "" ) then
      msg( "IP address not found in message." )
      return
   end

   if ( val:match( "^(%d+.%d+.%d+.%d+)$" ) ) then
      file = "/etc/blacklist.d/" .. val

      if ( file_exists( file ) ) then
         msg( "IP already blacklisted: " .. val )
      else
         ff = io.open( file , "a");
         ff:write( val .. "\n" )
         ff:close();
         msg( "blacklisted IP: " .. val );
      end
   else
      msg( "Not an IPv4 address " .. val );
   end
end
