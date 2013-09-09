
--
-- If the function "on_reply_transform_subject()" is defined it will
-- be invoked when a message is replied to.
--
-- The intention is that you return an updated subject.
--
function on_reply_transform_subject( subject )

   if ( string.len( subject ) < 1 ) then
      return "No subject"
   end
   --
   -- Remove (repeated) "Re:" & "UNS:" from the start of string.
   --
   -- Sadly we cannot use "(Re|UNS):" because Lua doesn't support
   -- that without an external regexp library.
   --
   while( string.find(subject, "^[rR][eE]:" ) or
          string.find(subject, "^[uU][nN][sS]:" ) ) do

      subject = string.gsub( subject, "^[uU][nN][sS]:[ \t]+", "" )
      subject = string.gsub( subject, "^[rR][eE]:[ \t]+", "" )
   end
   --
   -- Add the standard prefix
   --
   return "Re: " .. subject
end

