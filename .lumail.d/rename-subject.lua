
--
-- If the function "on_reply_transform_subject()" is defined it will
-- be invoked when a message is replied to.
--
-- The intention is that you return an updated subject.
--
function on_reply_transform_subject( sub )

   if ( string.len( sub ) < 1 ) then
      return "No subject"
   end
   --
   -- Remove (repeated) "Re:" from the start of string.
   --
   while( string.find(sub, "^[rR][eE]:" ) ) do
      sub = string.gsub( sub, "^[rR][eE]:[ \t]+", "" )
   end

   --
   -- Remove 'UNS:' too, what I add if a mail is "unsure" of spam
   -- status.
   --
   while( string.find(sub, "^[uU][nN][sS]:" ) ) do
      sub = string.gsub( sub, "^[uU][nN][sS]:[ \t]+", "" )
   end

   -- Add prefix
   return "Re: " .. sub
end

