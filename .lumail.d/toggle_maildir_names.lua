--
-- Jump between showing the full-path to all Maildirs
-- and just the short-version.
--
function toggle_maildir_names()

   -- get the current format-string.
   fmt = maildir_format()

   -- if it contains the short name
   if ( fmt:find( "$NAME", 1, true ) ) then
      -- replace it with the complete path.
      fmt = fmt:gsub( "$NAME", "$PATH" )
   else
      -- otherwise if it contains the path
      if ( fmt:find( "$PATH", 1, true ) ) then
         -- replace with the short-name.
         fmt = fmt:gsub( "$PATH", "$NAME" )
      end
   end

   --
   -- Update the format.
   --
   maildir_format( fmt );
end

--
-- Bind to a key.
--
keymap['global']['p'] = "toggle_maildir_names()"
