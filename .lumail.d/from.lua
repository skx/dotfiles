

--
-- It is where you can run per-folder hooks.
--
-- Remember: zero or more folders might be selected at any given time.
--
function on_folder_selection( folder )

   --
   -- Change the email address we use depending on our folder.
   --
   if ( string.find( folder, "debian-administration", 1, true )  ) then
      from ("Steve <steve@debian-administration.org>");
   elseif ( string.find( folder, "debian" , 1, true )  ) then
      from("Steve <skx@debian.org>");
   elseif ( string.find( folder, "edinburgh-portraits.com" , 1, true )  ) then
      from("Steve <steve@edinburgh-portraits.com>");
   else
      from( default_email );
   end
end

