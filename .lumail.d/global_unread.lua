--
-- Show all unread messages, regardless of the containing folder.
--
function global_unread()

   -- set the mode appropriately.
   maildir_limit( "new" );
   global_mode( "maildir" );

   -- ensure we have no selected folders.
   clear_selected_folders();

   offset = 0;

   -- Now for each one we'll add it to the selected set.
   while( offset <= (count_maildirs() - 1) ) do

      -- jump to folder.
      jump_maildir_to( offset )

      -- Added the folder to the selected set.
      add_selected_folder()

      -- move to the next.
      offset = offset + 1;
   end

   --
   -- Now we've added all maildirs with new mail to the list
   -- of selected maildirs.
   --
   -- Jump to the global index-mode, and view only new mails.
   --
   --
   global_mode( "index" );
   index_limit( "new" );
end
