--
-- Save the important parts of the client such that they can be restored.
--
function save_state()
   table = {}

   table['gm'] = global_mode()

   table['il'] = index_limit()
   table['io'] = index_offset()

   table['ml'] = maildir_limit()
   table['mo'] = maildir_offset()

   table['mmo' ] = message_offset();

   table['selected'] = selected_folders();

   return table
end


--
-- Restore a previously saved state.
--
function load_state( table )

   -- ensure we have no selected folders.
   clear_selected_folders();

   -- restore the selected folders
   selected = table['selected']
   for i,v in ipairs( selected ) do
      add_selected_folder(v)
   end

   global_mode( table['gm'] );

   index_limit( table['il'] );
   jump_index_to( table['io'] );

   maildir_limit( table['ml'] );
   jump_maildir_to( table['mo'] );

   jump_message_to(table['mmo' ])

end