function view_attachments()

      -- Get the attachments for this message, and the count.
      a     = attachments()
      count = count_attachments()

      if ( count < 1 ) then
         msg( "This message has no attachments!" )
         return
      end

      -- Choose the attachment number to view
      chosen = nil
      if count == 1 then
         chosen = 1
      else
         valid = ''
         num = 0
         while( num < count ) do
            num = num + 1
            valid = valid .. num
         end
         chosen = tonumber(prompt_chars("Which attachment? [" .. valid .. "] ", valid))
      end

      -- Get the name the user chose.
      name = a[chosen]

      -- Find the suffix.
      pattern =  '.*%.(.*)'
      x = name:gmatch(pattern)()

      -- Known extensions...
      application = nil

      -- Test...
      if x == 'odt' or x == 'ods' or x == 'doc' or x == 'docx' or x == 'xls' or x == 'xlsx' then
         application = '/usr/bin/libreoffice'
      elseif x == 'pdf' then
         application = '/usr/bin/evince'
      elseif x == 'gif' or x == 'jpeg' or x == 'jpg' or x == 'png' or x == 'tiff' or x == 'bmp' then
         application = '/usr/bin/display'
      elseif x == 'wav' or x == 'gsm' or x == 'mp3' then
         application = '/usr/bin/paplay'
      end

      -- Give up and ask
      if application == nil then
         application = prompt("What application should I use? ")
      end

      -- Generate a temporary filename to save the attachment to.
      dst = os.tmpname();

      -- Give the temporary filename the right suffix, because some
      -- programs are dumb.
      os.rename( dst, dst .. "." .. x )

      -- so now we'll work with the updated destination file.
      dst = dst .. "." .. x

      -- save the attachment.
      save_attachment( chosen, dst  );

      -- Now display it
      cmd = application .. " '" .. dst .. "'"
      os.execute(cmd)

      -- And clean up
      os.remove(dst)
end
