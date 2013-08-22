--
-- This function is called at run-time when TAB-completion is invoked.
--
-- The function is expected to return a Lua table, the values of which
-- will be used for completion.  (The keys being ignored.)
--
-- Here we add any user-defined function the to the completion set.
-- This works because Lua uses the global "_G" table to store functions, etc.
--
--
function on_complete()

   --
   -- Add the colours we support for the various display functions.
   --
   ret = { "blue", "cyan", "green", "magenta", "red", "white", "yellow" }

   for k,v in pairs(_G) do
      ret[k] = k
   end

   --
   -- Add ~/.address-book if it exists
   --
   abook =  os.getenv( "HOME" ) .. "/.address-book"
   if ( file_exists( abook ) ) then
      file = assert(io.open( abook, "r"))
      for line in file:lines() do
         ret[line]= line
      end
   end

   return(ret)
end
