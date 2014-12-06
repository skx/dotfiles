--
-- Configuration for KPie
--

if ( DEBUG ) then
   print( "Title " .. window_title() )
   print( "\tApplication: " .. window_application() )
   print( "\tClass " .. window_class() )
end


--
-- The music-player should be on all windows
--
if ( window_class() == "sonata" ) then
   pin()
   minimize()
end


--
-- Media players deserve to be always-on-top
--
-- NOTE: Xine will update its title/application/class to include the
--       name of the media that is being played.
--
--
if ( ( string.sub(window_application(), 0, 5) == "xine:" ) or
     ( window_application() == "MPlayer" ) or
     ( window_application() == "Totem Movie Player" ) ) then
   above()
end


--
--  Web-browsers.
--
--  I use three browsers, constantly:
--
--  * iceweasel in the first desktop
--
--  * epiphany in the third destkop  (gaim is in desktop2)
--     * Match on both "class" and "application" to be sure we find it.
--
--  * chromium in the fourth destkop
--     * Look for a suffix of ' - Chromium' in the title to find it.
--

--
--  Chromium
--
if ( string.match( window_application(), " - Chromium$" ) ) then
   workspace(4)
   if ( not is_maximized() ) then maximize() end
end


--
-- Epiphany
--
if ( ( window_application() == "Web" )  and
     ( window_class() == "Web" ) ) then
   workspace(3)
   if ( not is_maximized() ) then maximize() end
end


--
--  Iceweasel
--
if ( window_application() == "Iceweasel" ) then
   workspace(1)
   if ( not is_maximized() ) then maximize() end
end



--
-- This is configuration for Pidgin.
--
-- The rough layout is a pair of windows looking like this, on workspace 2:
--
--     ---------------------------------------
--     | Chat Window           |  Buddy List |
--     |                       |             |
--     ---------------------------------------
--
-- The values are calculated by looking at the screen-size, with the
-- buddy-list occupying 1/5th of the screen estate.
--
--

if ( window_class() == "Pidgin" ) then

   -- The width/height of the screen.
   width  = screen_width()
   height = screen_height()

   -- The buddy window will be 1/5 of the screen width.
   buddy = width / 5;

   --
   -- Move the buddy-window to the correct location
   --
   if ( window_title() == "Buddy List" ) then

      if ( is_maximized() ) then unmaximize() end
      if ( is_minimized() ) then unminimize() end
      if ( is_fullscreen() ) then unfullscreen() end

      -- x,y
      xy(width-buddy,0 )

      -- width,height
      size(buddy,height )
   else

      if ( is_maximized() ) then unmaximize() end
      if ( is_minimized() ) then unminimize() end
      if ( is_fullscreen() ) then unfullscreen() end

      -- x,y
      xy(0,0)

      -- width,height
      size(width-buddy,height )
   end

   -- Both windows on workspace two.
   workspace(2)
end



if ( window_application() == "Terminal" ) then
   maximize_vertically()
end
