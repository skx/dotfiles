--
-- Load our dependencies
--
caffeinate = require "hs.caffeinate"
screen = require("hs.screen")


--
-- Lock the screen.
--
function lock_screen()
   caffeinate.lockScreen()
end

--
-- Center the current window - not yet used
--
function center_window()
   local win = hs.window.focusedWindow()
   win:centerOnScreen()
end


-- show details about the current window - not yet used.
function window_info()
   hs.alert.show(string.format("App path:        %s\nApp name:      %s\nIM source id:  %s",
                               hs.window.focusedWindow():application():path(),
                               hs.window.focusedWindow():application():name(),
                               hs.keycodes.currentSourceID()))
end


--
-- This moves windows around and should be easy to update.
--
function steve_layout()

   -- Count the number of screens.
   local numScreens = #screen.allScreens()

   -- If we don't have two screens available then we should just skip
   -- processing the windows.  Because we cannot setup their locations
   -- in the way we expect.
   if numScreens ~= 2 then
      return
   end

   -- The two monitors I have, identified by name but used in terms of logical location!
   left = "LEN P32p-20"
   right = "HP 732pk"

   -- Layouts
   local windowLayout = {
      {"Slack",   nil,   left, hs.layout.maximized, nil, nil},
      {"Firefox", nil,  right, hs.layout.maximized, nil, nil},
      {"Safari",  nil,  right, hs.layout.right50,   nil, nil},
      {"Emacs",   nil,  right, hs.layout.left50,    nil, nil},
   }

   hs.layout.apply(windowLayout)
end


--
-- When a new window is created/activated this runs.
--
function applicationWatcher(appName, eventType, appObject)

   if (eventType == hs.application.watcher.activated) then
      -- Bring all Finder windows forward when one gets activated
      if (appName == "Finder") then
         appObject:selectMenuItem({"Window", "Bring All to Front"})
      end
   end
   if (eventType == hs.application.watcher.launched) then
      -- Kill music
      if (appName == "Music") then
         appObject:kill()
      end
   end
end

--
-- Start the watcher for windows.
--
appWatcher = hs.application.watcher.new(applicationWatcher)
appWatcher:start()


--
-- We'll work out our location based on WiFi
--
local workSSID = "Metacore"
local lastSSID = hs.wifi.currentNetwork()

function ssidChangedCallback()
    newSSID = hs.wifi.currentNetwork()

    if newSSID == workSSID and lastSSID ~= workSSID then
       -- We just joined our work network
       print("We're at metacore")
       hs.audiodevice.defaultOutputDevice():setVolume(25)
       steve_layout()
    elseif newSSID ~= workSSID and lastSSID == workSSID then
        -- We just departed our work WiFi network
        hs.audiodevice.defaultOutputDevice():setVolume(0)
    end

    lastSSID = newSSID
end

--
-- Create a helper and start watching for WiFi changes
--
wifiWatcher = hs.wifi.watcher.new(ssidChangedCallback)
wifiWatcher:start()


--
-- Create a menu-bar to trigger layout reworking.
--
mb = hs.menubar.new()
mb:setClickCallback(steve_layout)
mb:setTitle("LAYOUT")

--
-- Create a second menu-bar to allow locking the screen.
--
lock = hs.menubar.new()
lock:setClickCallback(lock_screen)
lock:setTitle("🔒")


--
-- All is good
--
hs.alert.show("HammerSpoon Launched/Restarted")
