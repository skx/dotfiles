--
-- Show only maildirs which have received mail today
-- by default.
--
Config:set( "maildir.limit", "today" )

--
-- Show only mail received today, by default
--
Config:set( "index.limit", "today" )


--
-- Setup our editor
--
Config:set( "global.editor", "vim  +/^$ ++1 '+set tw=72'" )

--
-- Setup our default From: address.
--
local def_from = "Steve Kemp <steve@steve.org.uk>"
Config:set( "global.sender", def_from )

--
-- Setup folder-based addresses.
--
-- NOTE: `%` is used to escape special characters in Lua's regular
-- expression library.  As you can imagine this is horrid, and precisely
-- why we ported PCRE ;)
--
folder_from = {
   ['.*'] = def_from,
   ['debian%-administration%.'] = "Steve Kemp <steve@debian-administration.org>",
   ['dns%-api.com'] = "Steve Kemp <steve@dns-api.com>",
   ['dhcp.io'] = "Steve Kemp <steve@dhcp.io>",
}


--
-- Unread messages/maildirs are drawn in red.
--
Config:set( "colour.unread", "red" )
