local mash      = {"cmd", "alt", "ctrl"}

hs.hotkey.showHotkeys(mash, "/")
hs.hotkey.bind(mash, "d", function()
  for _, win in ipairs(hs.window.allWindows()) do
    if win then
      win:minimize()
    else
      print("Found a nil window for: "..win:application():name())
    end
  end
end)

-------------------------------------------------------------------------------
-- reload configuration
-------------------------------------------------------------------------------
hs.hotkey.bind(mash, "R",
               function()
                 hs.reload()
               end
)
hs.alert.show("Config loaded")

-------------------------------------------------------------------------------
-- Loading Spoons
-------------------------------------------------------------------------------
hs.fnutils.each(
  {
    { name = 'FnMate' },
    { name = 'MiroWindowsManager' }
  }, function(spoon) hs.loadSpoon(spoon.name) end
)

spoon.MiroWindowsManager:bindHotkeys({
  up         = { mash, "up"},
  right      = { mash, "right"},
  down       = { mash, "down"},
  left       = { mash, "left"},
  fullscreen = { mash, "f"}
})
