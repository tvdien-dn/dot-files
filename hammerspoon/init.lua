-- CONSTANT

hyper = {"cmd", "alt", "ctrl", "shift"}

-- CONFIG RELOADING
hs.alert.show("Config Loaded")
-- hs.alert.show(hs.hotkey.getHotkeys())
-- hs.hotkey.showHotkeys()
hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration:start()
-- hs.hotkey.bind({"cmd", "alt", "ctrl"}, "R", function()
--   hs.reload()
-- end)

hs.loadSpoon("WindowScreenLeftAndRight")
hs.loadSpoon("WindowHalfsAndThirds")
spoon.WindowHalfsAndThirds:bindHotkeys(spoon.WindowHalfsAndThirds.defaultHotkeys)
spoon.WindowScreenLeftAndRight:bindHotkeys(spoon.WindowScreenLeftAndRight.defaultHotkeys)
