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
spoon.WindowHalfsAndThirds:bindHotkeys({
   left_half    = { {"ctrl",        "cmd"}, "Left" },
   right_half   = { {"ctrl",        "cmd"}, "Right" },
   top_half     = { {"ctrl",        "cmd"}, "Up" },
   bottom_half  = { {"ctrl",        "cmd"}, "Down" },
   third_left   = { {"ctrl", "alt"       }, "Left" },
   third_right  = { {"ctrl", "alt"       }, "Right" },
   third_up     = { {"ctrl", "alt"       }, "Up" },
   third_down   = { {"ctrl", "alt"       }, "Down" },
   top_left     = { {"ctrl",        "cmd"}, "1" },
   top_right    = { {"ctrl",        "cmd"}, "2" },
   bottom_left  = { {"ctrl",        "cmd"}, "3" },
   bottom_right = { {"ctrl",        "cmd"}, "4" },
   max_toggle   = { {"ctrl", "alt", "cmd"}, "f" },
   max          = { {"ctrl", "alt", "cmd"}, "Up" },
   -- undo         = { {        "alt", "cmd"}, "z" },
   -- center       = { {        "alt", "cmd"}, "c" },
   larger       = { {        "alt", "cmd", "shift"}, "Right" },
   smaller      = { {        "alt", "cmd", "shift"}, "Left" },
})
spoon.WindowScreenLeftAndRight:bindHotkeys(spoon.WindowScreenLeftAndRight.defaultHotkeys)
