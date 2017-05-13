-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")

-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
local beautiful = require("beautiful")

-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal("debug::error", function (err)
    -- Make sure we don't go into an endless error loop
    if in_error then
      return
    end
    in_error = true

    naughty.notify({
      preset = naughty.config.presets.critical,
      title = "Oops, an error happened!",
      text = tostring(err)
    })

    in_error = false
  end)
end

beautiful.init(awful.util.get_themes_dir() .. "zenburn/theme.lua")

terminal = "xfce4-terminal"
editor = "gvim"
editor_cmd = terminal .. " -e " .. editor

menubar.utils.terminal = terminal -- Set the terminal for applications that require it

modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
  awful.layout.suit.tile,
  awful.layout.suit.tile.left,
  awful.layout.suit.tile.bottom,
  awful.layout.suit.tile.top,
  awful.layout.suit.fair,
  awful.layout.suit.fair.horizontal,
  awful.layout.suit.spiral,
  awful.layout.suit.spiral.dwindle,
  awful.layout.suit.max,
  awful.layout.suit.max.fullscreen,
  awful.layout.suit.magnifier,
  awful.layout.suit.corner.nw,
  awful.layout.suit.corner.ne,
  awful.layout.suit.corner.sw,
  awful.layout.suit.corner.se,
  awful.layout.suit.floating,
}


-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
  awful.button({ }, 1, function(t)
    t:view_only()
  end),

  awful.button({ modkey }, 1, function(t)
    if client.focus then
      client.focus:move_to_tag(t)
    end
  end),

  awful.button({ }, 3, awful.tag.viewtoggle),

  awful.button({ modkey }, 3, function(t)
    if client.focus then
      client.focus:toggle_tag(t)
    end
  end),

  awful.button({ }, 4, function(t)
    awful.tag.viewnext(t.screen)
  end),

  awful.button({ }, 5, function(t)
    awful.tag.viewprev(t.screen)
  end)
)

local tasklist_buttons = gears.table.join(
  awful.button({ }, 1, function (c)
    if c == client.focus then
      c.minimized = true
    else
      -- Without this, the following
      -- :isvisible() makes no sense
      c.minimized = false
      if not c:isvisible() and c.first_tag then
        c.first_tag:view_only()
      end
      -- This will also un-minimize
      -- the client, if needed
      client.focus = c
      c:raise()
    end
  end),

  awful.button({ }, 4, function ()
    awful.client.focus.byidx(1)
  end),

  awful.button({ }, 5, function ()
    awful.client.focus.byidx(-1)
  end)
)

local function set_wallpaper(screen)
  if beautiful.wallpaper then
    if type(beautiful.wallpaper) == "function" then
      gears.wallpaper.maximized(beautiful.wallpaper(screen), screen, true)
    else
      gears.wallpaper.maximized(beautiful.wallpaper, screen, true)
    end
  end
end
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
  -- Wallpaper
  set_wallpaper(s)

  -- Each screen has its own tag table.
  awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" }, s, awful.layout.layouts[1])

  -- Create a promptbox for each screen
  s.mypromptbox = awful.widget.prompt()

  -- Create an imagebox widget which will contains an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox(s)
  s.mylayoutbox:buttons(gears.table.join(
    awful.button({ }, 1, function () awful.layout.inc( 1) end),
    awful.button({ }, 3, function () awful.layout.inc(-1) end),
    awful.button({ }, 4, function () awful.layout.inc( 1) end),
    awful.button({ }, 5, function () awful.layout.inc(-1) end)
  ))

  -- Create a taglist widget
  s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

  -- Create a tasklist widget
  s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

  -- Create the wibox
  s.mywibox = awful.wibar({ position = "top", screen = s })

  -- Add widgets to the wibox
  s.mywibox:setup {
    layout = wibox.layout.align.horizontal,

    -- Left widgets
    {
      layout = wibox.layout.fixed.horizontal,

      awful.widget.launcher({
        image = beautiful.awesome_icon,
        menu = awful.menu({
          items = {
            {
              "awesome",
              {
                 { "hotkeys", function() return false, hotkeys_popup.show_help end},
                 { "manual", terminal .. " -e man awesome" },
                 { "edit config", editor_cmd .. " " .. awesome.conffile },
                 { "restart", awesome.restart },
                 { "quit", function() awesome.quit() end}
              },

              beautiful.awesome_icon
            },
            { "open terminal", terminal },
          },
        }),
      }),
      s.mytaglist,
      s.mypromptbox,
    },

    -- Middle widget
    s.mytasklist,

    -- Right widgets
    {
      layout = wibox.layout.fixed.horizontal,

      wibox.widget.systray(),
      wibox.widget.textclock(" %F %r ", 1),
      s.mylayoutbox,
    },
  }

  for _, tag in ipairs(s.tags) do
    tag.gap = 5
  end
end)

globalkeys = gears.table.join(
  awful.key({ modkey }, "Escape",
    awful.tag.history.restore,
    {
      group       = "tag",
      description = "go back",
    }
  ),

  awful.key({ modkey }, "Return",
    function ()
      awful.spawn(terminal)
    end,
    {
      group       = "launcher",
      description = "open a terminal",
    }
  ),

  awful.key({ modkey }, "F1",
    function ()
      awful.layout.inc( 1)
    end,
    {
      group       = "layout",
      description = "select next",
    }
  ),

  awful.key({ modkey, "Shift" }, "F1",
    function ()
      awful.layout.inc(-1)
    end,
    {
      group       = "layout",
      description = "select previous",
    }
  ),

  awful.key({ modkey }, "space",
    function()
      menubar.show()
    end,
    {
      group       = "launcher",
      description = "show the menubar",
    }
  ),

  awful.key({ modkey, }, "h",
    function ()
      awful.tag.incmwfact(-0.05)
    end,
    {
      group       = "layout",
      description = "decrease master width factor",
    }
  ),

  awful.key({ modkey, "Control" }, "h",
    function ()
      awful.tag.incncol( 1, nil, true)
    end,
    {
      group       = "layout",
      description = "increase the number of columns",
    }
  ),

  awful.key({ modkey, "Shift" }, "h",
    function ()
      awful.tag.incnmaster( 1, nil, true)
    end,
    {
      group       = "layout",
      description = "increase the number of master clients",
    }
  ),

  awful.key({ modkey }, "j",
    function ()
      awful.client.focus.byidx( 1)
    end,
    {
      group       = "client",
      description = "focus next by index",
    }
  ),

  awful.key({ modkey, "Control" }, "j",
    function ()
      awful.screen.focus_relative(1)
    end,
    {
      group       = "screen",
      description = "focus the next screen",
    }
  ),

  awful.key({ modkey, "Shift" }, "j",
    function ()
      awful.client.swap.byidx(1)
    end,
    {
      group       = "client",
      description = "swap with next client by index",
    }
  ),

  awful.key({ modkey }, "k",
    function ()
      awful.client.focus.byidx(-1)
    end,
    {
      description = "focus previous by index",
      group       = "client",
    }
  ),

  awful.key({ modkey, "Control" }, "k",
    function ()
      awful.screen.focus_relative(-1)
    end,
    {
      group       = "screen",
      description = "focus the previous screen",
    }
  ),

  awful.key({ modkey, "Shift" }, "k",
    function ()
      awful.client.swap.byidx(-1)
    end,
    {
      group       = "client",
      description = "swap with previous client by index",
    }
  ),
  awful.key({ modkey }, "l",
    function ()
      awful.tag.incmwfact( 0.05)
    end,
    {
      group       = "layout",
      description = "increase master width factor",
    }
  ),

  awful.key({ modkey, "Control" }, "l",
    function ()
      awful.tag.incncol(-1, nil, true)
    end,
    {
      group       = "layout",
      description = "decrease the number of columns",
    }
  ),

  awful.key({ modkey, "Shift" }, "l",
    function ()
      awful.tag.incnmaster(-1, nil, true)
    end,
    {
      group       = "layout",
      description = "decrease the number of master clients",
    }
  ),

  awful.key({ modkey }, "u",
    awful.client.urgent.jumpto,
    {
      group       = "client",
      description = "jump to urgent client",
    }
  )
)

clientkeys = gears.table.join(
  awful.key({ modkey }, "w",
    function (c)
      c:kill()
    end,
    {
      group       = "client",
      description = "close",
    }
  )
)

for i = 1, 10 do
  local key_number
  if i == 10 then
    key_number = "0"
  else
    key_number = ""..i
  end

  globalkeys = gears.table.join(globalkeys,
    awful.key({ modkey }, key_number,
      function ()
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
        tag:view_only()
      end,
      {
        group       = "tag",
        description = "view tag #"..key_number,
      }
    ),

    awful.key({ modkey, "Control" }, key_number,
      function ()
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
        awful.tag.viewtoggle(tag)
      end,
      {
        group       = "tag",
        description = "toggle tag #" .. key_number,
      }
    ),

    awful.key({ modkey, "Shift" }, key_number,
      function ()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then
            client.focus:move_to_tag(tag)
          end
        end
      end,
      {
        group       = "tag",
        description = "move focused client to tag #"..key_number,
      }
    ),

    awful.key({ modkey, "Control", "Shift" }, key_number,
      function ()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then
            client.focus:toggle_tag(tag)
          end
        end
      end,
      {
        group       = "tag",
        description = "toggle focused client on tag #" .. key_number,
      }
    )
  )
end

root.keys(globalkeys)

awful.rules.rules = {
  {
    rule = {},
    properties = {
      border_width      = beautiful.border_width,
      border_color      = beautiful.border_normal,
      focus             = awful.client.focus.filter,
      keys              = clientkeys,
      placement         = awful.placement.no_overlap+awful.placement.no_offscreen,
      raise             = true,
      screen            = awful.screen.preferred,
      titlebars_enabled = true,
   },
  },
}

-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
  -- Set the windows at the slave,
  -- i.e. put it at the end of others instead of setting it master.
  -- if not awesome.startup then awful.client.setslave(c) end

  if true
    and awesome.startup
    and not c.size_hints.user_position
    and not c.size_hints.program_position
  then
    -- Prevent clients from being unreachable after screen count changes.
    awful.placement.no_offscreen(c)
  end
end)

client.connect_signal("request::titlebars", function(c)
  local buttons = gears.table.join(
    awful.button({ }, 1, function()
      client.focus = c
      c:raise()
      awful.mouse.client.move(c)
    end),
    awful.button({ }, 3, function()
      client.focus = c
      c:raise()
      awful.mouse.client.resize(c)
    end)
  )

  awful.titlebar(c) : setup {
    layout = wibox.layout.align.horizontal,

    -- Left
    {
      buttons = buttons,
      layout  = wibox.layout.fixed.horizontal,

      awful.titlebar.widget.iconwidget(c),
    },

    -- Middle
    {
      buttons = buttons,
      layout  = wibox.layout.flex.horizontal,

      {
        align  = "center",
        widget = awful.titlebar.widget.titlewidget(c),
      },
    },

    -- Right
    {
      layout = wibox.layout.fixed.horizontal(),

      awful.titlebar.widget.floatingbutton(c),
      awful.titlebar.widget.minimizebutton(c),
      awful.titlebar.widget.maximizedbutton(c),
      awful.titlebar.widget.closebutton(c),
    },
  }
end)

client.connect_signal("mouse::enter", function(c)
  if true
    and awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
    and awful.client.focus.filter(c)
  then
    client.focus = c
  end
end)

client.connect_signal("focus", function(c)
  c.border_color = beautiful.border_focus
end)

client.connect_signal("unfocus", function(c)
  c.border_color = beautiful.border_normal
end)

-- for later ref
-- http://www.holgerschurig.de/en/awesome-4.0-titlebars/
