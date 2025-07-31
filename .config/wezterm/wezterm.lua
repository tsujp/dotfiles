-- Wezterm configuration is a Lua script expected to return a configuration table.

local wezterm = require 'wezterm'
local act = wezterm.action
local config = {}

-- List of pre-bundled themes: https://wezfurlong.org/wezterm/colorschemes/index.html
-- config.color_scheme = 'Ayu Dark (Gogh)'
-- config.color_scheme = 'Sonokai (Gogh)'
config.color_scheme = 'Modus-Vivendi'

-- Wezterm ships JetBrains Mono, Nerd Font Symbols, and Noto Color Emoji by default.
-- When resolving text into glyphs Wezterm will consult the first font and if said
--   glyph is not found it'll try the next font in the list and so on. This also
--   means no font-patching is required for Nerd Fonts since Wezterm ships it by
--   default and when using fallback it'll... fallback to Nerd Font for symbols.

-- If on laptop screen.
-- config.font_size = 17

-- If on 2560x1440 LG monitor.
config.font_size = 16
config.freetype_load_flags = 'NO_HINTING'
config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }
-- config.freetype_load_target = 'Light'
config.max_fps = 144

config.font = wezterm.font_with_fallback {
  {
    family = 'tsujp Medium',
    -- family = 'Zed Mono Medium',
  },
  'JetBrains Mono',
  'Noto Color Emoji',
}

config.font_rules = {
  {
    intensity = 'Bold',
    italic = false,
    font = wezterm.font {
      -- family = 'Zed Mono Extrabold Extended',
      family = 'tsujp Extrabold',
    },
  },
  {
    intensity = 'Normal',
    italic = true,
    font = wezterm.font {
      -- family = 'Zed Mono Medium Extended',
      family = 'tsujp Medium',
      italic = true,
    },
  },
  {
    intensity = 'Bold',
    italic = true,
    font = wezterm.font {
      -- family = 'Zed Mono Extrabold Extended',
      family = 'tsujp Extrabold',
      italic = true,
    },
  },
}

-- config.window_padding = {
--   -- left = 0,
--   left = '5cell',
--   -- right = 0,
--   right = 100,
--   top = 20,
--   bottom = 0,
-- }

config.inactive_pane_hsb = {
  saturation = 1, -- 1.1
  brightness = 0.65, -- 0.85
}

config.tab_bar_at_bottom = true
config.use_resize_increments = true
config.hide_mouse_cursor_when_typing = true

-- TODO: Separate other line drawing content by making a PR, if I want thick undercurls
--       so they are easily noticeable it doesn't mean I want a thick split line
--       or a thick dashed line (e.g. chars "┆", "┊", "⸽"). Currently all these
--       inherit from "UNDERLINE_thickness" with the sole escape-hatch present for
--       the cursor alone.
-- Thick and noticeable underlines and undercurls.
config.underline_thickness = 1 -- or 3.
config.underline_position = -2

-- Cursor shape.
config.default_cursor_style = 'SteadyBlock'
config.cursor_thickness = 2

config.colors = {
  -- (active) cell background colour where cursor is iff cursor is block.
  cursor_bg = '#FFFF00', -- Electric yellow.
  -- (inactive) cursor border colour iff block, else cursor {under}line colour.
  cursor_border = '#FFFF00', -- Electric yellow.
  -- Text colour where cursor is.
  cursor_fg = '#000000', -- Black.

  -- Cursor colour when IME, dead key, or leader key pressed and waiting on key
  --   chord to complete.
  compose_cursor = '#FF0000', -- Red? (TODO)

-- TODO: Wezterm PR for the split line not drawing correctly, e.g. if terminal (UI) height
--       is a fractional amount of the height of a terminal cell the split line either
--       draws too far (into the tab line) or too near; we know the dimensions of the
--       terminal area since we're the ones rendering it so I think the line could
--       be rendered correctly here. This also goes for the background appearing
--       with non-stepped resizes; add an option to repeat the terminal background
--       to the edge to remove that.
  split = '#666666',
}

config.use_fancy_tab_bar = false

config.window_padding = {
  left = 5, -- 10
  right = 5, -- 3
  top = 5,
  bottom = 0,
}

-- config.window_decorations = 'RESIZE'

config.window_frame = {
  border_left_width = 5,
  border_right_width = 0,
  border_bottom_height = 0,
  border_top_height = 0,
  -- border_top_color = 'cyan',
  -- titlebar_bg = 'red',
  -- inactive_titlebar_border_bottom = 'cyan',
  -- active_titlebar_border_bottom = 'cyan',
}

config.tab_max_width = 48
config.show_new_tab_button_in_tab_bar = false

wezterm.on('update-status', function(window, pane)
  -- Un-processed text elements.
  local cells = {}
  
  -- TODO: Don't need CWD really, only hostname.
  -- local cwd_uri = pane:get_current_working_dir()
  -- if cwd_uri then
  --   cwd_uri = cwd_uri:sub(8)
  --   local slash = cwd_uri:find '/'
  --   local cwd = ''
  --   local hostname = ''
  --   if slash then
  --     hostname = cwd_uri:sub(1, slash - 1)
  --     -- Remove the domain name portion of the hostname
  --     local dot = hostname:find '[.]'
  --     if dot then
  --       hostname = hostname:sub(1, dot - 1)
  --     end
  --     -- and extract the cwd from the uri
  --     cwd = cwd_uri:sub(slash)

  --     table.insert(cells, cwd)
  --     table.insert(cells, hostname)
  --   end
  -- end

  local date = wezterm.strftime '%H:%M %a %-d %b'
  table.insert(cells, date)

  -- Battery %.
  for _, b in ipairs(wezterm.battery_info()) do
    table.insert(cells, string.format('%.0f', b.state_of_charge * 100))
  end

  -- Final to-be-rendered elements.
  local tab_bar_elements = {}

  -- Convert un-processed text into tab bar elements.
  function push(text)
    -- A smidge of padding.
    table.insert(tab_bar_elements, { Text = '  ' .. text .. '  ' })
  end

  while #cells > 0 do
    local cell = table.remove(cells, 1)
    push(cell)
  end

  window:set_right_status(wezterm.format(tab_bar_elements))

  -- TODO: On left do hostname and if this pane is zoomed red text that says "ZOOMED"
  -- window:set_left_status(wezterm.format { { Text = '' }})
end)

config.colors.tab_bar = {
  background = '#313244',

  active_tab = {
    bg_color = '#1E1E2E',
    fg_color = '#FFF',
    intensity = 'Bold',
  },

  inactive_tab = {
    bg_color = '#45475A',
    fg_color = '#BAC2DE',
  },

  inactive_tab_hover = {
    bg_color = '#575B70',
    fg_color = '#FFF',
    italic = true
  },

  new_tab = {
    bg_color = '#313244',
    fg_color = '#BAC2DE',
  },

  new_tab_hover = {
    bg_color = '#575B70',
    fg_color = '#FFF',
  },
}

-- Pane selection
-- https://wezfurlong.org/wezterm/config/lua/keyassignment/PaneSelect.html
config.keys = {
  { key = '0', mods = 'CMD', action = act.PaneSelect },
  {
    key = '8',
    mods = 'CMD',
    action = act.PaneSelect {
      mode = 'SwapWithActive',
    },
  },
}

return config
