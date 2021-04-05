local wezterm = require 'wezterm';

function font_with_fallback(name, params)
  local names = {name, "Iosevka"}
  return wezterm.font_with_fallback(names, params)
end

return {
  font_size = 10.5,
  line_height = 1.0,
  color_scheme = "nord",

  -- fonts
  font = font_with_fallback("Iosevka Fixed Medium"),
  font_rules = {
    {
      intensity = "Bold",
      font = font_with_fallback("Iosevka Fixed Semibold"),
    },
  },
  font_antialias = "Subpixel",
  font_hinting = "Full",
}
