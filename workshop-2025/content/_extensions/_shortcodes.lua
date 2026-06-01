function Shortcode (elem)
  if elem.name == "colored" then
    local text = pandoc.utils.stringify(elem.args[1])
    local color = pandoc.utils.stringify(elem.args[2]) or "black"
    return pandoc.RawInline('html', '<span style="color:' .. color .. '">' .. text .. '</span>')
  end
end