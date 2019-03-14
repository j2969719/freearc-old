-- This file uses UTF8 encoding

-- Maximum allowed length of command line
MAX_CMD_LENGTH = 4000


-- User-defined function that returns Right-Click Menu items for given filename(s)
--   filenames: array of UTF8-encoded filenames selected in Explorer
--   return value: array of menu items, each item is structure with the following fields:
--                   for commands: text, command, help
--                   for submenus: text, submenu, help (where submenu is, recursively, array of menu items)
register_menu_handler (function (filenames)
  nameext  = drop_dir(filenames[1])
  path     = get_dir(filenames[1])
  basename = drop_ext(nameext)
  ext      = string.lower(get_ext(nameext))

  -- Menu items for Compresion operations - the only menu items for non-archive files
  if #filenames==1 then
    isdir       = dir_exists(filenames[1])
    arcbase     = isdir and nameext or basename
    subdir      = basename..DIR_SEPARATOR
    add_options = ""   -- "-ep1": disabled due to bug in FreeArc
    filename    = quote(nameext)
  else
    arcbase     = drop_dir(path) or "default"
    subdir      = "*"..DIR_SEPARATOR
    add_options = ""
    filename    = ""
    for _,f in ipairs(filenames) do
      filename = filename.." "..quote(drop_dir(f))
    end
  end
  if string.len(filename) > MAX_CMD_LENGTH   then filelist = "@{listfile}"  else filelist = filename end
  arcname = arcbase.."."..freearc_ext
  sfxname = arcbase.."."..freearc_sfx_ext

  -- Check that all files selected are archives, SFX-es or non-FreeArc archives
  all_arcs     = true    -- all selected files are .arc
  all_sfxes    = true    -- all selected files are FreeArc SFXes
  all_freearcs = true    -- all selected files are FreeArc archives
  all_zips     = true    -- all selected files are non-FreeArc archives
  all_archives = true    -- all selected files are archives
  for _,f in ipairs(filenames) do
    nameext  = drop_dir(f)
    ext      = string.lower(get_ext(nameext))
    is_arc = string.find (' '..freearc_ext    ..' ', ' '..ext..' ', 1, true)
    is_sfx = string.find (' '..freearc_sfx_ext..' ', ' '..ext..' ', 1, true)  and  check_for_sfx(f)
    is_zip = string.find (' '..archives_ext   ..' ', ' '..ext..' ', 1, true)
    all_arcs     = all_arcs     and is_arc
    all_sfxes    = all_sfxes    and is_sfx
    all_freearcs = all_freearcs and (is_arc or is_sfx)
    all_zips     = all_zips     and is_zip
    all_archives = all_archives and (is_arc or is_sfx or is_zip)
    if not all_archives then break end
  end


  -- Compression commands
  compression_menu = {
          append (command.add2arc,  {param = arcname,  command = freearc.." a --noarcext "     ..add_options.." -sclUTF-8 -- \"" .. arcname .. "\" " .. filelist}),
          append (command.add2sfx,  {param = sfxname,  command = freearc.." a -sfx --noarcext "..add_options.." -sclUTF-8 -- \"" .. arcname .. "\" " .. filelist}),
          append (command.add,      {                  command = freearc.." --add-dialog a "   ..add_options.." -- "..filename}),
  }

  -- Extraction commands
  extraction_menu = {
    #filenames==1   and append (command.open,         {command = freearc.." "..filename}),
                        append (command.extractTo,    {command = multi_command (freearc, " x -ad --noarcext -- ", filenames),  param = subdir}),
                        append (command.extractHere,  {command = multi_command (freearc, " x --noarcext -- ", filenames)}),
                        append (command.extract,      {command = freearc.." --extract-dialog x -- "..filename}),
                        append (command.test,         {command = multi_command (freearc, " t --noarcext -- ", filenames)}),
  }

  -- Modification commands
  modification_menu = {
      all_arcs      and append (command.arc2sfx,      {command = multi_command (freearc, " s --noarcext -- ",  filenames)}),
      all_sfxes     and append (command.sfx2arc,      {command = multi_command (freearc, " s- --noarcext -- ", filenames)}),
                        append (command.modify,       {command = freearc.." --add-dialog ch -- "..filename}),
      #filenames>1  and append (command.join,         {command = freearc.." --add-dialog j -- "..filename}),
  }

  -- Archive conversion commands
  cvt_menu = {
                        append (command.zip2arc,      {command = all2arc..                 " -- "..filename}),
                        append (command.zip2sfx,      {command = all2arc..            " -sfx -- "..filename}),
                        append (command.zip2a,        {command = freearc.." --add-dialog cvt -- "..filename}),
  }

  -- Provide various menus depending on types of files (archives) selected
  if all_freearcs then
    arc_menu = concat(extraction_menu,modification_menu)
    if #filenames>1   then menu = concat(arc_menu,compression_menu)   else menu = arc_menu end
  elseif all_zips then
    menu = concat(extraction_menu, concat(cvt_menu,compression_menu))
  elseif all_archives then
    menu = concat(extraction_menu, compression_menu)
  else
    menu = compression_menu
  end

  if cascaded then
    menu = { {text = FreeArcName,  submenu = menu,  help = FreeArcName.." commands"} }
  end

  return menu
end)


--os.execute ("start "..arg)
