-------------------------------------------------------------------------------
-- nerveux.nvim
-------------------------------------------------------------------------------

local nerveux = require 'nerveux';

nerveux.setup {
    --- path to neuron executable (default: neuron in PATH)
    neuron_cmd = "neuron",

    --- no trailing slash, (default: current directory)
    neuron_dir = "/home/evie/code/wiki",

    --- Use the cache, significantly faster (default: false)
    use_cache = true,

    --- start the neuron daemon to keep the cache up to date (default: false)
    start_daemon = true,

    --- show zettel titles inline as virtual text (default: false)
    virtual_titles = false,

    --- Automatically create mappings (default: false)
    create_default_mappings = true,

    --- The Highlight Group used for the inline zettel titles (default: Special)
    virtual_title_hl = "Special",
    virtual_title_hl_folge = "Repeat",

    --- `kill -9` the pid of the daemon at exit (VimPreLeave), only valid is
    -- start_daemon is true (default: false)
    kill_daemon_at_exit = true,

    --- You can overwrite this table partially
    -- and your settings will get merged with the defaults
    -- You can also disable a single mapping by settings it's value to an empty string.
    mappings = {

       -- Search all your zettels
       -- * then `<CR>` to edit
       -- * or `<Tab>` to insert the selected zettel ID under your cursor
       search_zettels = "gzz" ,

       -- Search the backlinks to the current zettel
       backlinks_search = "gzb" ,

       -- Search the only the uplinks to the current zettel
       uplinks_search = "gzu" ,

       -- Create a new zettel via neuron and :edit it
       new = "gzn" ,

       -- Search for content inside all the zettels
       search_content = "gzs" ,

       -- Insert the ID of the previously visited zettel
       insert_link = "gzl" ,

       -- Insert the ID of the previously visited zettel, but as a folgezettel
       insert_link_folge = "gzL" ,

       -- Open the zettel ID that's under the cursor
       follow = "<CR>" ,

       -- Show the help
       help = "gz?" ,
    }
}
