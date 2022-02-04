-------------------------------------------------------------------------------
-- orgmode
------------------------------------------------------------------------------

local orgmode = require 'orgmode'

local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.org = {
  install_info = {
    url = 'https://github.com/milisims/tree-sitter-org',
    revision = 'f110024d539e676f25b72b7c80b0fd43c34264ef',
    files = {'src/parser.c', 'src/scanner.cc'},
  },
  filetype = 'org',
}

orgmode.setup {
  org_agenda_files = { "~/Documents/wiki/todo.org", "~/Documents/wiki/calendar.org", "~/Documents/wiki/backlog.org", "~/Documents/wiki/todo-history.org" },
  org_default_notes_file = '~/Documents/wiki/refile.org';
}
