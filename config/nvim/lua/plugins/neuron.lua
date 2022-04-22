-------------------------------------------------------------------------------
-- neuron.nvim
-------------------------------------------------------------------------------

local neuron = require('neuron')

neuron.setup {
    virtual_titles = true,
    mappings = true,
    run = nil, -- function to run when in neuron dir
    neuron_dir = "~/code/wiki",
    leader = "<leader>n",
}
