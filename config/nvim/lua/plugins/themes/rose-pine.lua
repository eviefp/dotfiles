-------------------------------------------------------------------------------
-- theme
-------------------------------------------------------------------------------

-- set theme variant, matching terminal theme if unset
-- @usage 'main' | 'moon' | 'dawn'
vim.g.rose_pine_variant = 'moon'

vim.g.rose_pine_bold_vertical_split_line = true
vim.g.rose_pine_disable_background = true
vim.g.rose_pine_disable_float_background = true
vim.g.rose_pine_disable_italics = false
vim.g.rose_pine_inactive_background = false

local palette = require('rose-pine.palette')
vim.g.rose_pine_colors = {
	border = palette.highlight_med,
	comment = palette.pine,
	link = palette.love,
	punctuation = palette.gold,

	error = palette.love,
	hint = palette.iris,
	info = palette.foam,
	warn = palette.gold,

	git_add = palette.foam,
	git_change = palette.rose,
	git_delete = palette.love,
	git_dirty = palette.rose,
	git_ignore = palette.muted,
	git_merge = palette.iris,
	git_rename = palette.pine,
	git_stage = palette.iris,
	git_text = palette.rose,

	-- or set all headings to one colour: `headings = palette.text`
	headings = {
		h1 = palette.iris,
		h2 = palette.foam,
		h3 = palette.rose,
		h4 = palette.gold,
		h5 = palette.pine,
		h6 = palette.foam,
	},
}
