set termguicolors
" Plugin Packages
call plug#begin(stdpath('config') . '/plugged')

"" Plugins
""" Diffview
Plug 'nvim-lua/plenary.nvim'
Plug 'sindrets/diffview.nvim'

""" Which-Key
Plug 'folke/which-key.nvim'

""" Statusline
Plug 'feline-nvim/feline.nvim', { 'branch': 'develop' }

""" Icons
Plug 'kyazdani42/nvim-web-devicons'

""" Git
Plug 'lewis6991/gitsigns.nvim'

"" Themes
Plug 'folke/tokyonight.nvim', { 'branch': 'main' }

" Initialize plugin system
call plug#end() 

" Config
"" Feline
lua require('feline').setup()
