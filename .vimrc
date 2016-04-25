" Enables syntax highlighting
syntax enable

" Set to auto read when a file is changed from the outside
set autoread

" Set relative (smart) line-numbers
set nu
set rnu

" Make it look prettier
colorscheme nurelic

" Make it easier to use (especially for Haskell)
set tabstop=2 shiftwidth=2 expandtab
set nojoinspaces
set autoindent
set showcmd
set incsearch
set ignorecase
set smartcase
set nomodeline
set confirm

" This stops the warning message when an existing swap file is found
set shortmess+=A

" Linewrap makes handling certain files much easier
set wrap
set linebreak
set nolist  " list disables linebreak
set textwidth=0
set wrapmargin=0

" Visual movement works the way you would expect with linewrap
map j gj
map k gk

set pastetoggle=<F2>

set conceallevel=2
hi clear Conceal
hi Conceal ctermfg=110 guifg=#87afdf cterm=none gui=none
