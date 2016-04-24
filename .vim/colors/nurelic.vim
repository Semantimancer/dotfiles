" Vim color scheme
" Name:        nurelic.vim
" Maintainer:  Ben Kugler <ben@bkugler.com>
" License:     public domain
"
" Built on nucolors.vim by Christian Brassat [1].
" Adapated from the Relic Dark colorscheme by stark [2].
"
" [1] <crshd@mail.com>
" [2] <stark@openmailbox.org>

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "nurelic"

hi Normal                    ctermfg=blue

hi Nontext                   ctermbg=none
hi Cursor                    ctermfg=lightred
hi CursorLine                ctermfg=lightred
hi LineNr                    ctermfg=grey
hi Search                    cterm=reverse
hi VertSplit                 ctermbg=none ctermfg=none
hi Visual                    ctermfg=black ctermbg=cyan
hi Folded                    ctermfg=grey ctermbg=none
hi FoldColumn                ctermfg=grey ctermbg=none

hi Directory                 cterm=bold ctermfg=blue
hi StatusLine                cterm=bold ctermfg=red ctermbg=darkblue
hi StatusLineNC              ctermfg=darkblue ctermbg=none
hi VertSplit                 ctermfg=darkblue ctermbg=none

hi Comment                   ctermfg=black
hi Todo                      cterm=bold ctermfg=red ctermbg=none

hi Error                     ctermfg=white ctermbg=none

hi Function                  ctermfg=white
hi Define                    cterm=bold 

hi Type                      cterm=bold ctermfg=blue
hi Constant                  ctermfg=red
hi Identifier                cterm=bold ctermfg=white
hi Include                   cterm=bold ctermfg=grey
hi Keyword                   ctermfg=white
hi Statement                 ctermfg=lightcyan
hi Number                    ctermfg=red
hi String                    ctermfg=lightcyan
hi Title                     ctermfg=cyan
