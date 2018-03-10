" Import plugins {{{
call plug#begin("~/.vim/plugged")
Plug 'EESchneider/vim-rebase-mode'
Plug 'KeyboardFire/vim-minisnip'
Plug 'airblade/vim-gitgutter'
Plug 'ap/vim-buftabline'
Plug 'easymotion/vim-easymotion'
Plug 'godlygeek/tabular'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'kana/vim-arpeggio'
Plug 'lervag/vimtex', { 'for': 'latex' }
Plug 'machakann/vim-sandwich'
Plug 'majutsushi/tagbar'
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
Plug 'romainl/vim-qf'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-vinegar'
Plug 'w0rp/ale'
Plug 'wellle/targets.vim'

" pretty colors
Plug 'NLKNguyen/papercolor-theme'
Plug 'rakr/vim-two-firewatch'
Plug 'nanotech/jellybeans.vim'
Plug 'sonph/onehalf', { 'rtp': 'vim/' }
call plug#end()
" }}}

" Pretty things up! {{{
function! DarkTheme()
    set bg=dark
    colorscheme jellybeans
endfunction
function! LightTheme()
    set bg=light
    colorscheme onehalflight
endfunction
if strftime("%H") >= 7 && strftime("%H") < 19
    call LightTheme()  " daytime theme
else
    call DarkTheme()  " nighttime theme
endif

" for changing cursor shape in st
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"

" highlight BUG, DEBUG just like TODO
syntax match Todo "\<\(TODO\|BUG\|DEBUG\)\>"

" statusbar
augroup cherryline
    autocmd BufEnter * source ~/.vim/cherryline.vim
augroup END
source ~/.vim/cherryline.vim  " make it so re-sourcing this .vimrc doesn't hide the bar
" }}}

" General tweaks {{{
" misc options {{{
filetype plugin indent on
set wildmenu
set incsearch
set nohlsearch
set infercase
set expandtab
set shiftwidth=4
set path+=**
set mouse=a
set scrolloff=3
set splitright
set splitbelow
set noswapfile
set number
set termguicolors
set diffopt+=vertical
set tags=./.tags,./tags,tags
set hidden
set noshowmode

let g:netrw_liststyle=3
let g:netrw_winsize=25
let g:netrw_browse_alternative=4
let g:netrw_altv=1
" }}}

" convenience keybindings {{{
map <Space> <Nop>
let mapleader="\<Space>"
nnoremap <Leader>w :w<CR>
nnoremap <silent> <Leader>q :call Quitbuf()<CR>

function! Quitbuf()
    if len(getbufinfo({'buflisted':1})) > 1
        execute "bd"
    else
        execute "q"
    endif
endfunction

nnoremap 9 ^
vnoremap 9 ^

" single-line scroll keys I can remember
nnoremap 8 <C-y>
nnoremap 7 <C-e>

" buffer nvaigation
nnoremap <silent> K :bp<CR>
nnoremap <silent> J :bn<CR>
nnoremap <silent> M J

" window navigation
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

vnoremap > >gv
vnoremap < <gv

if has('nvim')
    tnoremap <Esc> <C-\><C-n>
endif

noremap ; :
noremap : ;
noremap q; q:

" indentation jumping
noremap <M-k> :call search('^'. matchstr(getline('.'), '\(^\s*\)') .'\%<' . line('.') . 'l\S', 'be')<CR>
noremap <M-j> :call search('^'. matchstr(getline('.'), '\(^\s*\)') .'\%>' . line('.') . 'l\S', 'e')<CR>
" }}}

" more complex stuff {{{
command! -nargs=1 Run new | resize 10 | silent execute '.!./' . string(<q-args>) . ' 2>&1' | nnoremap <silent> <buffer> q :q<CR> | setlocal ft=output readonly nomodifiable buftype=nowrite bufhidden=delete noswapfile nobuflisted


" update tags file on save if there is already a tags file
augroup AutoUpdateTags
    autocmd!
    autocmd BufWritePost * if filereadable('.tags') | silent execute '!ctags -Rf .tags' | elseif filereadable('tags') | silent execute '!ctags -Rf tags' | endif
augroup END

" Currently set for Python, Javascript, Elisp, and Clojure
nnoremap <Leader>d /^\s*\(def\\|class\\|function\|(defun\|(defn\|(defmacro\|(setq\)<Space>

" persistent undo
set undofile
set undodir=~/.vim/undo
" }}}

" filetype-specific {{{
" plain text {{{
function! MaximumLineLength(start, end)
    let i=a:start
    let max=0
    while i < a:end
        if max < strlen(getline(i))
            let max=strlen(getline(i))
        endif
        let i+=1
    endwhile
    return max
endfunction

function! WholeParagraphFormatExpr(start, end, width)
    " Collapse the range onto one line
    silent execute a:start . ',' . a:end . 's/\n/ /'
    silent execute a:start . 's/ $//'
    let lines_inserted=0
    " While there are too-long lines, add newlines as appropriate
    while search('\v.{' . a:width . '}', 'n', a:start + lines_inserted) != 0
        silent execute a:start + lines_inserted . 's/\v^.{1,' . a:width . '}\zs /\r/'
        let lines_inserted += 1
    endwhile
endfunction

augroup filetype_txt
    autocmd!
    autocmd BufEnter *.txt setlocal conceallevel=2
    autocmd BufEnter *.txt highlight Italics gui=italic
    autocmd BufEnter *.txt syntax match Italics /\<_.*_\>/
    " autocmd BufEnter *.txt syntax match Italics /\<_\ze.*_\>/ conceal
    " autocmd BufEnter *.txt syntax match Italics /\<_.*\zs_\>/ conceal
    autocmd BufEnter *.txt setlocal tw=80
    autocmd BufEnter *.txt setlocal formatexpr=ParagraphFormatExpr(v:lnum,v:lnum+v:count-1,80)
augroup END
" }}}

" cpp {{{
" c++ indentation style
augroup filetype_cpp
    autocmd!
    autocmd BufEnter *.cpp,*.h,*.hpp setlocal cindent
    autocmd BufEnter *.cpp,*.h,*.hpp setlocal cinoptions=g-1
augroup END
" }}}

" tex {{{
" autorender latex
augroup filetype_tex
    autocmd!
    autocmd BufWritePost *.tex,*.latex silent! execute '!latexmk -pdf % && latexmk -c'
    autocmd BufEnter *.tex,*.latex noremap <buffer><silent> j gj
    autocmd BufEnter *.tex,*.latex noremap <buffer><silent> k gk
    autocmd BufEnter *.tex,*.latex noremap <buffer><silent> 0 g0
    autocmd BufEnter *.tex,*.latex noremap <buffer><silent> $ g$
augroup END
" }}}

" mips assembly {{{
" use MIPS comments
augroup filetype_asm
    autocmd BufEnter *.s setlocal commentstring=#\ %s
    autocmd BufEnter *.s noremap <silent> <buffer> ]t :call search('^\ze\s*\w\+:')<CR>
    autocmd BufEnter *.s noremap <silent> <buffer> [t :call search('^\ze\s*\w\+:', 'b')<CR>
augroup END
" }}}

" haskell {{{
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
augroup filetype_hs
    autocmd!
    autocmd filetype *.hs setlocal shiftwidth=2
    autocmd filetype *.hs setlocal tabstop=2
    autocmd filetype *.hs setlocal softtabstop=2
augroup END
" }}}
" }}}
" }}}

" Plugin configuration {{{

" vimtex {{{
let g:polyglot_disabled = ['latex']
let g:vimtex_enabled = 1
" }}}

" arpeggio {{{
call arpeggio#load()
Arpeggio inoremap jk <Esc>
Arpeggio vnoremap jk <Esc>
Arpeggio imap kl <C-i>
" }}}

" ale {{{
let g:ale_linters={
            \ 'cpp':['clang', 'gcc', 'clangtidy', 'cppcheck', 'cpplint'],
            \ 'haskell': ['brittany', 'stack-ghc', 'stack-build', 'ghc-mod', 'stack-ghc-mod', 'hdevtools', 'hfmt'],
            \ 'asm': [],
            \ 'latex': []
            \}
" let g:ale_linters={'cpp':['clang', 'gcc', 'clangtidy', 'cppcheck', 'cpplint'],'haskell':['hlint', 'hdevtools', 'hfmt']}
let g:ale_set_quickfix=1
let g:ale_set_loclist=0
let g:ale_fixers = {'cpp':['clang-format']}
let g:ale_set_highlights=0

let g:ale_set_signs=0
map <silent> [e <Plug>(ale_previous)
map <silent> ]e <Plug>(ale_next)
nmap <silent> ga <Plug>(ale_detail)
noremap <silent> <Leader>ll :lopen<CR>
noremap <silent> [l :lprev<CR>
noremap <silent> ]l :lnext<CR>
noremap <silent> <Leader>kk :copen<CR>
noremap <silent> [q :cprev<CR>
noremap <silent> ]q :cnext<CR>
function! ALEUnderlineErrors()
    highlight EvieError gui=undercurl,italic guifg=sienna
    let pattern = ''
    for error in ale#engine#GetLoclist(bufnr('%'))
        let lnum = error['lnum']
        let cnum = error['col']
        let this_line = '\%' . lnum . 'l\%' . cnum . 'c\zs\w*'  " matches the line numbered lnum, from first non-whitespace character
        let pattern = pattern . '\|' . this_line
    endfor
    let pattern = pattern[2:]  " remove initial \|
    execute 'match EvieError /' . pattern . '/'
endfunction
augroup autoclose_loclist_quickfix
    autocmd!
    autocmd FileType qf noremap <buffer> <silent> q :q<CR>
    autocmd FileType qf match Error /error\ze:/
augroup END
augroup ale_underlining
    autocmd!
    autocmd User ALELint call ALEUnderlineErrors()
augroup END
" }}}

" fzf-vim {{{
nnoremap <Leader>e :Files<CR>
nnoremap <Leader>f :GFiles?<CR>
nnoremap <Leader>h :Helptags<CR>
" }}}

" vim-commentary {{{
nmap <Leader>cc gcc
nmap <Leader>c gc
vmap <Leader>cc gc
" }}}

" vim-fugitive {{{
nnoremap <Leader>gs :Gstatus<CR>
vnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gw :Gwrite<CR>
vnoremap <Leader>gw :Gwrite<CR>
nnoremap <Leader>gr :Gread<CR>
vnoremap <Leader>gr :Gread<CR>
nnoremap <Leader>G<Return> :Gcommit<CR>i
vnoremap <Leader>G<Return> :Gcommit<CR>i
" }}}

" git-gutter {{{
nnoremap [g :GitGutterPrevHunk<CR>
nnoremap ]g :GitGutterNextHunk<CR>
nnoremap <Leader>gpw :GitGutterStageHunk<CR>
nnoremap <Leader>gpr :GitGutterUndoHunk<CR>
" }}}

" easymotion {{{
map , <Leader><Leader>
" }}}

" buftabline {{{
let g:buftabline_numbers=1
" }}}

" }}}

" vim: foldmethod=marker
