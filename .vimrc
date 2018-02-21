" plug installs
call plug#begin("~/.vim/plugged")
Plug 'EESchneider/vim-rebase-mode'
Plug 'KeyboardFire/vim-minisnip'
Plug 'airblade/vim-gitgutter'
Plug 'ap/vim-buftabline'
Plug 'easymotion/vim-easymotion'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'kana/vim-arpeggio'
Plug 'machakann/vim-sandwich'
Plug 'majutsushi/tagbar'
Plug 'neovimhaskell/haskell-vim'
Plug 'romainl/vim-qf'
Plug 'sheerun/vim-polyglot'
Plug 'svermeulen/vim-easyclip'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-vinegar'
Plug 'w0rp/ale'
Plug 'wellle/targets.vim'
Plug 'osyo-manga/vim-over'

" pretty colors
Plug 'NLKNguyen/papercolor-theme'
Plug 'rakr/vim-two-firewatch'
Plug 'nanotech/jellybeans.vim'
Plug 'sonph/onehalf', { 'rtp': 'vim/' }
call plug#end()

colorscheme onehalflight

" for changing cursor shape in st
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"

filetype plugin indent on
set wildmenu
set incsearch
set nohlsearch
set infercase
set path+=**
set mouse=a
set scrolloff=3
set splitright
set splitbelow
set noswapfile
set number
set termguicolors
set diffopt+=vertical

noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

call arpeggio#load()
Arpeggio inoremap jk <Esc>
Arpeggio vnoremap jk <Esc>
Arpeggio imap kl <C-i>

let g:ale_linters={
            \ 'cpp':['clang', 'gcc', 'clangtidy', 'cppcheck', 'cpplint'],
            \ 'haskell': ['brittany', 'stack-ghc', 'stack-build', 'ghc-mod', 'stack-ghc-mod', 'hdevtools', 'hfmt']
            \}
" let g:ale_linters={'cpp':['clang', 'gcc', 'clangtidy', 'cppcheck', 'cpplint'],'haskell':['hlint', 'hdevtools', 'hfmt']}
let g:ale_set_quickfix=1
let g:ale_set_loclist=0
let g:ale_fixers = {'cpp':['clang-format']}
let g:ale_set_highlights=0
noremap ; :
noremap q; q:

command! -nargs=1 Run new | resize 10 | silent execute '.!./' . string(<q-args>) . ' 2>&1' | nnoremap <silent> <buffer> q :q<CR> | setlocal ft=output readonly nomodifiable buftype=nowrite bufhidden=delete noswapfile nobuflisted

vnoremap > >gv
vnoremap < <gv

" " clear highlight when the cursor moves
" set incsearch
" set hlsearch
" function! ClearHighlights()
"     nohlsearch
"     redraw
" endfunction
" augroup clear_highlights
"     autocmd!
"     autocmd CursorMoved * call ClearHighlights()
"     autocmd 
" augroup END

" update tags file on save if there is already a tags file
augroup AutoUpdateTags
    autocmd!
    autocmd BufWritePost * if filereadable('tags') | silent execute '!ctags -R' | endif
augroup END

" Currently set for Python, Javascript, Elisp, and Clojure
nnoremap <Leader>d /^\s*\(def\\|class\\|function\|(defun\|(defn\|(defmacro\|(setq\)<Space>

" persistent undo
set undofile
set undodir=~/.vim/undo

" TODO automatically insert indentation at start of paragraphs
" BUG make it work on the last line

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

" c++ indentation style
augroup filetype_cpp
    autocmd!
    autocmd BufEnter *.cpp,*.h,*.hpp setlocal cindent
    autocmd BufEnter *.cpp,*.h,*.hpp setlocal cinoptions=g-1
augroup END

" autorender latex
augroup filetype_tex
    autocmd!
    autocmd BufWritePost *.tex,*.latex silent! execute '!latexmk -pdf % && latexmk -c'
    autocmd BufEnter *.tex,*.latex noremap <buffer> j gj
    autocmd BufEnter *.tex,*.latex noremap <buffer> k gk
    autocmd BufEnter *.tex,*.latex noremap <buffer> 0 g0
    autocmd BufEnter *.tex,*.latex noremap <buffer> 9 g0
    autocmd BufEnter *.tex,*.latex noremap <buffer> $ g$
augroup END

" - does mark-setting
nnoremap - m
vnoremap - m

nnoremap 8 <C-y>
nnoremap 7 <C-e>

" indendation
set smarttab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" buffers
set hidden
nnoremap <silent> J :bn<CR>
nnoremap <silent> K :bp<CR>
nnoremap M J

" leader
map <Space> <Nop>
let mapleader="\<Space>"
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :bd<CR>
nnoremap <Leader>e :Files<CR>
nnoremap <Leader>f :GFiles?<CR>
nnoremap <Leader>h :Helptags<CR>

nnoremap 9 ^
vnoremap 9 ^

let g:netrw_liststyle=3
let g:netrw_winsize=25
let g:netrw_browse_alternative=4
let g:netrw_altv=1
nnoremap Z :Ex<CR>
nnoremap zf :Vex<CR>

" vim-commentary
nmap <Leader>cc gcc
nmap <Leader>c gc
vmap <Leader>cc gc

" vim-fugitive
nnoremap <Leader>gs :Gstatus<CR>
vnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gw :Gwrite<CR>
vnoremap <Leader>gw :Gwrite<CR>
nnoremap <Leader>gr :Gread<CR>
vnoremap <Leader>gr :Gread<CR>
nnoremap <Leader>G<Return> :Gcommit<CR>i
vnoremap <Leader>G<Return> :Gcommit<CR>i

" git-gutter
nnoremap <Leader>gk :GitGutterPrevHunk<CR>
nnoremap <Leader>gj :GitGutterNextHunk<CR>
nnoremap <Leader>gpw :GitGutterStageHunk<CR>
nnoremap <Leader>gpr :GitGutterUndoHunk<CR>

" rainbow
let g:rainbow_active = 1

" use , for easy-motion
map , <Leader><Leader>

" indentation jumping
noremap <M-k> :call search('^'. matchstr(getline('.'), '\(^\s*\)') .'\%<' . line('.') . 'l\S', 'be')<CR>
noremap <M-j> :call search('^'. matchstr(getline('.'), '\(^\s*\)') .'\%>' . line('.') . 'l\S', 'e')<CR>

" buftabline
let g:buftabline_numbers=1

" ale linting
let g:ale_set_signs=0
map <silent> <Leader>ak <Plug>(ale_previous)
map <silent> <Leader>aj <Plug>(ale_next)
nmap <silent> ga <Plug>(ale_detail)
noremap <silent> <Leader>ll :lopen<CR>
noremap <silent> <Leader>lk :lprev<CR>
noremap <silent> <Leader>lj :lnext<CR>
noremap <silent> <Leader>qq :copen<CR>
noremap <silent> <Leader>qk :cprev<CR>
noremap <silent> <Leader>qj :cnext<CR>
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
    autocmd FileType qf noremap <buffer> <silent> <Return> <Return>:lclose<CR>
    autocmd FileType qf noremap <buffer> <silent> g <Return>
    autocmd FileType qf noremap <buffer> <silent> q :q<CR>
    autocmd FileType qf match Error /error\ze:/
augroup END
augroup ale_underlining
    autocmd!
    autocmd User ALELint call ALEUnderlineErrors()
augroup END

" haskell-specific things
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

augroup cherryline
    autocmd BufEnter * source ~/.vim/cherryline.vim
augroup END
source ~/.vim/cherryline.vim  " make it so re-sourcing this .vimrc doesn't hide the bar
