let g:in_git_directory = 1
function! IsGitDir()
    return g:in_git_directory
endfunction

function! GitHead()
    if !IsGitDir()
        return ''
    elseif strlen(fugitive#head()) > 0
        return fugitive#head()
    elseif strlen(fugitive#head(7)) > 0
        return fugitive#head(7) . ' (detached)'
    else
        return ''
    endif
endfunction

function! AleStatus()
    let status=ale#statusline#Count(expand('%'))
    let acc=[]

    if status['error']
        call add(acc, status['error'])
    endif
    if status['warning']
        call add(acc, status['warning'])
    endif

    return join(acc, ', ')
endfunction

function! GitInsertions()
    if !IsGitDir()
        return ''
    elseif empty(fugitive#head(7))
        return ''
    endif
    let answer=matchstr(system('git diff --shortstat'), '\V\d\+\ze insertion')
    if empty(answer)
        return ''
    else
        return ' '.answer.' '
    endif
endfunction

function! GitDeletions()
    if !IsGitDir()
        return ''
    elseif empty(fugitive#head(7))
        return ''
    endif
    let answer=matchstr(system('git diff --shortstat'), '\V\d\+\ze deletion')
    if empty(answer)
        return ''
    else
        return ' '.answer.' '
    endif
endfunction

" function! GitFileInsertions()
"     if !IsGitDir()
"         return ''
"     endif
"     let file_path=gitgutter#utility#file_relative_to_repo_root()
"     let file_path=escape(file_path, '.')
"     echom file_path
"     let inserts=matchstr(system('git diff --numstat'), '^\d\+\ze\s\+\d\+\s*'.file_path)
"     if empty(inserts) || inserts ==# '0'
"         return ''
"     else
"         return ' '.inserts.' '
"     endif
" endfunction

" function! GitFileDeletions()
"     if !IsGitDir()
"         return ''
"     endif
"     let file_path=gitgutter#utility#file_relative_to_repo_root()
"     let deletions=matchstr(system('git diff --numstat'), '^\d\+\s\+\zs\d\+\ze\s*'.file_path)
"     if empty(deletions) || deletions ==# '0'
"         return ''
"     else
"         return ' '.deletions.' '
"     endif
" endfunction

let modes = {
            \ 'n': 'N',
            \ 'i': 'I',
            \ 'R': 'R',
            \ 'v': 'V',
            \ 'V': 'V-LN',
            \ "\<C-v>": 'V-BL',
            \ 'c': 'C',
            \ 's': 'S',
            \ 'S': 'S-LN',
            \ "\<C-s>": 'S-BL',
            \ 't': 'T'
            \}

if &bg ==# 'dark'
    highlight CherrylineMode guifg=darkgreen guibg=olivedrab1 gui=bold
    highlight CherrylineGitBranch guifg=#b7bdae guibg=gray26
    highlight CherrylineFilename guifg=gray90 guibg=gray15
    highlight CherrylineLineNumber guifg=black guibg=lavender
    highlight CherrylineGitInsert guifg=black guibg=olivedrab3
    highlight CherrylineGitDeletion guifg=white guibg=red1
else
    highlight CherrylineMode guifg=darkslategrey guibg=darkseagreen3 gui=bold
    highlight CherrylineGitBranch guifg=darkgreen guibg=bisque1
    highlight CherrylineFilename guifg=gray15 guibg=lightcyan1
    highlight CherrylineLineNumber guifg=black guibg=skyblue1
    highlight CherrylineGitInsert guifg=black guibg=olivedrab3
    highlight CherrylineGitDeletion guifg=white guibg=red1
endif

set statusline=%#CherrylineMode#\ %{get(modes,mode(),'OwO?...')}\ 
set statusline+=%#CherrylineGitBranch#%{strlen(GitHead())>0?'\ '.GitHead().'\ ':''}
" set statusline+=%#CherrylineGitInsert#%{empty(GitInsertions())?'':'\ '.GitInsertions()}
" set statusline+=%#CherrylineGitDeletion#%{empty(GitDeletions())?'':'\ '.GitDeletions()}
set statusline+=%#CherrylineFilename#\ %t\ %m%r%w\ 

set statusline+=%=
set statusline+=\ %{&ft}\ 
set statusline+=%{empty(AleStatus())?'':'~\ '.AleStatus().'\ '}
" set statusline+=%#CherrylineGitInsert#%{empty(GitFileInsertions())?'':'\ '.GitFileInsertions()}
" set statusline+=%#CherrylineGitDeletion#%{empty(GitFileDeletions())?'':'\ '.GitFileDeletions()}
set statusline+=%#CherrylineLineNumber#\ %l:%c/%L\ %P\ 
