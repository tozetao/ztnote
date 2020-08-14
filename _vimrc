set nocompatible

call plug#begin('$VIM/vimfiles/bundle')
Plug 'preservim/nerdtree'
Plug 'jistr/vim-nerdtree-tabs'

Plug 'ctrlpvim/ctrlp.vim'

Plug 'maralla/completor.vim'

Plug 'ludovicchabant/vim-gutentags'
Plug 'jiangmiao/auto-pairs'

call plug#end()

filetype plugin indent on

" python支持
" set pythonthreedll=python36.dll
set pythonthreedll=python27.dll

" 主题配置
syntax enable
set background=dark
colorscheme solarized


" 基本配置
set foldmethod=manual
set helplang=cn
set encoding=utf-8
set fileencodings=utf-8,chinese,latin-1
set guifont=Courier_new:h13:b:cDEFAULT
set guifont=Courier_new:h13
set tabstop=4
set shiftwidth=4
set softtabstop=4
set backspace=indent,eol,start
set number
set autoindent
set cindent
set wrap
set smartindent
set smarttab
set ignorecase
set hidden
set showmatch
set ruler
set incsearch
set lines=38
set columns=120
winpos 300 0 


source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim
language messages zh_CN.utf-8


" leader键设置
let mapleader=','

" NerdTree插件
map <leader>t :NERDTreeToggle <CR>
map <leader>guess :NERDTree d:/server/guess_new <CR>
map <leader>app :NERDTree d:/server/app <CR>
map <leader>buniu :NERDTree d:/server/cattle_server <CR>

" 当所有文件关闭时，退出NERDTred目录
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" 隐藏不显示的文件
let NERDTreeShowHidden=0
let NERDTreeIgnore=['\.pyc','\~$','\.swp', '\.beam', '\.svn', '\.root', '\.project']

map <leader>tn :tabnew<cr>
map <leader>tc :tabc<cr>
map <leader>to :tabo<cr>
map <c-tab> :tabn<cr>
map <s-tab> :tabp<cr>


" ctrlp插件
let g:ctrlp_map = '<leader>p'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
map <leader>f :CtrlPMRU<CR>
let g:ctrlp_custom_ignore = {
    \ 'dir':  '\v[\/]\.(git|hg|svn|rvm)$',
    \ 'file': '\v\.(exe|so|dll|zip|tar|tar.gz|pyc|beam)$',
    \ }


" completor插件
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"
let g:completor_erlang_omni_trigger='[\w-]+:$'
let g:completor_complete_options='menuone,noselect'


" gutentags插件：https://www.cnblogs.com/pengdonglin137/articles/10202606.html
" gutentags默认会执行setlocal tags+= 来添加变量
" gutentags插件会根据gutentags_project_root参数的配置来判断文件所在的工程目录，以此决定是否执行ctags命令
let g:gutentags_project_root = ['.root', '.svn', '.git', '.project']

let g:gutentags_ctags_tagfile = '.tags'

let s:vim_tags = expand('~/.cache/tags')
let g:gutentags_cache_dir = s:vim_tags
if !isdirectory(s:vim_tags)
   silent! call mkdir(s:vim_tags, 'p')
endif

let g:gutentags_ctags_extra_args = ['--fields=+niazS', '--extra=+q']
let g:gutentags_ctags_extra_args += ['--c++-kinds=+pxI']
let g:gutentags_ctags_extra_args += ['--c-kinds=+px']


" auto pairs插件
let g:AutoPairs = {'(':')', '[':']', '{':'}', "'":"'", '"':'"'}


" 括号补全
" inoremap ( ()<LEFT>
" inoremap [ []<LEFT>
" inoremap { {}<LEFT>
" inoremap " ""<LEFT>
" inoremap ' ''<LEFT>
" inoremap < <><LEFT>
" 
" function! RemovePairs()
"     let s:line = getline(".")
"     let s:previous_char = s:line[col(".")-1]
" 
"     if index(["(", "[", "{"],s:previous_char) != -1
"         let l:original_pos = getpos(".")
"         execute "normal %"
"         let l:new_pos = getpos(".")
"         " only right (
"         if l:original_pos == l:new_pos
"             execute "normal! a\<BS>"
"             return
"         end
" 
"         let l:line2 = getline(".")
"         if len(l:line2) == col(".")
"             execute "normal! v%xa"
"         else
"             execute "normal! v%xi"
"         end
"     else
"         execute "normal! a\<BS>"
"     end
" endfunction
" 
" function! RemoveNextDoubleChar(char)
"     let l:line = getline(".")
"     let l:next_char = l:line[col(".")]
" 
"     if a:char == l:next_char
"         execute "normal! l"
"     else
"         execute "normal! i" . a:char . ""
"     end
" endfunction
" 
" inoremap <BS> <ESC>:call RemovePairs()<CR>a
" inoremap ) <ESC>:call RemoveNextDoubleChar(')')<CR>a
" inoremap ] <ESC>:call RemoveNextDoubleChar(']')<CR>a
" inoremap } <ESC>:call RemoveNextDoubleChar('}')<CR>a
