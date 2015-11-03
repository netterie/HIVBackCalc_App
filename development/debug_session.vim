let SessionLoad = 1
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
map! <D-v> *
nmap  :tabnew
vmap ,Sws :s/ /_/gc
nmap ,Sws :s/ /_/gc
map Q gq
map \shh :call RAction("show")
map \rdd :call RAction("dim")
map \rtt :call RAction("tail")
map \hh :call RAction("head")
vmap gx <Plug>NetrwBrowseXVis
nmap gx <Plug>NetrwBrowseX
nnoremap qd :silent! normal mpea"bi"`pl
nnoremap qw :silent! normal mpea'bi'`pl
nnoremap wq :silent! normal mpeld bhd `ph
vnoremap <silent> <Plug>NetrwBrowseXVis :call netrw#BrowseXVis()
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#BrowseX(expand((exists("g:netrw_gx")? g:netrw_gx : '<cfile>')),netrw#CheckIfRemote())
map <F7> :set spell!
map <C-Down> o
nmap <C-S-Tab> :tabprevious
nmap <C-Tab> :tabnext
map <F10> :set invhls
map <F5> :syntax on
map <F4> :syntax on
map <F3> :syntax on
map <F2> :syntax on
vmap <BS> "-d
vmap <D-x> "*d
vmap <D-c> "*y
vmap <D-v> "-d"*P
nmap <D-v> "*P
abbr _ <-
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set backspace=indent,eol,start
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set helplang=en
set history=100
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set ruler
set runtimepath=~/.vim,~/.vim/bundle/Vundle.vim,~/.vim/bundle/Vim-R-plugin,~/.vim/bundle/vim-rmarkdown,/usr/local/share/vim/vimfiles,/usr/local/share/vim/vim74,/usr/local/share/vim/vimfiles/after,~/.vim/after,~/.vim/bundle/Vundle.vim,~/.vim/bundle/Vundle.vim/after,~/.vim/bundle/Vim-R-plugin/after,~/.vim/bundle/vim-rmarkdown/after
set scrolloff=2
set shiftwidth=4
set showcmd
set smartcase
set smarttab
set spellsuggest=best,10
set statusline=%F%m%r%h%w\ (FORMAT=%{&ff})\ (ASCII=%03.3b)\ (POS=%4l,%4v)\ (LEN=%L)
set tabstop=4
set wildmode=longest,list
set window=50
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/Dropbox/School/PhD/HIV_WA/analysis_WA
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +0 2015_HIVBackCalc_report.Rnw
badd +0 ../HIVBackCalc/R/temp.R
badd +0 ../HIVBackCalc/R/internal_fxns.R
badd +0 ../HIVBackCalc_App/development/debug_fxns.R
badd +0 ../HIVBackCalc_App/development/debug_run.R
badd +0 ../HIVBackCalc_App/server.R
badd +0 ../HIVBackCalc_App/development/edit_app.R
badd +0 ../HIVBackCalc_App/development/model.R
badd +0 ../HIVBackCalc_App/development/other.R
argglobal
silent! argdel *
argadd 2015_HIVBackCalc_report.Rnw
set stal=2
edit ../HIVBackCalc_App/development/debug_fxns.R
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
vnoremap <buffer> <silent> \rd :call RSetWD()
vnoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
vnoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
vnoremap <buffer> <silent> \ro :call RObjBrowser()
vnoremap <buffer> <silent> \rb :call RAction("plotsumm")
vnoremap <buffer> <silent> \rg :call RAction("plot")
vnoremap <buffer> <silent> \rs :call RAction("summary")
vnoremap <buffer> <silent> \rh :call RAction("help")
vnoremap <buffer> <silent> \re :call RAction("example")
vnoremap <buffer> <silent> \ra :call RAction("args")
vnoremap <buffer> <silent> \rv :call RAction("viewdf")
vnoremap <buffer> <silent> \rt :call RAction("str")
vnoremap <buffer> <silent> \rn :call RAction("vim.names")
vnoremap <buffer> <silent> \rp :call RAction("print")
vnoremap <buffer> <silent> \rm :call RClearAll()
vnoremap <buffer> <silent> \rr :call RClearConsole()
vnoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
vnoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
vnoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
vnoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
vnoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
vnoremap <buffer> <silent> \; :call MovePosRCodeComment("selection")
vnoremap <buffer> <silent> \xu :call RSimpleCommentLine("selection", "u")
vnoremap <buffer> <silent> \xc :call RSimpleCommentLine("selection", "c")
vnoremap <buffer> <silent> \xx :call RComment("selection")
vnoremap <buffer> <silent> \rw :call RQuit('save')
vnoremap <buffer> <silent> \rq :call RQuit('nosave')
vnoremap <buffer> <silent> \rc :call StartR("custom")
vnoremap <buffer> <silent> \rf :call StartR("R")
nnoremap <buffer> <silent> \rd :call RSetWD()
onoremap <buffer> <silent> \rd :call RSetWD()
nnoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
onoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
nnoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
onoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
nnoremap <buffer> <silent> \ro :call RObjBrowser()
onoremap <buffer> <silent> \ro :call RObjBrowser()
nnoremap <buffer> <silent> \rb :call RAction("plotsumm")
onoremap <buffer> <silent> \rb :call RAction("plotsumm")
nnoremap <buffer> <silent> \rg :call RAction("plot")
onoremap <buffer> <silent> \rg :call RAction("plot")
nnoremap <buffer> <silent> \rs :call RAction("summary")
onoremap <buffer> <silent> \rs :call RAction("summary")
nnoremap <buffer> <silent> \rh :call RAction("help")
onoremap <buffer> <silent> \rh :call RAction("help")
nnoremap <buffer> <silent> \re :call RAction("example")
onoremap <buffer> <silent> \re :call RAction("example")
nnoremap <buffer> <silent> \ra :call RAction("args")
onoremap <buffer> <silent> \ra :call RAction("args")
nnoremap <buffer> <silent> \rv :call RAction("viewdf")
onoremap <buffer> <silent> \rv :call RAction("viewdf")
nnoremap <buffer> <silent> \rt :call RAction("str")
onoremap <buffer> <silent> \rt :call RAction("str")
nnoremap <buffer> <silent> \rn :call RAction("vim.names")
onoremap <buffer> <silent> \rn :call RAction("vim.names")
nnoremap <buffer> <silent> \rp :call RAction("print")
onoremap <buffer> <silent> \rp :call RAction("print")
nnoremap <buffer> <silent> \rm :call RClearAll()
onoremap <buffer> <silent> \rm :call RClearAll()
nnoremap <buffer> <silent> \rr :call RClearConsole()
onoremap <buffer> <silent> \rr :call RClearConsole()
nnoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
onoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
nnoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
onoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
nnoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
onoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
nnoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
onoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
nnoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
onoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
nnoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
onoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
nnoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
onoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
nnoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
onoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
nnoremap <buffer> <silent> \xx :call RComment("normal")
onoremap <buffer> <silent> \xx :call RComment("normal")
nnoremap <buffer> <silent> \rw :call RQuit('save')
onoremap <buffer> <silent> \rw :call RQuit('save')
nnoremap <buffer> <silent> \rq :call RQuit('nosave')
onoremap <buffer> <silent> \rq :call RQuit('nosave')
nnoremap <buffer> <silent> \rc :call StartR("custom")
onoremap <buffer> <silent> \rc :call StartR("custom")
nnoremap <buffer> <silent> \rf :call StartR("R")
onoremap <buffer> <silent> \rf :call StartR("R")
let s:cpo_save=&cpo
set cpo&vim
noremap <buffer> <silent> \r<Right> :call RSendPartOfLine("right", 0)
noremap <buffer> <silent> \r<Left> :call RSendPartOfLine("left", 0)
noremap <buffer> <silent> \o :call SendLineToRAndInsertOutput()
noremap <buffer> <silent> \d :call SendLineToR("down")
noremap <buffer> <silent> \l :call SendLineToR("stay")
noremap <buffer> <silent> \pa :call SendParagraphToR("echo", "down")
noremap <buffer> <silent> \pd :call SendParagraphToR("silent", "down")
noremap <buffer> <silent> \pe :call SendParagraphToR("echo", "stay")
noremap <buffer> <silent> \pp :call SendParagraphToR("silent", "stay")
vnoremap <buffer> <silent> \so :call SendSelectionToR("echo", "stay", "NewtabInsert")
vnoremap <buffer> <silent> \sa :call SendSelectionToR("echo", "down")
vnoremap <buffer> <silent> \sd :call SendSelectionToR("silent", "down")
vnoremap <buffer> <silent> \se :call SendSelectionToR("echo", "stay")
vnoremap <buffer> <silent> \ss :call SendSelectionToR("silent", "stay")
noremap <buffer> <silent> \ba :call SendMBlockToR("echo", "down")
noremap <buffer> <silent> \bd :call SendMBlockToR("silent", "down")
noremap <buffer> <silent> \be :call SendMBlockToR("echo", "stay")
noremap <buffer> <silent> \bb :call SendMBlockToR("silent", "stay")
noremap <buffer> <silent> \ks :call RSpin()
noremap <buffer> <silent> \ao :call ShowRout()
noremap <buffer> <silent> \ae :call SendFileToR("echo")
noremap <buffer> <silent> \aa :call SendFileToR("silent")
imap <buffer> <silent>  =RCompleteArgs()
inoremap <buffer> <silent> \rd :call RSetWD()
inoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
inoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
inoremap <buffer> <silent> \ro :call RObjBrowser()
inoremap <buffer> <silent> \rb :call RAction("plotsumm")
inoremap <buffer> <silent> \rg :call RAction("plot")
inoremap <buffer> <silent> \rs :call RAction("summary")
inoremap <buffer> <silent> \rh :call RAction("help")
inoremap <buffer> <silent> \re :call RAction("example")
inoremap <buffer> <silent> \ra :call RAction("args")
inoremap <buffer> <silent> \rv :call RAction("viewdf")
inoremap <buffer> <silent> \rt :call RAction("str")
inoremap <buffer> <silent> \rn :call RAction("vim.names")
inoremap <buffer> <silent> \rp :call RAction("print")
inoremap <buffer> <silent> \rm :call RClearAll()
inoremap <buffer> <silent> \rr :call RClearConsole()
inoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
inoremap <buffer> <silent> \r<Right> l:call RSendPartOfLine("right", 1)
inoremap <buffer> <silent> \r<Left> l:call RSendPartOfLine("left", 1)
inoremap <buffer> <silent> \q :call SendLineToR("newline")
inoremap <buffer> <silent> \o :call SendLineToRAndInsertOutput()
inoremap <buffer> <silent> \d :call SendLineToR("down")
inoremap <buffer> <silent> \l :call SendLineToR("stay")
inoremap <buffer> <silent> \pa :call SendParagraphToR("echo", "down")
inoremap <buffer> <silent> \pd :call SendParagraphToR("silent", "down")
inoremap <buffer> <silent> \pe :call SendParagraphToR("echo", "stay")
inoremap <buffer> <silent> \pp :call SendParagraphToR("silent", "stay")
inoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
inoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
inoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
inoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
inoremap <buffer> <silent> \ba :call SendMBlockToR("echo", "down")
inoremap <buffer> <silent> \bd :call SendMBlockToR("silent", "down")
inoremap <buffer> <silent> \be :call SendMBlockToR("echo", "stay")
inoremap <buffer> <silent> \bb :call SendMBlockToR("silent", "stay")
inoremap <buffer> <silent> \ks :call RSpin()
inoremap <buffer> <silent> \ao :call ShowRout()
inoremap <buffer> <silent> \ae :call SendFileToR("echo")
inoremap <buffer> <silent> \aa :call SendFileToR("silent")
inoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
inoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
inoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
inoremap <buffer> <silent> \xx :call RComment("normal")
inoremap <buffer> <silent> \rw :call RQuit('save')
inoremap <buffer> <silent> \rq :call RQuit('nosave')
inoremap <buffer> <silent> \rc :call StartR("custom")
inoremap <buffer> <silent> \rf :call StartR("R")
imap <buffer> <silent> _ :call ReplaceUnderS()
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal backupcopy=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=:#',:###,:##,:#
setlocal commentstring=#\ %s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'r'
setlocal filetype=r
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=cq
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=0
setlocal include=
setlocal includeexpr=
setlocal indentexpr=GetRIndent()
setlocal indentkeys=0{,0},:,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=@,48-57,_,.
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=rcomplete#CompleteR
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=4
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'r'
setlocal syntax=r
endif
setlocal tabstop=4
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal undolevels=-123456
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 1 - ((0 * winheight(0) + 24) / 48)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
tabedit ../HIVBackCalc_App/development/debug_run.R
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
vnoremap <buffer> <silent> \rd :call RSetWD()
vnoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
vnoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
vnoremap <buffer> <silent> \ro :call RObjBrowser()
vnoremap <buffer> <silent> \rb :call RAction("plotsumm")
vnoremap <buffer> <silent> \rg :call RAction("plot")
vnoremap <buffer> <silent> \rs :call RAction("summary")
vnoremap <buffer> <silent> \rh :call RAction("help")
vnoremap <buffer> <silent> \re :call RAction("example")
vnoremap <buffer> <silent> \ra :call RAction("args")
vnoremap <buffer> <silent> \rv :call RAction("viewdf")
vnoremap <buffer> <silent> \rt :call RAction("str")
vnoremap <buffer> <silent> \rn :call RAction("vim.names")
vnoremap <buffer> <silent> \rp :call RAction("print")
vnoremap <buffer> <silent> \rm :call RClearAll()
vnoremap <buffer> <silent> \rr :call RClearConsole()
vnoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
vnoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
vnoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
vnoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
vnoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
vnoremap <buffer> <silent> \; :call MovePosRCodeComment("selection")
vnoremap <buffer> <silent> \xu :call RSimpleCommentLine("selection", "u")
vnoremap <buffer> <silent> \xc :call RSimpleCommentLine("selection", "c")
vnoremap <buffer> <silent> \xx :call RComment("selection")
vnoremap <buffer> <silent> \rw :call RQuit('save')
vnoremap <buffer> <silent> \rq :call RQuit('nosave')
vnoremap <buffer> <silent> \rc :call StartR("custom")
vnoremap <buffer> <silent> \rf :call StartR("R")
nnoremap <buffer> <silent> \rd :call RSetWD()
onoremap <buffer> <silent> \rd :call RSetWD()
nnoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
onoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
nnoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
onoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
nnoremap <buffer> <silent> \ro :call RObjBrowser()
onoremap <buffer> <silent> \ro :call RObjBrowser()
nnoremap <buffer> <silent> \rb :call RAction("plotsumm")
onoremap <buffer> <silent> \rb :call RAction("plotsumm")
nnoremap <buffer> <silent> \rg :call RAction("plot")
onoremap <buffer> <silent> \rg :call RAction("plot")
nnoremap <buffer> <silent> \rs :call RAction("summary")
onoremap <buffer> <silent> \rs :call RAction("summary")
nnoremap <buffer> <silent> \rh :call RAction("help")
onoremap <buffer> <silent> \rh :call RAction("help")
nnoremap <buffer> <silent> \re :call RAction("example")
onoremap <buffer> <silent> \re :call RAction("example")
nnoremap <buffer> <silent> \ra :call RAction("args")
onoremap <buffer> <silent> \ra :call RAction("args")
nnoremap <buffer> <silent> \rv :call RAction("viewdf")
onoremap <buffer> <silent> \rv :call RAction("viewdf")
nnoremap <buffer> <silent> \rt :call RAction("str")
onoremap <buffer> <silent> \rt :call RAction("str")
nnoremap <buffer> <silent> \rn :call RAction("vim.names")
onoremap <buffer> <silent> \rn :call RAction("vim.names")
nnoremap <buffer> <silent> \rp :call RAction("print")
onoremap <buffer> <silent> \rp :call RAction("print")
nnoremap <buffer> <silent> \rm :call RClearAll()
onoremap <buffer> <silent> \rm :call RClearAll()
nnoremap <buffer> <silent> \rr :call RClearConsole()
onoremap <buffer> <silent> \rr :call RClearConsole()
nnoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
onoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
nnoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
onoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
nnoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
onoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
nnoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
onoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
nnoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
onoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
nnoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
onoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
nnoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
onoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
nnoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
onoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
nnoremap <buffer> <silent> \xx :call RComment("normal")
onoremap <buffer> <silent> \xx :call RComment("normal")
nnoremap <buffer> <silent> \rw :call RQuit('save')
onoremap <buffer> <silent> \rw :call RQuit('save')
nnoremap <buffer> <silent> \rq :call RQuit('nosave')
onoremap <buffer> <silent> \rq :call RQuit('nosave')
nnoremap <buffer> <silent> \rc :call StartR("custom")
onoremap <buffer> <silent> \rc :call StartR("custom")
nnoremap <buffer> <silent> \rf :call StartR("R")
onoremap <buffer> <silent> \rf :call StartR("R")
let s:cpo_save=&cpo
set cpo&vim
noremap <buffer> <silent> \r<Right> :call RSendPartOfLine("right", 0)
noremap <buffer> <silent> \r<Left> :call RSendPartOfLine("left", 0)
noremap <buffer> <silent> \o :call SendLineToRAndInsertOutput()
noremap <buffer> <silent> \d :call SendLineToR("down")
noremap <buffer> <silent> \l :call SendLineToR("stay")
noremap <buffer> <silent> \pa :call SendParagraphToR("echo", "down")
noremap <buffer> <silent> \pd :call SendParagraphToR("silent", "down")
noremap <buffer> <silent> \pe :call SendParagraphToR("echo", "stay")
noremap <buffer> <silent> \pp :call SendParagraphToR("silent", "stay")
vnoremap <buffer> <silent> \so :call SendSelectionToR("echo", "stay", "NewtabInsert")
vnoremap <buffer> <silent> \sa :call SendSelectionToR("echo", "down")
vnoremap <buffer> <silent> \sd :call SendSelectionToR("silent", "down")
vnoremap <buffer> <silent> \se :call SendSelectionToR("echo", "stay")
vnoremap <buffer> <silent> \ss :call SendSelectionToR("silent", "stay")
noremap <buffer> <silent> \ba :call SendMBlockToR("echo", "down")
noremap <buffer> <silent> \bd :call SendMBlockToR("silent", "down")
noremap <buffer> <silent> \be :call SendMBlockToR("echo", "stay")
noremap <buffer> <silent> \bb :call SendMBlockToR("silent", "stay")
noremap <buffer> <silent> \ks :call RSpin()
noremap <buffer> <silent> \ao :call ShowRout()
noremap <buffer> <silent> \ae :call SendFileToR("echo")
noremap <buffer> <silent> \aa :call SendFileToR("silent")
imap <buffer> <silent>  =RCompleteArgs()
inoremap <buffer> <silent> \rd :call RSetWD()
inoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
inoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
inoremap <buffer> <silent> \ro :call RObjBrowser()
inoremap <buffer> <silent> \rb :call RAction("plotsumm")
inoremap <buffer> <silent> \rg :call RAction("plot")
inoremap <buffer> <silent> \rs :call RAction("summary")
inoremap <buffer> <silent> \rh :call RAction("help")
inoremap <buffer> <silent> \re :call RAction("example")
inoremap <buffer> <silent> \ra :call RAction("args")
inoremap <buffer> <silent> \rv :call RAction("viewdf")
inoremap <buffer> <silent> \rt :call RAction("str")
inoremap <buffer> <silent> \rn :call RAction("vim.names")
inoremap <buffer> <silent> \rp :call RAction("print")
inoremap <buffer> <silent> \rm :call RClearAll()
inoremap <buffer> <silent> \rr :call RClearConsole()
inoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
inoremap <buffer> <silent> \r<Right> l:call RSendPartOfLine("right", 1)
inoremap <buffer> <silent> \r<Left> l:call RSendPartOfLine("left", 1)
inoremap <buffer> <silent> \q :call SendLineToR("newline")
inoremap <buffer> <silent> \o :call SendLineToRAndInsertOutput()
inoremap <buffer> <silent> \d :call SendLineToR("down")
inoremap <buffer> <silent> \l :call SendLineToR("stay")
inoremap <buffer> <silent> \pa :call SendParagraphToR("echo", "down")
inoremap <buffer> <silent> \pd :call SendParagraphToR("silent", "down")
inoremap <buffer> <silent> \pe :call SendParagraphToR("echo", "stay")
inoremap <buffer> <silent> \pp :call SendParagraphToR("silent", "stay")
inoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
inoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
inoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
inoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
inoremap <buffer> <silent> \ba :call SendMBlockToR("echo", "down")
inoremap <buffer> <silent> \bd :call SendMBlockToR("silent", "down")
inoremap <buffer> <silent> \be :call SendMBlockToR("echo", "stay")
inoremap <buffer> <silent> \bb :call SendMBlockToR("silent", "stay")
inoremap <buffer> <silent> \ks :call RSpin()
inoremap <buffer> <silent> \ao :call ShowRout()
inoremap <buffer> <silent> \ae :call SendFileToR("echo")
inoremap <buffer> <silent> \aa :call SendFileToR("silent")
inoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
inoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
inoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
inoremap <buffer> <silent> \xx :call RComment("normal")
inoremap <buffer> <silent> \rw :call RQuit('save')
inoremap <buffer> <silent> \rq :call RQuit('nosave')
inoremap <buffer> <silent> \rc :call StartR("custom")
inoremap <buffer> <silent> \rf :call StartR("R")
imap <buffer> <silent> _ :call ReplaceUnderS()
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal backupcopy=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=:#',:###,:##,:#
setlocal commentstring=#\ %s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'r'
setlocal filetype=r
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=cq
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=0
setlocal include=
setlocal includeexpr=
setlocal indentexpr=GetRIndent()
setlocal indentkeys=0{,0},:,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=@,48-57,_,.
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=rcomplete#CompleteR
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=4
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'r'
setlocal syntax=r
endif
setlocal tabstop=4
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal undolevels=-123456
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 60 - ((31 * winheight(0) + 24) / 48)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
60
normal! 0
tabedit ../HIVBackCalc_App/server.R
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
vnoremap <buffer> <silent> \rd :call RSetWD()
vnoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
vnoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
vnoremap <buffer> <silent> \ro :call RObjBrowser()
vnoremap <buffer> <silent> \rb :call RAction("plotsumm")
vnoremap <buffer> <silent> \rg :call RAction("plot")
vnoremap <buffer> <silent> \rs :call RAction("summary")
vnoremap <buffer> <silent> \rh :call RAction("help")
vnoremap <buffer> <silent> \re :call RAction("example")
vnoremap <buffer> <silent> \ra :call RAction("args")
vnoremap <buffer> <silent> \rv :call RAction("viewdf")
vnoremap <buffer> <silent> \rt :call RAction("str")
vnoremap <buffer> <silent> \rn :call RAction("vim.names")
vnoremap <buffer> <silent> \rp :call RAction("print")
vnoremap <buffer> <silent> \rm :call RClearAll()
vnoremap <buffer> <silent> \rr :call RClearConsole()
vnoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
vnoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
vnoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
vnoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
vnoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
vnoremap <buffer> <silent> \; :call MovePosRCodeComment("selection")
vnoremap <buffer> <silent> \xu :call RSimpleCommentLine("selection", "u")
vnoremap <buffer> <silent> \xc :call RSimpleCommentLine("selection", "c")
vnoremap <buffer> <silent> \xx :call RComment("selection")
vnoremap <buffer> <silent> \rw :call RQuit('save')
vnoremap <buffer> <silent> \rq :call RQuit('nosave')
vnoremap <buffer> <silent> \rc :call StartR("custom")
vnoremap <buffer> <silent> \rf :call StartR("R")
nnoremap <buffer> <silent> \rd :call RSetWD()
onoremap <buffer> <silent> \rd :call RSetWD()
nnoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
onoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
nnoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
onoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
nnoremap <buffer> <silent> \ro :call RObjBrowser()
onoremap <buffer> <silent> \ro :call RObjBrowser()
nnoremap <buffer> <silent> \rb :call RAction("plotsumm")
onoremap <buffer> <silent> \rb :call RAction("plotsumm")
nnoremap <buffer> <silent> \rg :call RAction("plot")
onoremap <buffer> <silent> \rg :call RAction("plot")
nnoremap <buffer> <silent> \rs :call RAction("summary")
onoremap <buffer> <silent> \rs :call RAction("summary")
nnoremap <buffer> <silent> \rh :call RAction("help")
onoremap <buffer> <silent> \rh :call RAction("help")
nnoremap <buffer> <silent> \re :call RAction("example")
onoremap <buffer> <silent> \re :call RAction("example")
nnoremap <buffer> <silent> \ra :call RAction("args")
onoremap <buffer> <silent> \ra :call RAction("args")
nnoremap <buffer> <silent> \rv :call RAction("viewdf")
onoremap <buffer> <silent> \rv :call RAction("viewdf")
nnoremap <buffer> <silent> \rt :call RAction("str")
onoremap <buffer> <silent> \rt :call RAction("str")
nnoremap <buffer> <silent> \rn :call RAction("vim.names")
onoremap <buffer> <silent> \rn :call RAction("vim.names")
nnoremap <buffer> <silent> \rp :call RAction("print")
onoremap <buffer> <silent> \rp :call RAction("print")
nnoremap <buffer> <silent> \rm :call RClearAll()
onoremap <buffer> <silent> \rm :call RClearAll()
nnoremap <buffer> <silent> \rr :call RClearConsole()
onoremap <buffer> <silent> \rr :call RClearConsole()
nnoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
onoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
nnoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
onoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
nnoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
onoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
nnoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
onoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
nnoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
onoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
nnoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
onoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
nnoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
onoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
nnoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
onoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
nnoremap <buffer> <silent> \xx :call RComment("normal")
onoremap <buffer> <silent> \xx :call RComment("normal")
nnoremap <buffer> <silent> \rw :call RQuit('save')
onoremap <buffer> <silent> \rw :call RQuit('save')
nnoremap <buffer> <silent> \rq :call RQuit('nosave')
onoremap <buffer> <silent> \rq :call RQuit('nosave')
nnoremap <buffer> <silent> \rc :call StartR("custom")
onoremap <buffer> <silent> \rc :call StartR("custom")
nnoremap <buffer> <silent> \rf :call StartR("R")
onoremap <buffer> <silent> \rf :call StartR("R")
let s:cpo_save=&cpo
set cpo&vim
noremap <buffer> <silent> \r<Right> :call RSendPartOfLine("right", 0)
noremap <buffer> <silent> \r<Left> :call RSendPartOfLine("left", 0)
noremap <buffer> <silent> \o :call SendLineToRAndInsertOutput()
noremap <buffer> <silent> \d :call SendLineToR("down")
noremap <buffer> <silent> \l :call SendLineToR("stay")
noremap <buffer> <silent> \pa :call SendParagraphToR("echo", "down")
noremap <buffer> <silent> \pd :call SendParagraphToR("silent", "down")
noremap <buffer> <silent> \pe :call SendParagraphToR("echo", "stay")
noremap <buffer> <silent> \pp :call SendParagraphToR("silent", "stay")
vnoremap <buffer> <silent> \so :call SendSelectionToR("echo", "stay", "NewtabInsert")
vnoremap <buffer> <silent> \sa :call SendSelectionToR("echo", "down")
vnoremap <buffer> <silent> \sd :call SendSelectionToR("silent", "down")
vnoremap <buffer> <silent> \se :call SendSelectionToR("echo", "stay")
vnoremap <buffer> <silent> \ss :call SendSelectionToR("silent", "stay")
noremap <buffer> <silent> \ba :call SendMBlockToR("echo", "down")
noremap <buffer> <silent> \bd :call SendMBlockToR("silent", "down")
noremap <buffer> <silent> \be :call SendMBlockToR("echo", "stay")
noremap <buffer> <silent> \bb :call SendMBlockToR("silent", "stay")
noremap <buffer> <silent> \ks :call RSpin()
noremap <buffer> <silent> \ao :call ShowRout()
noremap <buffer> <silent> \ae :call SendFileToR("echo")
noremap <buffer> <silent> \aa :call SendFileToR("silent")
imap <buffer> <silent>  =RCompleteArgs()
inoremap <buffer> <silent> \rd :call RSetWD()
inoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
inoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
inoremap <buffer> <silent> \ro :call RObjBrowser()
inoremap <buffer> <silent> \rb :call RAction("plotsumm")
inoremap <buffer> <silent> \rg :call RAction("plot")
inoremap <buffer> <silent> \rs :call RAction("summary")
inoremap <buffer> <silent> \rh :call RAction("help")
inoremap <buffer> <silent> \re :call RAction("example")
inoremap <buffer> <silent> \ra :call RAction("args")
inoremap <buffer> <silent> \rv :call RAction("viewdf")
inoremap <buffer> <silent> \rt :call RAction("str")
inoremap <buffer> <silent> \rn :call RAction("vim.names")
inoremap <buffer> <silent> \rp :call RAction("print")
inoremap <buffer> <silent> \rm :call RClearAll()
inoremap <buffer> <silent> \rr :call RClearConsole()
inoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
inoremap <buffer> <silent> \r<Right> l:call RSendPartOfLine("right", 1)
inoremap <buffer> <silent> \r<Left> l:call RSendPartOfLine("left", 1)
inoremap <buffer> <silent> \q :call SendLineToR("newline")
inoremap <buffer> <silent> \o :call SendLineToRAndInsertOutput()
inoremap <buffer> <silent> \d :call SendLineToR("down")
inoremap <buffer> <silent> \l :call SendLineToR("stay")
inoremap <buffer> <silent> \pa :call SendParagraphToR("echo", "down")
inoremap <buffer> <silent> \pd :call SendParagraphToR("silent", "down")
inoremap <buffer> <silent> \pe :call SendParagraphToR("echo", "stay")
inoremap <buffer> <silent> \pp :call SendParagraphToR("silent", "stay")
inoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
inoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
inoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
inoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
inoremap <buffer> <silent> \ba :call SendMBlockToR("echo", "down")
inoremap <buffer> <silent> \bd :call SendMBlockToR("silent", "down")
inoremap <buffer> <silent> \be :call SendMBlockToR("echo", "stay")
inoremap <buffer> <silent> \bb :call SendMBlockToR("silent", "stay")
inoremap <buffer> <silent> \ks :call RSpin()
inoremap <buffer> <silent> \ao :call ShowRout()
inoremap <buffer> <silent> \ae :call SendFileToR("echo")
inoremap <buffer> <silent> \aa :call SendFileToR("silent")
inoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
inoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
inoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
inoremap <buffer> <silent> \xx :call RComment("normal")
inoremap <buffer> <silent> \rw :call RQuit('save')
inoremap <buffer> <silent> \rq :call RQuit('nosave')
inoremap <buffer> <silent> \rc :call StartR("custom")
inoremap <buffer> <silent> \rf :call StartR("R")
imap <buffer> <silent> _ :call ReplaceUnderS()
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal backupcopy=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=:#',:###,:##,:#
setlocal commentstring=#\ %s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'r'
setlocal filetype=r
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=cq
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=0
setlocal include=
setlocal includeexpr=
setlocal indentexpr=GetRIndent()
setlocal indentkeys=0{,0},:,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=@,48-57,_,.
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=rcomplete#CompleteR
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=4
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'r'
setlocal syntax=r
endif
setlocal tabstop=4
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal undolevels=-123456
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 197 - ((2 * winheight(0) + 24) / 48)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
197
normal! 09|
tabedit ../HIVBackCalc_App/development/model.R
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
vnoremap <buffer> <silent> \rd :call RSetWD()
vnoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
vnoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
vnoremap <buffer> <silent> \ro :call RObjBrowser()
vnoremap <buffer> <silent> \rb :call RAction("plotsumm")
vnoremap <buffer> <silent> \rg :call RAction("plot")
vnoremap <buffer> <silent> \rs :call RAction("summary")
vnoremap <buffer> <silent> \rh :call RAction("help")
vnoremap <buffer> <silent> \re :call RAction("example")
vnoremap <buffer> <silent> \ra :call RAction("args")
vnoremap <buffer> <silent> \rv :call RAction("viewdf")
vnoremap <buffer> <silent> \rt :call RAction("str")
vnoremap <buffer> <silent> \rn :call RAction("vim.names")
vnoremap <buffer> <silent> \rp :call RAction("print")
vnoremap <buffer> <silent> \rm :call RClearAll()
vnoremap <buffer> <silent> \rr :call RClearConsole()
vnoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
vnoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
vnoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
vnoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
vnoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
vnoremap <buffer> <silent> \; :call MovePosRCodeComment("selection")
vnoremap <buffer> <silent> \xu :call RSimpleCommentLine("selection", "u")
vnoremap <buffer> <silent> \xc :call RSimpleCommentLine("selection", "c")
vnoremap <buffer> <silent> \xx :call RComment("selection")
vnoremap <buffer> <silent> \rw :call RQuit('save')
vnoremap <buffer> <silent> \rq :call RQuit('nosave')
vnoremap <buffer> <silent> \rc :call StartR("custom")
vnoremap <buffer> <silent> \rf :call StartR("R")
nnoremap <buffer> <silent> \rd :call RSetWD()
onoremap <buffer> <silent> \rd :call RSetWD()
nnoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
onoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
nnoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
onoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
nnoremap <buffer> <silent> \ro :call RObjBrowser()
onoremap <buffer> <silent> \ro :call RObjBrowser()
nnoremap <buffer> <silent> \rb :call RAction("plotsumm")
onoremap <buffer> <silent> \rb :call RAction("plotsumm")
nnoremap <buffer> <silent> \rg :call RAction("plot")
onoremap <buffer> <silent> \rg :call RAction("plot")
nnoremap <buffer> <silent> \rs :call RAction("summary")
onoremap <buffer> <silent> \rs :call RAction("summary")
nnoremap <buffer> <silent> \rh :call RAction("help")
onoremap <buffer> <silent> \rh :call RAction("help")
nnoremap <buffer> <silent> \re :call RAction("example")
onoremap <buffer> <silent> \re :call RAction("example")
nnoremap <buffer> <silent> \ra :call RAction("args")
onoremap <buffer> <silent> \ra :call RAction("args")
nnoremap <buffer> <silent> \rv :call RAction("viewdf")
onoremap <buffer> <silent> \rv :call RAction("viewdf")
nnoremap <buffer> <silent> \rt :call RAction("str")
onoremap <buffer> <silent> \rt :call RAction("str")
nnoremap <buffer> <silent> \rn :call RAction("vim.names")
onoremap <buffer> <silent> \rn :call RAction("vim.names")
nnoremap <buffer> <silent> \rp :call RAction("print")
onoremap <buffer> <silent> \rp :call RAction("print")
nnoremap <buffer> <silent> \rm :call RClearAll()
onoremap <buffer> <silent> \rm :call RClearAll()
nnoremap <buffer> <silent> \rr :call RClearConsole()
onoremap <buffer> <silent> \rr :call RClearConsole()
nnoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
onoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
nnoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
onoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
nnoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
onoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
nnoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
onoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
nnoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
onoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
nnoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
onoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
nnoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
onoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
nnoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
onoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
nnoremap <buffer> <silent> \xx :call RComment("normal")
onoremap <buffer> <silent> \xx :call RComment("normal")
nnoremap <buffer> <silent> \rw :call RQuit('save')
onoremap <buffer> <silent> \rw :call RQuit('save')
nnoremap <buffer> <silent> \rq :call RQuit('nosave')
onoremap <buffer> <silent> \rq :call RQuit('nosave')
nnoremap <buffer> <silent> \rc :call StartR("custom")
onoremap <buffer> <silent> \rc :call StartR("custom")
nnoremap <buffer> <silent> \rf :call StartR("R")
onoremap <buffer> <silent> \rf :call StartR("R")
let s:cpo_save=&cpo
set cpo&vim
noremap <buffer> <silent> \r<Right> :call RSendPartOfLine("right", 0)
noremap <buffer> <silent> \r<Left> :call RSendPartOfLine("left", 0)
noremap <buffer> <silent> \o :call SendLineToRAndInsertOutput()
noremap <buffer> <silent> \d :call SendLineToR("down")
noremap <buffer> <silent> \l :call SendLineToR("stay")
noremap <buffer> <silent> \pa :call SendParagraphToR("echo", "down")
noremap <buffer> <silent> \pd :call SendParagraphToR("silent", "down")
noremap <buffer> <silent> \pe :call SendParagraphToR("echo", "stay")
noremap <buffer> <silent> \pp :call SendParagraphToR("silent", "stay")
vnoremap <buffer> <silent> \so :call SendSelectionToR("echo", "stay", "NewtabInsert")
vnoremap <buffer> <silent> \sa :call SendSelectionToR("echo", "down")
vnoremap <buffer> <silent> \sd :call SendSelectionToR("silent", "down")
vnoremap <buffer> <silent> \se :call SendSelectionToR("echo", "stay")
vnoremap <buffer> <silent> \ss :call SendSelectionToR("silent", "stay")
noremap <buffer> <silent> \ba :call SendMBlockToR("echo", "down")
noremap <buffer> <silent> \bd :call SendMBlockToR("silent", "down")
noremap <buffer> <silent> \be :call SendMBlockToR("echo", "stay")
noremap <buffer> <silent> \bb :call SendMBlockToR("silent", "stay")
noremap <buffer> <silent> \ks :call RSpin()
noremap <buffer> <silent> \ao :call ShowRout()
noremap <buffer> <silent> \ae :call SendFileToR("echo")
noremap <buffer> <silent> \aa :call SendFileToR("silent")
imap <buffer> <silent>  =RCompleteArgs()
inoremap <buffer> <silent> \rd :call RSetWD()
inoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
inoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
inoremap <buffer> <silent> \ro :call RObjBrowser()
inoremap <buffer> <silent> \rb :call RAction("plotsumm")
inoremap <buffer> <silent> \rg :call RAction("plot")
inoremap <buffer> <silent> \rs :call RAction("summary")
inoremap <buffer> <silent> \rh :call RAction("help")
inoremap <buffer> <silent> \re :call RAction("example")
inoremap <buffer> <silent> \ra :call RAction("args")
inoremap <buffer> <silent> \rv :call RAction("viewdf")
inoremap <buffer> <silent> \rt :call RAction("str")
inoremap <buffer> <silent> \rn :call RAction("vim.names")
inoremap <buffer> <silent> \rp :call RAction("print")
inoremap <buffer> <silent> \rm :call RClearAll()
inoremap <buffer> <silent> \rr :call RClearConsole()
inoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
inoremap <buffer> <silent> \r<Right> l:call RSendPartOfLine("right", 1)
inoremap <buffer> <silent> \r<Left> l:call RSendPartOfLine("left", 1)
inoremap <buffer> <silent> \q :call SendLineToR("newline")
inoremap <buffer> <silent> \o :call SendLineToRAndInsertOutput()
inoremap <buffer> <silent> \d :call SendLineToR("down")
inoremap <buffer> <silent> \l :call SendLineToR("stay")
inoremap <buffer> <silent> \pa :call SendParagraphToR("echo", "down")
inoremap <buffer> <silent> \pd :call SendParagraphToR("silent", "down")
inoremap <buffer> <silent> \pe :call SendParagraphToR("echo", "stay")
inoremap <buffer> <silent> \pp :call SendParagraphToR("silent", "stay")
inoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
inoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
inoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
inoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
inoremap <buffer> <silent> \ba :call SendMBlockToR("echo", "down")
inoremap <buffer> <silent> \bd :call SendMBlockToR("silent", "down")
inoremap <buffer> <silent> \be :call SendMBlockToR("echo", "stay")
inoremap <buffer> <silent> \bb :call SendMBlockToR("silent", "stay")
inoremap <buffer> <silent> \ks :call RSpin()
inoremap <buffer> <silent> \ao :call ShowRout()
inoremap <buffer> <silent> \ae :call SendFileToR("echo")
inoremap <buffer> <silent> \aa :call SendFileToR("silent")
inoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
inoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
inoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
inoremap <buffer> <silent> \xx :call RComment("normal")
inoremap <buffer> <silent> \rw :call RQuit('save')
inoremap <buffer> <silent> \rq :call RQuit('nosave')
inoremap <buffer> <silent> \rc :call StartR("custom")
inoremap <buffer> <silent> \rf :call StartR("R")
imap <buffer> <silent> _ :call ReplaceUnderS()
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal backupcopy=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=:#',:###,:##,:#
setlocal commentstring=#\ %s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'r'
setlocal filetype=r
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=cq
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=0
setlocal include=
setlocal includeexpr=
setlocal indentexpr=GetRIndent()
setlocal indentkeys=0{,0},:,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=@,48-57,_,.
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=rcomplete#CompleteR
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=4
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'r'
setlocal syntax=r
endif
setlocal tabstop=4
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal undolevels=-123456
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 1 - ((0 * winheight(0) + 24) / 48)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
tabedit ../HIVBackCalc_App/development/other.R
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
vnoremap <buffer> <silent> \rd :call RSetWD()
vnoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
vnoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
vnoremap <buffer> <silent> \ro :call RObjBrowser()
vnoremap <buffer> <silent> \rb :call RAction("plotsumm")
vnoremap <buffer> <silent> \rg :call RAction("plot")
vnoremap <buffer> <silent> \rs :call RAction("summary")
vnoremap <buffer> <silent> \rh :call RAction("help")
vnoremap <buffer> <silent> \re :call RAction("example")
vnoremap <buffer> <silent> \ra :call RAction("args")
vnoremap <buffer> <silent> \rv :call RAction("viewdf")
vnoremap <buffer> <silent> \rt :call RAction("str")
vnoremap <buffer> <silent> \rn :call RAction("vim.names")
vnoremap <buffer> <silent> \rp :call RAction("print")
vnoremap <buffer> <silent> \rm :call RClearAll()
vnoremap <buffer> <silent> \rr :call RClearConsole()
vnoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
vnoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
vnoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
vnoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
vnoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
vnoremap <buffer> <silent> \; :call MovePosRCodeComment("selection")
vnoremap <buffer> <silent> \xu :call RSimpleCommentLine("selection", "u")
vnoremap <buffer> <silent> \xc :call RSimpleCommentLine("selection", "c")
vnoremap <buffer> <silent> \xx :call RComment("selection")
vnoremap <buffer> <silent> \rw :call RQuit('save')
vnoremap <buffer> <silent> \rq :call RQuit('nosave')
vnoremap <buffer> <silent> \rc :call StartR("custom")
vnoremap <buffer> <silent> \rf :call StartR("R")
nnoremap <buffer> <silent> \rd :call RSetWD()
onoremap <buffer> <silent> \rd :call RSetWD()
nnoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
onoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
nnoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
onoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
nnoremap <buffer> <silent> \ro :call RObjBrowser()
onoremap <buffer> <silent> \ro :call RObjBrowser()
nnoremap <buffer> <silent> \rb :call RAction("plotsumm")
onoremap <buffer> <silent> \rb :call RAction("plotsumm")
nnoremap <buffer> <silent> \rg :call RAction("plot")
onoremap <buffer> <silent> \rg :call RAction("plot")
nnoremap <buffer> <silent> \rs :call RAction("summary")
onoremap <buffer> <silent> \rs :call RAction("summary")
nnoremap <buffer> <silent> \rh :call RAction("help")
onoremap <buffer> <silent> \rh :call RAction("help")
nnoremap <buffer> <silent> \re :call RAction("example")
onoremap <buffer> <silent> \re :call RAction("example")
nnoremap <buffer> <silent> \ra :call RAction("args")
onoremap <buffer> <silent> \ra :call RAction("args")
nnoremap <buffer> <silent> \rv :call RAction("viewdf")
onoremap <buffer> <silent> \rv :call RAction("viewdf")
nnoremap <buffer> <silent> \rt :call RAction("str")
onoremap <buffer> <silent> \rt :call RAction("str")
nnoremap <buffer> <silent> \rn :call RAction("vim.names")
onoremap <buffer> <silent> \rn :call RAction("vim.names")
nnoremap <buffer> <silent> \rp :call RAction("print")
onoremap <buffer> <silent> \rp :call RAction("print")
nnoremap <buffer> <silent> \rm :call RClearAll()
onoremap <buffer> <silent> \rm :call RClearAll()
nnoremap <buffer> <silent> \rr :call RClearConsole()
onoremap <buffer> <silent> \rr :call RClearConsole()
nnoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
onoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
nnoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
onoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
nnoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
onoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
nnoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
onoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
nnoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
onoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
nnoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
onoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
nnoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
onoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
nnoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
onoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
nnoremap <buffer> <silent> \xx :call RComment("normal")
onoremap <buffer> <silent> \xx :call RComment("normal")
nnoremap <buffer> <silent> \rw :call RQuit('save')
onoremap <buffer> <silent> \rw :call RQuit('save')
nnoremap <buffer> <silent> \rq :call RQuit('nosave')
onoremap <buffer> <silent> \rq :call RQuit('nosave')
nnoremap <buffer> <silent> \rc :call StartR("custom")
onoremap <buffer> <silent> \rc :call StartR("custom")
nnoremap <buffer> <silent> \rf :call StartR("R")
onoremap <buffer> <silent> \rf :call StartR("R")
let s:cpo_save=&cpo
set cpo&vim
noremap <buffer> <silent> \r<Right> :call RSendPartOfLine("right", 0)
noremap <buffer> <silent> \r<Left> :call RSendPartOfLine("left", 0)
noremap <buffer> <silent> \o :call SendLineToRAndInsertOutput()
noremap <buffer> <silent> \d :call SendLineToR("down")
noremap <buffer> <silent> \l :call SendLineToR("stay")
noremap <buffer> <silent> \pa :call SendParagraphToR("echo", "down")
noremap <buffer> <silent> \pd :call SendParagraphToR("silent", "down")
noremap <buffer> <silent> \pe :call SendParagraphToR("echo", "stay")
noremap <buffer> <silent> \pp :call SendParagraphToR("silent", "stay")
vnoremap <buffer> <silent> \so :call SendSelectionToR("echo", "stay", "NewtabInsert")
vnoremap <buffer> <silent> \sa :call SendSelectionToR("echo", "down")
vnoremap <buffer> <silent> \sd :call SendSelectionToR("silent", "down")
vnoremap <buffer> <silent> \se :call SendSelectionToR("echo", "stay")
vnoremap <buffer> <silent> \ss :call SendSelectionToR("silent", "stay")
noremap <buffer> <silent> \ba :call SendMBlockToR("echo", "down")
noremap <buffer> <silent> \bd :call SendMBlockToR("silent", "down")
noremap <buffer> <silent> \be :call SendMBlockToR("echo", "stay")
noremap <buffer> <silent> \bb :call SendMBlockToR("silent", "stay")
noremap <buffer> <silent> \ks :call RSpin()
noremap <buffer> <silent> \ao :call ShowRout()
noremap <buffer> <silent> \ae :call SendFileToR("echo")
noremap <buffer> <silent> \aa :call SendFileToR("silent")
imap <buffer> <silent>  =RCompleteArgs()
inoremap <buffer> <silent> \rd :call RSetWD()
inoremap <buffer> <silent> \r- :call g:RBrOpenCloseLs(0)
inoremap <buffer> <silent> \r= :call g:RBrOpenCloseLs(1)
inoremap <buffer> <silent> \ro :call RObjBrowser()
inoremap <buffer> <silent> \rb :call RAction("plotsumm")
inoremap <buffer> <silent> \rg :call RAction("plot")
inoremap <buffer> <silent> \rs :call RAction("summary")
inoremap <buffer> <silent> \rh :call RAction("help")
inoremap <buffer> <silent> \re :call RAction("example")
inoremap <buffer> <silent> \ra :call RAction("args")
inoremap <buffer> <silent> \rv :call RAction("viewdf")
inoremap <buffer> <silent> \rt :call RAction("str")
inoremap <buffer> <silent> \rn :call RAction("vim.names")
inoremap <buffer> <silent> \rp :call RAction("print")
inoremap <buffer> <silent> \rm :call RClearAll()
inoremap <buffer> <silent> \rr :call RClearConsole()
inoremap <buffer> <silent> \rl :call g:SendCmdToR("ls()")
inoremap <buffer> <silent> \r<Right> l:call RSendPartOfLine("right", 1)
inoremap <buffer> <silent> \r<Left> l:call RSendPartOfLine("left", 1)
inoremap <buffer> <silent> \q :call SendLineToR("newline")
inoremap <buffer> <silent> \o :call SendLineToRAndInsertOutput()
inoremap <buffer> <silent> \d :call SendLineToR("down")
inoremap <buffer> <silent> \l :call SendLineToR("stay")
inoremap <buffer> <silent> \pa :call SendParagraphToR("echo", "down")
inoremap <buffer> <silent> \pd :call SendParagraphToR("silent", "down")
inoremap <buffer> <silent> \pe :call SendParagraphToR("echo", "stay")
inoremap <buffer> <silent> \pp :call SendParagraphToR("silent", "stay")
inoremap <buffer> <silent> \fa :call SendFunctionToR("echo", "down")
inoremap <buffer> <silent> \fd :call SendFunctionToR("silent", "down")
inoremap <buffer> <silent> \fe :call SendFunctionToR("echo", "stay")
inoremap <buffer> <silent> \ff :call SendFunctionToR("silent", "stay")
inoremap <buffer> <silent> \ba :call SendMBlockToR("echo", "down")
inoremap <buffer> <silent> \bd :call SendMBlockToR("silent", "down")
inoremap <buffer> <silent> \be :call SendMBlockToR("echo", "stay")
inoremap <buffer> <silent> \bb :call SendMBlockToR("silent", "stay")
inoremap <buffer> <silent> \ks :call RSpin()
inoremap <buffer> <silent> \ao :call ShowRout()
inoremap <buffer> <silent> \ae :call SendFileToR("echo")
inoremap <buffer> <silent> \aa :call SendFileToR("silent")
inoremap <buffer> <silent> \; :call MovePosRCodeComment("normal")
inoremap <buffer> <silent> \xu :call RSimpleCommentLine("normal", "u")
inoremap <buffer> <silent> \xc :call RSimpleCommentLine("normal", "c")
inoremap <buffer> <silent> \xx :call RComment("normal")
inoremap <buffer> <silent> \rw :call RQuit('save')
inoremap <buffer> <silent> \rq :call RQuit('nosave')
inoremap <buffer> <silent> \rc :call StartR("custom")
inoremap <buffer> <silent> \rf :call StartR("R")
imap <buffer> <silent> _ :call ReplaceUnderS()
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal backupcopy=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=:#',:###,:##,:#
setlocal commentstring=#\ %s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'r'
setlocal filetype=r
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=cq
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=0
setlocal include=
setlocal includeexpr=
setlocal indentexpr=GetRIndent()
setlocal indentkeys=0{,0},:,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=@,48-57,_,.
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=rcomplete#CompleteR
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=4
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'r'
setlocal syntax=r
endif
setlocal tabstop=4
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal undolevels=-123456
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 368 - ((22 * winheight(0) + 24) / 48)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
368
normal! 09|
tabnext 5
set stal=1
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToO
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :