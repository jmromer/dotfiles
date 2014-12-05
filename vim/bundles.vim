if &compatible
  set nocompatible
end

filetype off
set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

" Let Vundler manage Vundle
Plugin 'gmarik/Vundle.vim'

" User-interface and navigation enhancements
Plugin 'airblade/vim-gitgutter'        " git diff in the gutter
Plugin 'ervandew/supertab'             " context-aware tab-completion
Plugin 'jszakmeister/vim-togglecursor' " toggle cursor between modes
Plugin 'pbrisbin/vim-mkdir'            " create subdirectories as needed
Plugin 'Lokaltog/vim-easymotion'       " jump without numbers
Plugin 'kien/ctrlp.vim'                " Rapid file finding
Plugin 'sjl/gundo.vim'                 " graphical undo tree
Plugin 'tpope/vim-fugitive'            " side-by-side git blame with :Gblame
Plugin 'bling/vim-airline'             " nice, lightweight statusbar (readme)
Plugin 'majutsushi/tagbar'             " navigate a list of methods / classes
Plugin 'troydm/easybuffer.vim'         " user-friendly buffer list
Plugin 'wesQ3/vim-windowswap'          " move panes around with <Leader>ww
Plugin 'tpope/vim-eunuch'              " Unix commands in vim
Plugin 'tpope/vim-repeat'              " repeat last mapped command with `.`
Plugin 'vim-scripts/YankRing.vim'      " maintain yank/del history

" Text processing
Plugin 'godlygeek/tabular'       " align on a given regex (e.g., =)
Plugin 'tpope/vim-surround'      " surround text block with delimiters
Plugin 'sickill/vim-pasta'       " context-aware pasting
Plugin 'vim-scripts/matchit.zip' " goto matching delimiter with %
Plugin 'vim-scripts/tComment'    " ctr+// and ctrl+/p to comment line / block
Plugin 'Raimondi/delimitMate'    " auto insert closing delimiters

" Autocompletion
Plugin 'tpope/vim-endwise'       " add 'end' in ruby et al
Plugin 'Valloric/YouCompleteMe'  " syntax completion

" Code snippets
Plugin 'SirVer/ultisnips'        " snippets engine, integrates with YCM
Plugin 'honza/vim-snippets'      " textmate-style code snippets

" Custom text objects (dependent on vim-textoobj-user)
Plugin 'kana/vim-textobj-user'          " custom text objects
Plugin 'nelstrom/vim-textobj-rubyblock' " ruby block text objects

" tmux
Plugin 'christoomey/vim-tmux-navigator' " navigate with awarenes of vim splits
Plugin 'jgdavey/tslime.vim' " send spec run to a designated tmux pane

" documentation
Plugin 'rizzatti/dash.vim'

" dependency: vim-misc
Plugin 'xolox/vim-misc'     " vim plugin utility library
Plugin 'xolox/vim-easytags' " continuously updated tags
Plugin 'xolox/vim-session'  " session management

" linting: general
Plugin 'scrooloose/syntastic'    " hook into syntax style checkers
Plugin 'bitc/vim-bad-whitespace' " detect and highlight bad whitespace
Plugin 'ciaranm/detectindent'    " detect indentation level

" linting: language-specific
Plugin 'jelera/vim-javascript-syntax' " JavaScript
Plugin 'kchmck/vim-coffee-script'     " CoffeeScript
Plugin 'lervag/vim-latex'             " LaTeX
Plugin 'marijnh/tern_for_vim'         " JavaScript
Plugin 'Keithbsmiley/swift.vim'       " Swift
Plugin 'vim-ruby/vim-ruby'            " Ruby
Plugin 'tpope/vim-haml'               " Haml

" Frameworks
Plugin 'tpope/vim-bundler'    " Bundler
Plugin 'moll/vim-node'        " Node
Plugin 'tpope/vim-rails'      " Rails
Plugin 'thoughtbot/vim-rspec' " Rspec
Plugin 'tpope/vim-cucumber'   " Cucumber

" Ruby workflows
Plugin 'vim-scripts/blockle.vim'      " toggle ruby block styles with <L>tb
Plugin 'ecomba/vim-ruby-refactoring'  " keybindings for refactoring

"------------------ Probationary -------------------------- "

Plugin 'scrooloose/nerdtree'
let g:rspec_command = 'call Send_to_Tmux("rspec {spec}\n")'

"using spring:
" let g:rspec_command = 'call Send_to_Tmux("spring rspec {spec}\n")'

call vundle#end()
filetype on

