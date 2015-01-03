if &compatible
  set nocompatible
end

filetype off
set runtimepath+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

Plugin 'Konfekt/FastFold'               " optimized folding for large projects
Plugin 'Lokaltog/vim-easymotion'        " jump without numbers
Plugin 'Raimondi/delimitMate'           " auto insert closing delimiters
Plugin 'SirVer/ultisnips'               " snippets engine, integrates with YCM
Plugin 'Valloric/YouCompleteMe'         " syntax completion
Plugin 'airblade/vim-gitgutter'         " git diff in the gutter
Plugin 'bitc/vim-bad-whitespace'        " detect and highlight bad whitespace
Plugin 'christoomey/vim-tmux-navigator' " navigate with awarenes of vim splits
Plugin 'christoomey/vim-tmux-runner'    " send commands to tmxu pane
Plugin 'ciaranm/detectindent'           " detect indentation level
Plugin 'ecomba/vim-ruby-refactoring'    " keybindings for refactoring
Plugin 'ervandew/supertab'              " context-aware tab-completion
Plugin 'gavinbeatty/dragvisuals.vim'    " move visual blocks / lines around
Plugin 'gmarik/Vundle.vim'              " Let Vundler manage Vundle
Plugin 'godlygeek/tabular'              " align on a given regex (e.g., =)
Plugin 'honza/vim-snippets'             " textmate-style code snippets
Plugin 'janko-m/vim-test'               " runner for rspec, cucumber, et al.
Plugin 'jelera/vim-javascript-syntax'   " JavaScript Syntax definitions
Plugin 'jgdavey/tslime.vim'             " send specs to a designated tmux pane
Plugin 'jszakmeister/vim-togglecursor'  " toggle cursor between modes
Plugin 'kana/vim-textobj-user'          " custom text objects
Plugin 'kchmck/vim-coffee-script'       " CoffeeScript syntax defintions
Plugin 'kien/ctrlp.vim'                 " Rapid file finding
Plugin 'lervag/vim-latex'               " LaTeX support
Plugin 'maciakl/vim-neatstatus'         " lightweight native statusline
Plugin 'majutsushi/tagbar'              " navigate a list of methods / classes
Plugin 'marijnh/tern_for_vim'           " JS method jumping (like ctags)
Plugin 'moll/vim-node'                  " Node support
Plugin 'nelstrom/vim-textobj-rubyblock' " ruby block text objects
Plugin 'pbrisbin/vim-mkdir'             " create subdirectories as needed
Plugin 'rking/ag.vim'                   " Ag conveniences
Plugin 'sandeepcr529/Buffet.vim'        " user-friendly buffer list
Plugin 'scrooloose/syntastic'           " hook into syntax style checkers
Plugin 'sickill/vim-pasta'              " context-aware pasting
Plugin 'sjl/gundo.vim'                  " graphical undo tree
Plugin 'tpope/vim-bundler'              " Bundler commands
Plugin 'tpope/vim-endwise'              " add 'end' in ruby et al
Plugin 'tpope/vim-eunuch'               " Unix commands in vim
Plugin 'tpope/vim-fugitive'             " side-by-side git blame with :Gblame
Plugin 'tpope/vim-haml'                 " Haml support
Plugin 'tpope/vim-rails'                " Rails support
Plugin 'tpope/vim-repeat'               " repeat last mapped command with `.`
Plugin 'tpope/vim-surround'             " surround text block with delimiters
Plugin 'tpope/vim-unimpaired'           " useful complementary mappings
Plugin 'tpope/vim-vinegar'              " enhancements to netrw
Plugin 'vim-ruby/vim-ruby'              " Ruby support
Plugin 'vim-scripts/YankRing.vim'       " maintain yank/del history
Plugin 'vim-scripts/blockle.vim'        " toggle ruby block styles with <L>rtb
Plugin 'vim-scripts/c.vim'              " C completions
Plugin 'vim-scripts/matchit.zip'        " goto matching delimiter with %
Plugin 'vim-scripts/noerrmsg.vim'       " hide insert mode error messages
Plugin 'vim-scripts/tComment'           " line: ctrl+//  block: ctrl+/p
Plugin 'vitorgalvao/autoswap_mac'       " auto-respond to swap file message
Plugin 'wesQ3/vim-windowswap'           " move panes around with <Leader>ww
Plugin 'xolox/vim-easytags'             " continuously updated tags
Plugin 'xolox/vim-misc'                 " vim plugin utility library
Plugin 'xolox/vim-session'              " session management

call vundle#end()
filetype on

