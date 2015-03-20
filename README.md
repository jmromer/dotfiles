dotfiles
=========

Installation
------------
00. Run `setup/setup`. Grab a sandwich, because it'll take a while. Stick around,
though, to enter your password when needed.

Features
--------

* A simple but informative Git prompt (all the pre-made ones I could find were
  either too busy or too uninformative for my purposes).
    - Shows the current branch name or commit id.
    - Color-coding by status:
      clean, clean but out of sync with remote, dirty, rebasing/merging.
* Right-side zsh prompt with current vi mode (normal or insert)
* `reload!` to source `~/.zshrc` or `~/.bash_profile`
* `dotfiles` to open dotfiles directories in an editor
* Safeguard aliases for destructive shell commands
* `desktop (show/hide)` to show/hide the Desktop (for Macs)
* `hidden_files (show/hide)` to show/hide hidden files (Macs)
* `update_homebrew` to run brew update, upgrade, cleanup, and doctor
* GitHub-flavored Git, courtesy of `hub`
* Annotated vimrc
