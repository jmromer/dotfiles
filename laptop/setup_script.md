Setup
======

What the script does
----------------------

NB: See the actual script

00. Clone this repo and link `.laptop.local`:

    ```sh
    git clone git@github.com:jkrmr/dotfiles.git ~/.dotfiles
    ln -s ~/.dotfiles/home/laptop.local ~/.laptop.local
    ```

00. Run thoughtbot's [laptop script](http://github.com/thoughtbot/laptop). Install with

    ```sh
    bash <(curl -s https://raw.githubusercontent.com/thoughtbot/laptop/master/mac) 2>&1 | tee ~/laptop.log
    ```

00. Install rcm: `brew tap thoughtbot/formulae   &&   brew install rcm`

00. Link into your home directory using `rcm`'s `rcup`:
    * Link into `~`: `env RCRC=$HOME/.dotfiles/rcrc rcup`
    * Create link to ssh config file:

    ```sh
    ln -s  ~/.dotfiles/sshconfig  ~/.ssh/config
    ```

[laptop-readme]: https://github.com/thoughtbot/laptop/blob/master/README.md
