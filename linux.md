Steps to add to the bootstrap:

## Apt

```
sudo apt install nvidia-cuda-toolkit wget tmux vim gh
```

# keyboard delay and repeat interval
# ----------------------------------
gsettings set org.gnome.desktop.peripherals.keyboard delay 50
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 15

## input-remapper-2 [prefer Toshy]

https://github.com/sezanzeb/input-remapper
```
wget https://github.com/sezanzeb/input-remapper/releases/download/2.1.1/input-remapper-2.1.1.deb
sudo apt install -f ./input-remapper-2.1.1.deb
```

## Ulauncher

https://ulauncher.io

```
sudo add-apt-repository universe -y
sudo add-apt-repository ppa:agornostal/ulauncher -y
sudo apt update
sudo apt install ulauncher
```

## Emacs

Need to build with sqlite3 support (install with brew).

```
sudo add-apt-repository ppa:ubuntuhandbook1/emacs
sudo apt update
sudo apt install emacs emacs-gtk emacs-common
```

For vterm:
``` rb
sudo apt install libvterm-dev
```

## Toshy

Interferes with input remapper, might be causing instability.

https://github.com/RedBearAK/toshy

```
bash -c "$(curl -L https://raw.githubusercontent.com/RedBearAK/toshy/main/scripts/bootstrap.sh ||
 wget -O - https://raw.githubusercontent.com/RedBearAK/toshy/main/scripts/bootstrap.sh)"
```

## Vim

To build with clipboard support + GUI

```
sudo apt-get install libx11-dev libxt-dev libxpm-dev libgtk2.0-dev
brew install --build-from-source ~/.dotfiles/scripts/vim.rb
```

Relevant flags for reference:
```rb
# . . .
"--enable-gui=auto",
"--with-x",
"--with-feautures=huge",
# . . .
```

## Kitty

```
./lib/install-kitty
```

## Flatpak

```
flatpak install extensionmanager
```

## Extensions

Most not very useful:

- Hide Top Bar
- Just Perfection
- Logo Menu
- Open Bar
- Space Bar
- Tactile
- TopHat
- Undecorate Window
- User Themes

