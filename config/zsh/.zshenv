# |                            | Interactive login | Interactive non-login | Script |
# |---------------------------:|:-----------------:|:---------------------:|:------:|
# |                /etc/zshenv |         A         |           A           |    A   |
# |                  ~/.zshenv |         B         |           B           |    B   |
# |              /etc/zprofile |         C         |                       |        |
# |                ~/.zprofile |         D         |                       |        |
# |                 /etc/zshrc |         E         |           C           |        |
# |                   ~/.zshrc |         F         |           D           |        |
# |                /etc/zlogin |         G         |                       |        |
# |                  ~/.zlogin |         H         |                       |        |
# |   ~/.zlogout (logout only) |         I         |                       |        |
# | /etc/zlogout (logout only) |         J         |                       |        |

# dotfiles dir
export DOTFILES_DIR="${HOME}/.dotfiles"

source "${DOTFILES_DIR}/env/xdg.sh"
source "${DOTFILES_DIR}/env/asdf.sh"
source "${ASDF_DIR}/asdf.sh"

# Homebrew setup
case "$(uname -ps)" in
  Linux*)
    MACHINE="linux"
    HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew"
  ;;
  Darwin\ arm*)
    MACHINE="apple"
    HOMEBREW_PREFIX="/opt/homebrew"
  ;;
  Darwin*)
    MACHINE="intel-mac"
    HOMEBREW_PREFIX="/usr/local"
  ;;
esac

export MACHINE
export HOMEBREW_PREFIX
eval "$($HOMEBREW_PREFIX/bin/brew shellenv)"

# PATH setup
PATH="/usr/local/bin"
PATH+=":/usr/bin"
PATH+=":/bin"
PATH+=":/usr/local/sbin"
PATH+=":/usr/sbin"
PATH+=":/sbin"
PATH+=":${DOTFILES_DIR}/bin"
PATH+=":${DOTFILES_DIR}/share/asdf-versions/shims"
PATH+=":${DOTFILES_DIR}/share/asdf-manager/bin"
PATH+=":/Library/TeX/texbin"
export PATH

# disable to prevent /etc/profile from overwriting
# possible replacement: https://github.com/yb66/path_helper
if [ -x /usr/libexec/path_helper ]; then
  function noop () { }
  alias '/usr/libexec/path_helper'=noop
fi
