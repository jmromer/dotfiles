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

# Environment setup
# -----------------------------
export DOTFILES_DIR="${HOME}/.dotfiles"
source "${DOTFILES_DIR}/env/xdg.sh"

source "${DOTFILES_DIR}/env/asdf.sh"
source "${DOTFILES_DIR}/env/build.sh"
source "${DOTFILES_DIR}/env/fzf.sh"
source "${ASDF_DIR}/asdf.sh"


# PATH setup
PATH="${DOTFILES_DIR}/bin"

PATH+=":${XDG_DATA_HOME}/asdf-versions/shims"
PATH+=":${XDG_DATA_HOME}/asdf-manager/bin"
PATH+=":${XDG_DATA_HOME}/fzf/bin"
PATH+=":${XDG_DATA_HOME}/git/bin"
PATH+=":${XDG_CONFIG_HOME}/emacs/bin"

PATH+=":${HOMEBREW_PREFIX}/opt/coreutils/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/curl/bin"
PATH+=":${HOMEBREW_PREFIX}/opt/findutils/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/gettext/bin"
PATH+=":${HOMEBREW_PREFIX}/opt/gnu-bin/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/gnu-indent/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/gnu-sed/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/gnu-tar/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/gnu-which/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/gnutls/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/make/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/rg/bin"
PATH+=":${HOMEBREW_PREFIX}/share/git-core/contrib/diff-highlight"

PATH+=":${HOMEBREW_PREFIX}/bin"
PATH+=":${HOMEBREW_PREFIX}/sbin"
PATH+=":/usr/bin"
PATH+=":/bin"
PATH+=":/usr/sbin"
PATH+=":/sbin"

PATH+=":/Library/TeX/texbin"
export PATH

# disable to prevent /etc/profile from overwriting
# possible replacement: https://github.com/yb66/path_helper
if [ -x /usr/libexec/path_helper ]; then
  function noop () { }
  alias '/usr/libexec/path_helper'=noop
fi