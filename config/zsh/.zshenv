# Homebrew setup
# -----------------------------
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

if command -v /usr/sbin/sysctl >/dev/null; then
  MACHINE_CORES=$(echo "$(/usr/sbin/sysctl -n hw.ncpu) / 2" | bc)
else
  MACHINE_CORES=8
  echo "Warning: MACHINE_CORES set to default value of ${MACHINE_CORES}."
fi

export MACHINE
export MACHINE_CORES
export HOMEBREW_PREFIX

eval "$($HOMEBREW_PREFIX/bin/brew shellenv)"

# XDG setup
# -----------------------------
export DOTFILES_DIR="${HOME}/.dotfiles"
source "${DOTFILES_DIR}/env/xdg.sh"

# Environment setup
# -----------------------------
source "${DOTFILES_DIR}/env/asdf.sh"
source "${DOTFILES_DIR}/env/build.sh"
source "${DOTFILES_DIR}/env/fzf.sh"
source "${DOTFILES_DIR}/env/bundler.sh"
source "${DOTFILES_DIR}/env/gpg.sh"
source "${ASDF_DIR}/asdf.sh"


# PATH setup
# -----------------------------
PATH="${DOTFILES_DIR}/bin"

PATH+=":${XDG_DATA_HOME}/asdf-versions/shims"
PATH+=":${XDG_DATA_HOME}/asdf-manager/bin"
PATH+=":${XDG_DATA_HOME}/fzf/bin"
PATH+=":${XDG_DATA_HOME}/git/bin"
PATH+=":${XDG_DATA_HOME}/emacs/bin"

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

# Shell config execution order
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

# disable to prevent /etc/profile from overwriting
# possible replacement: https://github.com/yb66/path_helper
if [ -x /usr/libexec/path_helper ]; then
  function noop () { }
  alias '/usr/libexec/path_helper'=noop
fi
