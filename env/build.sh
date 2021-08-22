if [[ -z "${HOMEBREW_PREFIX}" ]]; then
  echo "WARNING: HOMEBREW_PREFIX env var is not set in ${0}"
fi

#-------------------------------------------------------------
# Compilation flags
#-------------------------------------------------------------
export CC=gcc-11
export CXX=g++-11

LDFLAGS="  -L${HOMEBREW_PREFIX}/opt/gettext/lib"
LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/libffi/lib"
LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/libxml2/lib"
LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/llvm/lib -Wl,-rpath,${HOMEBREW_PREFIX}/opt/llvm/lib"
LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/ncurses/lib"
LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/openssl/lib"
LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/icu4c/lib"
LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/readline/lib"
LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/zlib/lib"
export LDFLAGS

CPPFLAGS="-I${HOMEBREW_PREFIX}/opt/gettext/include"
CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/libxml2/include"
CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/llvm/include"
CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/ncurses/include"
CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/openssl/include"
CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/icu4c/include"
CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/readline/include"
CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/zlib/include"
export CPPFLAGS

PKG_CONFIG_PATH="${HOMEBREW_PREFIX}/opt/libffi/lib/pkgconfig"
PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/libxml2/lib/pkgconfig"
PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/ncurses/lib/pkgconfig"
PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/openssl/lib/pkgconfig"
PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/icu4c/lib/pkgconfig"
PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/readline/lib/pkgconfig"
PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/zlib/lib/pkgconfig"
PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/imagemagick/lib/pkgconfig"
export PKG_CONFIG_PATH

RUBY_CONFIGURE_OPTS="--with-readline-dir=${HOMEBREW_PREFIX}/opt/readline"
RUBY_CONFIGURE_OPTS+=" --with-openssl-dir=${HOMEBREW_PREFIX}/opt/openssl"
RUBY_CONFIGURE_OPTS+=" --enable-shared"
RUBY_CONFIGURE_OPTS+=" --disable-libedit"
export RUBY_CONFIGURE_OPTS

export CFLAGS="-O3 -g -I${HOMEBREW_PREFIX}/opt/openssl/include"
export RUBY_CFLAGS="-march=native -Os"
export RUBY_GC_MALLOC_LIMIT=60000000
export RUBY_GC_HEAP_FREE_SLOTS=200000
