if [[ -z "${HOMEBREW_PREFIX}" ]]; then
  echo "WARNING: HOMEBREW_PREFIX env var is not set in ${0}"
fi

#-------------------------------------------------------------
# Compilation flags
#-------------------------------------------------------------
BUILD_VARS='CC CXX CFLAGS CPPFLAGS LDFLAGS PKG_CONFIG_PATH RUBY_CONFIGURE_OPTS RUBY_GC_MALLOC_LIMIT RUBY_GC_HEAP_FREE_SLOTS WARNFLAGS'

build_flags_unset() {
  for var in $(echo "$BUILD_VARS"); do
    unset "$var"
  done
}

build_flags_export() {
  for var in $(echo "$BUILD_VARS"); do
    export "$var"
  done
}

build_flags_inspect() {
  echo """
  CC: ${CC}

  CXX: ${CXX}

  WARNFLAGS: ${WARNFLAGS// -/\n-}

  CFLAGS: ${CFLAGS// -I/\n-I}

  CPPFLAGS: ${CPPFLAGS// -I/\n-I}

  LDFLAGS: ${LDFLAGS// -L/\n-L}

  PKG_CONFIG_PATH: ${PKG_CONFIG_PATH//:/\n}

  RUBY_CONFIGURE_OPTS: ${RUBY_CONFIGURE_OPTS// -/\n-}
  RUBY_GC_MALLOC_LIMIT: ${RUBY_GC_MALLOC_LIMIT}
  RUBY_GC_HEAP_FREE_SLOTS: ${RUBY_GC_HEAP_FREE_SLOTS}
  """ | sed -E 's/^ +//g' | less
}

build_flags_reset() {
  build_flags_unset
  build_flags_set "$@"
}

build_flags_set() {
  local quiet_mode

  while [ $# -gt -1 ]; do
    case "$1" in
      gettext)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/gettext/lib"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/gettext/include"
        shift
        ;;
      icu4c)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/icu4c/lib"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/icu4c/include"
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/icu4c/lib/pkgconfig"
        shift
        ;;
      libffi)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/libffi/lib"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/libffi/include"
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/libffi/lib/pkgconfig"
        shift
        ;;
      libxml2)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/libxml2/lib"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/libxml2/include"
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/libxml2/lib/pkgconfig"
        shift
        ;;
      ncurses)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/ncurses/lib"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/ncurses/include"
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/ncurses/lib/pkgconfig"
        shift
        ;;
      llvm)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/llvm/lib -Wl,-rpath,${HOMEBREW_PREFIX}/opt/llvm/lib"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/llvm/include"
        shift
        ;;
      openssl)
        OPENSSL="openssl@1.1"
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/openssl@1.1/lib"
        CFLAGS+=" -I${HOMEBREW_PREFIX}/opt/openssl@1.1/include"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/openssl@1.1/include"
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/openssl@1.1/lib/pkgconfig"
        RUBY_CONFIGURE_OPTS+=" --with-openssl-dir=${HOMEBREW_PREFIX}/opt/openssl@1.1"
        shift
        ;;
      readline)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/readline/lib"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/readline/include"
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/readline/lib/pkgconfig"
        RUBY_CONFIGURE_OPTS+=" --with-readline-dir=${HOMEBREW_PREFIX}/opt/readline"
        shift
        ;;
      zlib)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/zlib/lib"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/zlib/include"
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/zlib/lib/pkgconfig"
        shift
        ;;
      imagemagick)
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/imagemagick/lib/pkgconfig"
        shift
        ;;
      ruby)
        RUBY_CONFIGURE_OPTS+=" --enable-shared"
        RUBY_CONFIGURE_OPTS+=" --disable-libedit"
        RUBY_GC_MALLOC_LIMIT=60000000
        RUBY_GC_HEAP_FREE_SLOTS=200000
        shift
        ;;
      c)
        CFLAGS+=" -O3 -g"
        shift
        ;;
      clang++|g++*)
        CXX="$1"
        shift
        ;;
      clang|gcc*)
        CC="$1"
        shift
        ;;
      --quietly|-q)
        quiet_mode=1
        shift
        ;;
      "")
        WARNFLAGS=" -Wno-error=implicit-function-declaration -Wno-expansion-to-defined"
        CFLAGS+="${WARNFLAGS}"
        build_flags_export
        [ ! $quiet_mode ] && build_flags_inspect
        return
        ;;
      *)
        echo "Unrecognized: $1"
        shift
        ;;
    esac
  done
}

build_flags_set \
  --quietly \
  gettext \
  icu4c \
  imagemagick \
  libffi \
  libxml2 \
  llvm \
  ncurses \
  openssl \
  readline \
  ruby \
  zlib \
  gcc-11 \
  g++-11
