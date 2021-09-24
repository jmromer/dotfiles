if [[ -z "${HOMEBREW_PREFIX}" ]]; then
  echo "WARNING: HOMEBREW_PREFIX env var is not set in ${0}"
fi

#-------------------------------------------------------------
# Compilation flags
#-------------------------------------------------------------
BUILD_VARS='CC CXX CFLAGS CPPFLAGS LDFLAGS PKG_CONFIG_PATH RUBY_CONFIGURE_OPTS RUBY_GC_MALLOC_LIMIT RUBY_GC_HEAP_FREE_SLOTS WARNFLAGS OPTFLAGS'

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

  OPTFLAGS: ${OPTFLAGS// -/\n-}

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

  if [ $# -eq 0 ]; then
    build_flags_set --quietly --all
  else
    build_flags_set "$@"
  fi
}

build_flags_set() {
  local quiet_mode

  while [ $# -gt -1 ]; do
    case "$1" in
      --all)
        set -- bzip2 g++-11 gcc-11 gettext icu4c imagemagick libffi libxml2 llvm ncurses no-warnings openssl optimize3 readline ruby xcrun zlib
        shift
        ;;
      bzip2)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/bzip2/lib"
        CFLAGS+=" -I${HOMEBREW_PREFIX}/opt/bzip2/include"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/bzip2/include"
        shift
        ;;
      gettext)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/gettext/lib"
        CFLAGS+=" -I${HOMEBREW_PREFIX}/opt/gettext/include"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/gettext/include"
        shift
        ;;
      icu4c)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/icu4c/lib"
        CFLAGS+=" -I${HOMEBREW_PREFIX}/opt/icu4c/include"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/icu4c/include"
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/icu4c/lib/pkgconfig"
        shift
        ;;
      libffi)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/libffi/lib"
        CFLAGS+=" -I${HOMEBREW_PREFIX}/opt/libffi/include"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/libffi/include"
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/libffi/lib/pkgconfig"
        shift
        ;;
      libxml2)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/libxml2/lib"
        CFLAGS+=" -I${HOMEBREW_PREFIX}/opt/libxml2/include"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/libxml2/include"
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/libxml2/lib/pkgconfig"
        shift
        ;;
      ncurses)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/ncurses/lib"
        CFLAGS+=" -I${HOMEBREW_PREFIX}/opt/ncurses/include"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/ncurses/include"
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/ncurses/lib/pkgconfig"
        shift
        ;;
      llvm)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/llvm/lib -Wl,-rpath,${HOMEBREW_PREFIX}/opt/llvm/lib"
        CFLAGS+=" -I${HOMEBREW_PREFIX}/opt/llvm/include"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/llvm/include"
        shift
        ;;
      openssl)
        OPENSSL="openssl@3"
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/${OPENSSL}/lib"
        CFLAGS+=" -I${HOMEBREW_PREFIX}/opt/${OPENSSL}/include"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/${OPENSSL}/include"
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/${OPENSSL}/lib/pkgconfig"
        RUBY_CONFIGURE_OPTS+=" --with-openssl-dir=${HOMEBREW_PREFIX}/opt/${OPENSSL}"
        shift
        ;;
      readline)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/readline/lib"
        CFLAGS+=" -I${HOMEBREW_PREFIX}/opt/readline/include"
        CPPFLAGS+=" -I${HOMEBREW_PREFIX}/opt/readline/include"
        PKG_CONFIG_PATH+=":${HOMEBREW_PREFIX}/opt/readline/lib/pkgconfig"
        RUBY_CONFIGURE_OPTS+=" --with-readline-dir=${HOMEBREW_PREFIX}/opt/readline"
        shift
        ;;
      xcrun)
        sdk_path="$(xcrun --show-sdk-path)"
        LDFLAGS+=" -L${sdk_path}/usr/lib"
        CFLAGS+=" -I${sdk_path}/usr/include"
        CPPFLAGS+=" -I${sdk_path}/usr/include"
        shift
        ;;
      zlib)
        LDFLAGS+=" -L${HOMEBREW_PREFIX}/opt/zlib/lib"
        CFLAGS+=" -I${HOMEBREW_PREFIX}/opt/zlib/include"
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
      clang++|g++*)
        CXX="$1"
        shift
        ;;
      clang|gcc*)
        CC="$1"
        shift
        ;;
      no-warnings)
        unset WARNFLAGS
        WARNFLAGS+=" -w"         # Inhibit warnings
        WARNFLAGS+=" -Wno-error" # Don't turn warnings into errors (override)
        shift
        ;;
      optimize*)
        unset OPTFLAGS
        local level="${1/optimize/}"

        if [[ -n "${level}" ]]; then
          # Optimization level 3 (max)
          # (NB: Can cause some builds to fail. Un-setting or lower can help.)
          OPTFLAGS+=" -O${level}"
        fi

        OPTFLAGS+=" -g" # Produce debugging information
        shift
        ;;
      --quietly|-q)
        quiet_mode=1
        shift
        ;;
      "")
        CFLAGS+=" -std=c99"
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

build_flags_set --quietly --all
