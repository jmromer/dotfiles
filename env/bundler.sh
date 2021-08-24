if [[ -z "${MACHINE_CORES}" ]]; then
  echo "WARNING: MACHINE_CORES env var is not set in ${0}"
fi

export BUNDLE_JOBS="${MACHINE_CORES}"
export BUNDLE_CONSOLE="pry"
export BUNDLE_GEM__COC="false"
export BUNDLE_GEM__MIT="true"
export BUNDLE_GEM__TEST="rspec"
