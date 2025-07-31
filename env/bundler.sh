[[ -z "${MACHINE_CORES}" ]] && echo "WARNING: MACHINE_CORES is not set in ${0}"

export BUNDLE_JOBS="${MACHINE_CORES}"
export BUNDLE_CONSOLE="pry"
export BUNDLE_GEM__COC="false"
export BUNDLE_GEM__MIT="true"
export BUNDLE_GEM__TEST="rspec"
