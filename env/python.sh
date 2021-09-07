export PYTHONDONTWRITEBYTECODE=1

# Activate virtualenv if running python from within vim in a virtualenv
# http://vi.stackexchange.com/a/7654
if [[ -n "${VIRTUAL_ENV}" && -e "${VIRTUAL_ENV}/bin/activate" ]]; then
  source "${VIRTUAL_ENV}/bin/activate"
fi
