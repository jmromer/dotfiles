alias k=kubectl

if [[ -n "${ZSH_VERSION}" ]]; then
  alias -g now='--force --grace-period=0'
  alias -g cfg='--dry-run=client --output=yaml'
elif [[ -n "${BASH_VERSION}" ]]; then
  export now='--force --grace-period=0'
  export cfg='--dry-run=client --output=yaml'
fi

function req() {
  kubectl run tmp --image=nginx:alpine --rm -i --restart=Never -- curl "${1}"
}
