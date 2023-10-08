alias k=kubectl

export now='--force --grace-period=0'
export gen='--dry-run=client -oyaml'

req() {
  kubectl run tmp --image=nginx:alpine --rm -i --restart=Never -- curl "${1}"
}
