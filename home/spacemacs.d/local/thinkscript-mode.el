;;; package --- thinkscript-mode

;;; Commentary:

;; n/a

;;; Code:

(require 'generic-x)

(define-generic-mode 'thinkscript-mode
  '("#")
  '("above"
    "ago"
    "and"
    "bar"
    "bars"
    "below"
    "between"
    "case"
    "crosses"
    "declare"
    "def"
    "default"
    "do"
    "else"
    "equal"
    "equals"
    "false"
    "fold"
    "from"
    "greater"
    "if"
    "input"
    "is"
    "less"
    "no"
    "not"
    "or"
    "plot"
    "profile"
    "rec"
    "reference"
    "script"
    "switch"
    "than"
    "then"
    "to"
    "true"
    "while"
    "with"
    "within"
    "yes")
  '(("=" . 'font-lock-operator)
    (";" . 'font-lock-builtin))
  '(".thinkscript\\'")

  ;; other functions to call
  nil

  ;; Docstring
  "A major mode for thinkscript")

(provide 'thinkscript-mode)
;;; thinkscript-mode.el ends here
