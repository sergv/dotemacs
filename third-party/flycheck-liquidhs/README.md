# flycheck-liquidhs.el

[LiquidHaskell](https://github.com/ucsd-progsys/liquidhaskell) Flycheck
integegration.

*Now uses `stack` by default* (to change the default behavior, change the
`flycheck-haskell-liquid-executable` variable through `M-x customize`).

## Usage

To enable, add this to your `init.el`

```
(require 'flycheck-liquidhs)
(add-hook 'haskell-mode-hook
          '(lambda () (flycheck-select-checker 'haskell-liquid)))

(add-hook 'literate-haskell-mode-hook
          '(lambda () (flycheck-select-checker 'haskell-liquid)))
```
