# heredocs
A heredocument library supports shakespeare-like syntax.
<br><br>
Original: [cutsea110/heredoc](https://github.com/cutsea110/heredoc)

## Example
```haskell
{-# LANGUAGE QuasiQuotes #-}

import Text.Heredoc (heredoc)

main :: IO ()
main =
    let x = 42 in
        putStrLn [heredoc|x = ${show x}|] -- => x = 42
```

Since this library supports shakespeare-like syntax, please see [Yesod Web Framework Book](https://www.yesodweb.com/book/shakespearean-templates) for more informations.
