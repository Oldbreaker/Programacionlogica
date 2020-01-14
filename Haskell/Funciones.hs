factorial n = if n < 2 then 1 else n * factorial (n-1)
Palindromo :: String -> Bool
Palindromo xs = xs == reverse xs
gauss n = n*(n+1)/2