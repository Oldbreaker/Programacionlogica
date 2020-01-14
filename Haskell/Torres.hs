data Poste = A | B | C deriving Show
movimientos :: Integer -> [(Integer,Poste,Poste)]
movimientos n = aux n  A B C
  where aux 1 a b c = [(1,a,c)]
        aux n a b c =  aux (n-1) a c b ++ (n,a,c):aux (n-1) b a c
 
hanoi :: Integer -> IO ()
hanoi x = do
  let xs = movimientos x
  aux xs
  where aux []           = return ()
        aux ((x,y,z):xs) = do
             putStrLn ("Mueve el disco " ++
                       show x ++
                       " de " ++
                       show y ++
                       " a " ++
                       show z)
             aux xs