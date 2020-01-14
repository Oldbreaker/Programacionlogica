   --ghc -o eliza eliza.hs
	module Main (main) where
	    import Data.Char
	    import System.IO  
	    import Data.List
	    import Data.List.Split
	    import System.Random
	    import System.Exit
	    import System.Directory
	    import System.IO.Unsafe
	    import qualified Data.Map as Map
	    type RawIRPair = ([String], [String])                      
	    main :: IO ()
	    main = do
	        putStrLn "\nOpciones: "
	        putStrLn "1.- Platicar"
	        putStrLn "2.- Aprender"
	        putStrLn "bye.- Salir"
	        putStrLn "\nIngrese solo el número:"
	        opcion <- getLine
	 
	        case opcion of
	          "1" -> do
	            ro <- do getStdGen
	            interact (conversation (randoms (ro)))
	            main
	          "2" -> do
	            fras2
	            main
	          "bye" -> do -- Respuesta no válida
	            putStrLn "Hasta luego!"
	            exitSuccess
	          _   -> do -- Respuesta no válida
	            putStrLn "Opción invalida."
	            main
	    {-fras :: IO()
	    fras = do
	            putStrLn "Frase nueva:"
	            inh <- openFile "r1.txt" ReadMode
	            todoItem <- getLine
	            putStrLn "Respuesta nueva:"
	            todoItemR <- getLine
	            --contents <- readFile "r1.txt"
	            contents <- hGetContents inh
	            hClose inh
	            outh <- openFile "r1.txt" WriteMode
	            --writeFile "r1.txt" (contents++"\n-"++todoItemR)
	            hPutStr outh (contents++"\n"++todoItem)
	            hClose outh
	            --inh2 <- openFile "r2.txt" ReadMode
	            contents2 <- readFile "r2.txt"
	            --writeFile "r2.txt" (contents2++"\n-"++contents2)
	            --contents2 <- hGetContents inh2
	            --hClose inh2
	            outh2 <- openFile "r2.txt" WriteMode
	            hPutStr outh2 (contents2++"\n-"++todoItemR)
	            hClose outh2
	            --contents <- readFile "r1.txt"
	            --putStr contents-}
	    fras2 :: IO()
	    fras2 = do
	            putStrLn "Frase nueva:"
	            todoItem <- getLine
	            putStrLn "Respuesta nueva:"
	            todoItemR <- getLine
	            contents <- readFile "r1.txt"
	            seq (length contents) (return ())
	            writeFile "r1.txt" ('a':(contents++"\n"++todoItem))
	            contents2 <- readFile "r2.txt"
	            seq (length contents2) (return ())
	            writeFile "r2.txt" ('a':(contents2++"\n-"++todoItemR))
	 
	 
	    le1 :: [RawIRPair]
	    le1 = unsafePerformIO lee
	 
	    lee ::IO [RawIRPair]
	    lee = do
	        contents <- readFile "r1.txt"
	        --inh <- openFile "r1.txt" ReadMode
	        --inh2 <- openFile "r2.txt" ReadMode
	        --contents <- hGetContents inh
	        --contents2 <- hGetContents inh2
	       
	        let pat = (map (splitOn ",") (lines contents))
	        contents2 <- readFile "r2.txt"
	        let resps = (map (splitOn "\n") (splitOn "-" contents2))
	               --print(zip pat resps)
	        let arr=(zip pat resps)::[RawIRPair]
	        --hClose inh2
	        --hClose inh
	        return arr
	 
	    changePerspective :: String -> String
	    changePerspective s = unwords (map sub (words s))
	        where
	          sub w = maybe w id (lookup w subData)
	   
	 
	   
	 
	 
	    {- Match a string to pattern. The pattern optionally contain a *
	      wildcard character which will match any number of characters. Match returns a
	      maybe which is Nothing if the string didn't match and a Just if it
	      did. The data for the Just is the part that was matched against the
	      wildcard character.-}
	 
	    match :: String -> String -> Maybe String
	    match pat@('*':rp) str = maybe (matchWildcard str) Just (match rp str)
	                             where matchWildcard (s:rs)= (fmap (\x -> s : x) (match pat rs))
	    match (p:rp) (s:rs)
	        | p == s    = match rp rs
	        | otherwise = Nothing
	    match [] [] = Just ""
	    match _ _ = Nothing
	 
	    {- matchSomewhere will keep on dropping letters off the front of the
	       string until it finds a match -}
	               
	    matchSomewhere :: String -> String -> Maybe String
	    matchSomewhere pat str@(s:rs) = maybe (matchSomewhere pat rs) Just (match pat str)
	    matchSomewhere _ _ = Nothing
	 
	    {- Try patterns from ruleData in order until one of them matches,
	       there's a wildcard at the end so at least one rule will always
	       match.
	       The matchingRules sub-function uses the List Monad to generate a
	       list of every match. However, because Haskell is lazy only the
	       first is every actually calcualted.
	       doSub substitues the part of the message that was matched by the
	       wildcard into the marked part of the response. changePerspective is
	       used to switch around the words so that "i" becomes "you" and so
	       on.
	     -}
	 
	    applyRules q = head matchingRules
	        where matchingRules = do (pats,resps) <- le1
	                                 pat <- pats
	                                 Just sub <- [matchSomewhere pat (" " ++ q)]
	                                 return $ map (doSub $ changePerspective sub) resps
	              doSub sub ('_' : resp) = sub ++ resp
	              doSub sub (c : resp)   = c : doSub sub resp
	              doSub sub []             = []
	 
	                             
	    
	 
	    conversation rands input = unlines (convo (lines input) rands)
	        where convo (line : rest) (r : randrest) =
	                  let answers = applyRules line in
	                   choice r answers : convo rest randrest
	              choice r xs = xs!!(r `mod` (length xs))
	 
	     
	    ----------
	    subData = [("i","you"),
	               ("you","me"),
	               ("i've","you've"),
	               ("you've","I've"),
	               ("i'm","you're"),
	               ("you're","I'm"),
	               ("i'd","you'd"),
	               ("you'd","I'd"),
	               ("am","are"),
	               ("are","am"),
	               ("was","were"),
	               ("were","was"),
	               ("my","your"),
	               ("your","my"),
	               ("me","you"),
	               ("you","me"),
	               ("mine","yours"),
	               ("yours","mine"),
	               ("myself","yourself"),
	               ("yourself","myself")]  
