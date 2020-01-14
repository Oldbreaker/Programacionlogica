module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------
type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

--------------------------------------------------------
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind botBrain = 
  do 
    r <- randomIO :: IO Float
    return (rulesApply $ (map . map2) (id, pick r) botBrain)
  

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply phrasePair phrase =
  concat $ transformationsApply "*" reflect phrasePair phrase


reflect :: Phrase -> Phrase
reflect (s:slist) =
  [ if result /= [] then snd . head $ result else s | s<-(s:slist),
  let result = filter((==s).fst) reflections ]


reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile [] = []
rulesCompile (x:xs) =
  ( words . map toLower $ fst x, [ words $ y | y<-(snd x) ] ) : rulesCompile xs


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions


reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply phrasePair = 
  fix $ try $ transformationsApply "*" id phrasePair

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wildcard (t:tail) s =
  concatMap (\t -> if t == wildcard then s else [t]) (t:tail)


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [][] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wildcard (p:ps) (s:ss)
  -- Case when lists are equal
  | (p:ps) == (s:ss)                    = Just []
  -- Case when wildcard is not in pattern (p:ps)
  | not $ wildcard `elem` (p:ps)        = Nothing
  -- When (p:ps) consists of only the wildcard
  | wildcard == p && length (p:ps) == 1 = Just (s:ss)
  -- Case with multiple wildcards 
  | head ps == s && ps /= ss && length ps == length ss = Just []
  -- Case when wildcard is not the first element of p
  -- and head elemnts are equal
  | wildcard /= p && p == s            = match wildcard ps ss
  -- If wildcard is the first element of pattern
  | wildcard == p                      = orElse (singleWildcardMatch (p:ps) (s:ss))
                                          $ longerWildcardMatch (p:ps) (s:ss)
  | otherwise                          = Nothing



-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs)
  | ps == [x | x <- xs, x /= wc]                 = Just [x]
  | length ps == length xs && ps !! 0 == xs !! 0 = Just [x]
  | otherwise                                    = Nothing


longerWildcardMatch (wc:ps) (x:xs)
  | ps == xs  = Nothing
  | otherwise = mmap (x:) $ match wc (wc:ps) xs




-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply _ _ [] _       = Nothing
transformationApply _ _ _ ([],[])  = Nothing
transformationApply wildcard f orig present 
  | justResult /= Nothing = Just (substitute wildcard second $ f matchResult)
  | otherwise = Nothing
  where (first, second) = present
        justResult = match wildcard first orig
        matchResult = fromJust(justResult)


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply _ _ _ [] = Nothing
transformationsApply wildcard f (pres:plist) orig =
  orElse (transformationApply wildcard f orig pres)
  $ transformationsApply wildcard f plist orig
