module Token.Stemmer where

import qualified Data.Text as Text
import Control.Monad
import Data.Maybe
import Data.List
import Data.Text (Text)


{-|
  Porter 1 stemmer from
  https://tartarus.org/martin/PorterStemmer/

  Alternatives:
  eng-stemmer -- not on stack
  snowball -- need C stuff

  Maybe convert to all Text instead of Strings
-}


run :: [Text] -> [Text]
run =
    map ((Text.pack . stem) . Text.unpack)


stem :: String -> String
stem s
  | length s < 3 = s
  | otherwise    = allSteps s


allSteps :: String -> String
allSteps = step5 . step4 . step3 . step2 . step1


isConsonant :: String -> Int -> Bool
isConsonant str i
    | c `elem` "aeiou"  = False
    | c == 'y'          = i == 0 || isVowel str (i - 1)
    | otherwise         = True
    where
        c = str !! i


isVowel :: String -> Int -> Bool
isVowel = (not .) . isConsonant


byIndex :: Foldable t1 => (t1 a -> [Int] -> t2) -> t1 a -> t2
byIndex fun str = fun str [0..length str - 1]


measure :: String -> Int
measure = length . filter not . init . (True:) . map head . group . byIndex (map . isConsonant)


containsVowel :: String -> Bool
containsVowel = byIndex (any . isVowel)


endsWithDouble :: String -> Bool
endsWithDouble = startsWithDouble . reverse
    where
        startsWithDouble l | length l < 2 = False
                           | otherwise    = let (x:y:_) = l in x == y && x `notElem` "aeiou"


cvc :: String -> Bool
cvc word | length word < 3 = False
         | otherwise       = isConsonant word lastIndex       &&
                             isVowel     word (lastIndex - 1) &&
                             isConsonant word (lastIndex - 2) &&
                             last word `notElem` "wxy"
    where lastIndex = length word - 1


statefulReplace :: Eq a => ([a] -> Bool) -> [a] -> [a] -> [a] -> Maybe (Either [a] [a])
statefulReplace predicate str end replacement
    | end `isSuffixOf` str  = Just replaced
    | otherwise             = Nothing
    where
        part  = take (length str - length end) str
        replaced | predicate part = Right (part ++ replacement)
                 | otherwise      = Left str

replaceEnd :: Eq a => ([a] -> Bool) -> [a] -> [a] -> [a] -> Maybe [a]
replaceEnd predicate str end replacement = do
            result <- statefulReplace predicate str end replacement
            return (either id id result)


findStem :: Eq a => ([a] -> Bool) -> [a] -> [([a], [a])] -> Maybe [a]
findStem f word pairs = msum $ map (uncurry (replaceEnd f word)) pairs


measureGT :: Int -> String -> Bool
measureGT = flip ((>) . measure)


step1a :: String -> String
step1a word = fromMaybe word result
    where result = findStem (const True) word [("sses", "ss"), ("ies",  "i"), ("ss", "ss"), ("s", "")]


beforeStep1b :: String -> Either String String
beforeStep1b word = fromMaybe (Left word) result
    where
       cond23 x = do { v <- x; either (const Nothing) (return . Right) v }
       cond1  x = do { v <- x; return (Left v) }
       result =
           cond1  (replaceEnd (measureGT 0)  word "eed" "ee") `mplus`
           cond23 (statefulReplace containsVowel word "ed"  ""  ) `mplus`
           cond23 (statefulReplace containsVowel word "ing" ""  )


afterStep1b :: String -> String
afterStep1b word = fromMaybe word result
    where
        double        = endsWithDouble word && not (any ((`isSuffixOf` word) . return) "lsz")
        mEq1AndCvc    = measure word == 1 && cvc word
        iif cond val  = if cond then Just val else Nothing
        result        = findStem (const True) word [("at", "ate"), ("bl", "ble"), ("iz", "ize")]
                        `mplus` iif double (init word)
                        `mplus` iif mEq1AndCvc (word ++ "e")


step1b :: String -> String
step1b = either id afterStep1b . beforeStep1b


step1c :: String -> String
step1c word = fromMaybe word result
    where result = replaceEnd containsVowel word "y" "i"


step1 :: String -> String
step1 = step1c . step1b . step1a


step2 :: String -> String
step2 word = fromMaybe word result
    where
       result = findStem (measureGT 0) word
           [ ("ational", "ate" )
           , ("tional",  "tion")
           , ("enci",    "ence")
           , ("anci",    "ance")
           , ("izer",    "ize" )
           , ("bli",     "ble" )
           , ("alli",    "al"  )
           , ("entli",   "ent" )
           , ("eli",     "e"   )
           , ("ousli",   "ous" )
           , ("ization", "ize" )
           , ("ation",   "ate" )
           , ("ator",    "ate" )
           , ("alism",   "al"  )
           , ("iveness", "ive" )
           , ("fulness", "ful" )
           , ("ousness", "ous" )
           , ("aliti",   "al"  )
           , ("iviti",   "ive" )
           , ("biliti",  "ble" )
           , ("logi",    "log" ) ]


step3 :: String -> String
step3 word = fromMaybe word result
    where
       result = findStem (measureGT 0) word
           [ ("icate", "ic")
           , ("ative", ""  )
           , ("alize", "al")
           , ("iciti", "ic")
           , ("ical" , "ic")
           , ("ful"  , ""  )
           , ("ness" , ""  ) ]


step4 :: String -> String
step4 word = fromMaybe word result
    where
        gt1andST str = (measureGT 1) str && any ((`isSuffixOf` str) . return) "st"
        findGT1      = findStem (measureGT 1) word . map (flip (,) "")
        result       = (findGT1 ["al", "ance", "ence", "er", "ic", "able", "ible", "ant", "ement", "ment", "ent"]) `mplus`
                       (findStem gt1andST word [("ion","")]) `mplus`
                       (findGT1 ["ou", "ism", "ate", "iti", "ous", "ive", "ize"])


step5a :: String -> String
step5a word = fromMaybe word result
    where
        test str = (measureGT 1 str) || ((measure str == 1) && (not $ cvc str))
        result   = replaceEnd test word "e" ""


step5b :: String -> String
step5b word = fromMaybe word result
    where
       cond s = last s == 'l' && measureGT 1 s
       result = replaceEnd cond word "l" ""


step5 :: String -> String
step5 = step5b . step5a
