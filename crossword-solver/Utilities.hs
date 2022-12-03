module Utilities where

import Data.List
import Debug.Trace
import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as Map

import Types
import Constants

fastNub xs
  = nub' (sort xs)
  where
    nub' (x : xs@(y : _)) 
      = if x == y then xs' else x : xs'
      where
        xs' = nub' xs
    nub' xs
      = xs

lookUp x t
  = fromMaybe [](Prelude.lookup x t)

mlookUp x t
  = fromMaybe [] (Map.lookup x t)

isSingleton [_]
  = True
isSingleton _
  = False

cartesianAppend [xs] 
  = xs
cartesianAppend (xs : xss)
  = [x ++ y | x <- xs, y <- cartesianAppend xss]

isSubstring s s'
  = or [take (length s) t == s | t <- tails s']

hasTwoLettersOrMore :: String -> Bool
hasTwoLettersOrMore ""
  = False
hasTwoLettersOrMore [_]
  = False
hasTwoLettersOrMore _
  = True

isPalindrome s@(_ : _ : _)
  = isPalindrome' s (reverse s)
  where
    isPalindrome' "" ""
      = True
    isPalindrome' (c : cs) (c' : cs')
      = c == c' && isPalindrome' cs cs'
    isPalindrome' _ _
      = False
isPalindrome _
  = True

--
-- s' may contain spaces -- it may the the result of a charade
-- evaluation.
--
haveSameLetters :: String -> String -> Bool
haveSameLetters s s'
  = haveSameLetters' s (removeSpaces s')
  where
    haveSameLetters' s s' 
      = s /= s' && sameLetters s s'
      where
        sameLetters s s'
          = sort s == sort s'

removeSpaces
  = filter (/=' ')

removeHyphens
  = filter (/='-')

removeApostrophe 
  = filter (/= '\'') 

removeApostropheS ['\'', 's']
  = []
removeApostropheS (c : cs)
  = c : removeApostropheS cs
removeApostropheS ""
  = ""

prefixes 
  = tail . inits

suffixes s
  = take (length s) (iterate tail s)

substrings :: [a] -> [[a]]
substrings s
  = [i | t <- tails s, i <- tail (inits t)]

substringIndices :: Index -> [Index]
substringIndices (m, n)
  = concat [[(i, j) | j <- [i..n]] | i <- [m..n]]

partitions' [] 
  = [[]]
partitions' (x:xs) 
  = [[x] : p | p <- ps] ++
    [(x : ys) : yss | (ys : yss) <- ps]
  where
    ps = partitions' xs

splitOnPrefixes :: [String] -> String -> [(String, String)]
splitOnPrefixes prefixes s
  = concatMap split prefixes
  where
    split ind
      | pre == ind = [(pre, post)]
      | otherwise  = []
      where
        len = length ind
        (pre, post) = splitAt len s

--
-- Functions involving indices
--
indexList :: Index -> [a] -> [a]
indexList (m, n) xs
  = take (n - m + 1) (drop m xs)

partitionsIndices :: Index -> [[Index]]
partitionsIndices (m, n)
  = map (map makeIndex) (partitions [m..n])
  where
    makeIndex xs = (head xs, last xs)

partitions :: [a] -> [[[a]]]
partitions 
  = init . partitions'

extractMiddle (x : xs)
  = (x, middle, head y)
  where
    n = length xs
    (middle, y) = splitAt (n - 1) xs

--
-- N-way split optimised for constraints
-- Neither list can be empty.
--
splitNopt :: [a] -> [Bounds] -> [[[a]]]
splitNopt xs bs
  = makeSplits xs bs (scanr add (0, 0) (tail bs))
  where
    add :: Bounds -> Bounds -> Bounds
    add (x, y) (x', y') 
      = (x + x', y + y')
    makeSplits :: [a] -> [Bounds] -> [Bounds] -> [[[a]]]
    makeSplits xs (b : bs) (b' : bs')
      =  [xs' : res | (xs', ys') <- split2mn xs b b',
                     res <- makeSplits ys' bs bs']
    makeSplits _ _ _
      = [[]]

getSplitLengths :: Int -> [Bounds] -> [[Int]]
getSplitLengths len [_]
  = [[len]]
getSplitLengths len bs
  = makeSplits len bs (scanr add (0, 0) (tail bs))
  where
    add :: Bounds -> Bounds -> Bounds
    add (x, y) (x', y') 
      = (x + x', y + y')
    makeSplits :: Int -> [Bounds] -> [Bounds] -> [[Int]]
    makeSplits len (b : bs) (b' : bs')
      =  [k : rest | k <- get2waySplitLengths len b b',
                     rest <- makeSplits (len - k) bs bs']
    makeSplits _ _ _
      = [[]]

get2waySplitLengths :: Int -> Bounds -> Bounds -> [Int]
get2waySplitLengths len (m, n) (m', n')
  = [k | k <- [max m (len - n') .. min n (len - m')]]

--
-- The algorithm here is used in forwards charade solving, but
-- the function isn't directly used. Returns the first element of the
-- Bounds list, adjusted to take account of the target bounds (b).
-- E.g. makeBoundsOpt (6,10) [(1,9),(2,7),(2,6)] -> (2,6).
--
makeBoundsOpt :: Bounds -> [Bounds] -> Bounds
makeBoundsOpt b bs
  = makeBounds b bs (scanr add (0, 0) (tail bs))
  where
    add :: Bounds -> Bounds -> Bounds
    add (x, y) (x', y') 
      = (x + x', y + y')
    makeBounds :: Bounds -> [Bounds] -> [Bounds] -> Bounds
    makeBounds (l, u) ((l', u') : bs) ((lacc, uacc) : bsacc)
      | l' + lacc > u || u' + uacc < l 
        = (0, 0)
      | otherwise 
        = (min u' (max l' (l - uacc)), min u' (u - lacc))
        

split2mn :: [a] -> Bounds -> Bounds -> [([a], [a])]
split2mn xs (m, n) (m', n')
  = [splitAt k xs | 
      k <- [max m (len - n') .. min n (len - m')]]
  where
    len = length xs

--
-- Used to split text for insertions.
-- List must have at least 2 elements
-- (l', u') bounds the OUTER two lists returned.
-- (l, u) bounds the MIDDLE element.
-- The max and min expressions do the same as the 
-- comented-put list comp above them.
--
split3mn :: [a] -> Bounds -> Bounds -> [([a], [a], [a])]
split3mn xs (l, u) (l', u')
  = [(pre, mid, post') |
--      m <- [l .. u], n <- [l' .. u'], m + n == len,
      m <- [max l (len - u') .. min u (len - l')],
      let n = len - m,
      i <- [1 .. n - 1], 
      let (pre, post) = splitAt i xs,
      let (mid, post') = splitAt m post]
  where
    len = length xs

mirror2 :: [(a, a)] -> [(a, a)]
mirror2 xs
  = xs ++ [(y, x) | (x, y) <- xs]

mirror3 :: [(a, a, a)] -> [(a, a, a)]
mirror3 xs
  = xs ++ [(z, y, x) | (x, y, z) <- xs]

--
-- Needed by forwards solver (insertion)
--
split3 :: [a] -> [([a], [a], [a])]
split3 xs
  = [(take n xs, ys, zs) | n <- [1..length xs - 1], (ys, zs) <- split2 (drop n xs)]

split2 :: [a] -> [([a], [a])]
split2 xs
  = [splitAt k xs | k <- [1..length xs - 1]]

split2' :: [a] -> [([a], [a])]
split2' xs
  = sp2 ++ [(s2, s1) | (s1, s2) <- sp2]
  where
    sp2 = split2 xs

split2Indices :: Index -> [(Index, Index)]
split2Indices (m, n)
  = [((m, i), (i + 1, n)) | i <- [m .. n - 1]]

split2Indices' :: Index -> [(Index, Index)]
split2Indices' 
  = mirror2 . split2Indices

split3Indices :: Index -> [(Index, Index, Index)]
split3Indices (m, n)
  = [((m, i), (i + 1, j), (j + 1, n)) | 
      i <- [m .. n - 2], j <- [i + 1 .. n - 1]]

-- Note: This is only needed in top-level definitions, as
-- the 3-argument operators explicitly enumerate the six 
-- variants (L, C, R), (1, 2).
split3Indices' :: Index -> [(Index, Index, Index)]
split3Indices'
  = mirror3 . split3Indices

-- Need in order to parse 'distfix' operators, e.g.
-- "putting X into Y", "X with Y removed" etc.
split4Indices :: Index -> [(Index, Index, Index, Index)]
split4Indices (m, n)
  = [((m, j' - 1), ix, ix', ix'') | 
      i <- [m .. n - 3], 
      (ix@(j', _), ix', ix'') <- split3Indices (i + 1, n)]

--
-- Functions on strings
-- We keep apostrophes in...
--
cleanUp :: String -> String
cleanUp s
  = filter (not . flip elem removeChars) ((replaceQuotes . replaceHyphens) (map toLower s))

replaceQuotes s
  = unwords (map remove (words s))
  where
    remove "" = ""
    remove ('\'' : s) = remove s
    remove w
      | head rw == '\'' = reverse (tail rw)
      | otherwise = w
      where
         rw = reverse w

replaceHyphens ""
  = ""
replaceHyphens (' ' : '-' : '-' : ' ' : s)
  = ' ' : replaceHyphens s
replaceHyphens (' ' : '-' : ' ' : s)
  = ' ' : replaceHyphens s
replaceHyphens (c : s)
  = c : replaceHyphens s

removeChars = "()\".,:;?!"

-- 
-- s comes out first in the list, and we don't want it,
-- hence tail.
--
anagrams :: String -> [String]
anagrams s
  = tail (anagrams' s )
  where
    anagrams' []
      = [[]]
    anagrams' s
      = [c : s' | c <- s, s' <- anagrams' (s \\ [c])]

insertIntoMiddle s s'
  = [pre : s'' ++ [post] | s'' <- insertInto s mid]
  where
    (pre, mid, post) = extractMiddle s'

insertInto s []
  = [s]
insertInto s str@(x : xs)
  = (s ++ str) : map (x :) (insertInto s xs)

-- 
-- Removes every instance of xs from ys
--
subtractFrom :: Eq a => [a] -> [a] -> [[a]]
subtractFrom xs ys
  = subtractFrom' xs ys False
  where
    subtractFrom' list@(x : xs) (y : ys) matching
      | (x == y)  = (if matching then [] else rest) ++ 
                    subtractFrom' xs ys True
      | otherwise = rest
      where
        rest = [y : zs | zs <- subtractFrom' list ys False] 
    subtractFrom' [] ys@(_ : _) _
      = [ys]
    subtractFrom' (_ : _) ys _
      = []
    subtractFrom' [] [] _
      = [[]]
  
addWildcards :: [(Int, Int)] -> String -> String
addWildcards [] s
  = s
addWildcards ((i, j) : is) s
  = pre ++ replicate j '*' ++ addWildcards is post
  where
    (pre, post) = splitAt i s

removeWildcards :: [(Int, Int)] -> String -> String
removeWildcards [] s
  = s
removeWildcards ((i, j) : is) s
  = take i s ++ removeWildcards is (drop (i + j) s)

extractWildcardText [] s
  = []
extractWildcardText ((i, j) : is) s
  = take j (drop i s) ++ extractWildcardText is (drop (i + j) s)

--
-- The algorithm here is used in the forwards evaluation of 
-- subtext, although we don't use the function directly.
-- If we want words of length 6 ((m, n) = (6,6)) from a charade
-- tree with 3 nodes, then the allowable sublengths are:
-- makeLengths 3 (6,6) 1 = [[1,1,4],[1,2,3],[1,3,2],[1,4,1],
-- [2,1,3],[2,2,2],[2,3,1],[3,1,2], [3,2,1],[4,1,1]]
--
makeLengths 1 (m, n) minLen
  = [[i] | i <- [max 1 m .. n]]
makeLengths k (m, n) minLen
  = [i : rest | i <- [minLen .. n - (k - 1) * minLen],
                rest <- makeLengths (k - 1) (m - i, n - i) minLen]

--
-- These are needed when taking out under an anagram. We have
-- to solve 'forwards' in those cases.
-- We don't want spaces to appear. E.g. if there is a 
-- synonym of "large animal", say then "lar" is NOT a valid
-- first-letters take-out. That will be handled (and rejected)
-- by a charade of large and animal in another parse.
--
firstLettersGen :: String -> (Int, Int) -> [(Int, String)]
firstLettersGen s (m, n) 
  | elem ' ' s = []
  | otherwise  = zip [m..] (take (n - m + 1) (drop (m - 1) (inits' s)))

lastLettersGen :: String -> (Int, Int) -> [(Int, String)]
lastLettersGen s (m, n)
  | elem ' ' s = []
  | otherwise  = zip [len', len' - 1 ..] 
                     (drop (len - n - 1) (take (len - m) (tails' s)))
  where
    len = length s
    len' = min (len - 1) n

middleLettersGen :: String -> (Int, Int) -> [(Int, String)]
middleLettersGen s (m, n)
  | elem ' ' s = []
  | otherwise  = [(k, take k (drop i s)) | 
                   i <- [1 .. ((len - 1) `div` 2)], let k = len - 2 * i,
                   k >= m && k <= n]
  where
    len = length s

endLettersGen :: String -> (Int, Int) -> [(Int, String)]
endLettersGen s (m, n)
  | elem ' ' s = []
  | otherwise  = [(k, take i s ++ drop (i + len - k) s) | 
                   i <- [1 .. (len - 1) `div` 2], let k = 2 * i,
                   k >= m && k <= n] 
  where
    len = length s

--
-- Applies subtext function (f) to all subtext strings (ws) 
-- whilst satisfying the global length constraints (con).
-- minRes is the minimum number of letters that can be generated
-- by the given subtext function, e.g. 1 for first, 2 for ends...
--
spanWords :: (String -> (Int, Int) -> [(Int, String)]) -> 
             Int -> [String] -> (Int, Int) -> 
             [String]
spanWords f minRes ws con 
  = map concat (spanWords' ws lenws con)
  where
    lenws = length ws
    spanWords' [] _ _
      = [[]]
    spanWords' (w : ws) len (m, n)
      | len == 1
        = map (\(k, s) -> [s]) (f' w m n)
      | otherwise
        = [s : rest |
            (k, s) <- f' w 1 (n - reserve),
            rest <- spanWords' ws (len - 1) (m - k, n - k)]
      where
        reserve = (len - 1) * minRes
        f' s m n = f s (max 1 m, max 0 n)

genFirstLetters
  = spanWords firstLettersGen 1

genLastLetters
  = spanWords lastLettersGen 1

genMiddleLetters
  = spanWords middleLettersGen 1

genEndLetters
  = spanWords endLettersGen 2

middleLetters :: String -> [String]
middleLetters s
  | length s < 3 = []
  | otherwise    = substrings s2
  where
    (_, s2, _) = extractMiddle s

hiddenWords [w]
  = middleLetters w
hiddenWords ws
  = [s ++ concat m ++ s' | s <- suffixes (tail l), s' <- prefixes (take n r)]
  where
    (l, m, r) = extractMiddle (map removeApostrophe ws)
    n = length r - 1

--
-- We want, e.g., first ["star","animal"] to match "stan"
-- but not "star animal".
 
isOneWord s
  = not (elem ' ' s)

--
-- Matching functions for MAPPED versions of take-outs...
-- Must span all words
-- Must match at least one character in each word
-- Must not consume all characters in any word
-- Note: No need for the number of matching characters to be
-- the same in each word
-- The boolean tells us we've matched at least one character
-- in the lead word.
-- Singleton cases are coded separately.
--
matchesFirstLetters xs [s]
  = isOneWord s && length s > n && take n s == xs 
  where
    n = length xs
matchesFirstLetters xs ys 
  = all isOneWord ys &&
    matchesFirstLetters' xs (map removeApostropheS ys) False

matchesFirstLetters' [x] [y : (_ : _)] _
  = x == y
matchesFirstLetters' (x : xs) ((y : (ys@(_ : _))) : zs) matched
  | x == y    = matchesFirstLetters' xs (ys : zs) True ||
                matchesFirstLetters' xs zs False
  | matched   = matchesFirstLetters' (x : xs) zs False
  | otherwise = False
matchesFirstLetters' _ _ _
  = False

matchesLastLetters xs [s]
  = m < n && drop (n - m) s == xs
  where
    m = length xs
    n = length s
matchesLastLetters xs ys
  = matchesLastLetters' xs (map removeApostropheS ys) False

matchesLastLetters' xs ys b
  = matchesFirstLetters' (reverse xs) ws' b
  where
    ws' = map reverse (reverse ys)

--
-- For multi-word text middle means middle, 
-- although we don't have to take the same
-- number of letters out from each word. The recursive rule
-- ensures that we reserve at least one letter in every word.
-- For single word text the middle can be anywhere, 
-- e.g. enTHUSiastic.
-- A function, minMiddleLetterLength, determines the 
-- smallest allowable substring length for a middle letter take-out,
-- e.g. if minMiddleLetterLength 7 = 3 then only substrings of
-- length 3 or more are permitted from a word of length 7.
-- This prevents -- stupid solutions arising from, e.g. 
-- matchesMiddleLetters "r" ["percentages"] --> True.
-- The exception is when s is bang in the middle, in which 
-- case it's OK, subject to length constraints again 
-- (minMiddleLetterLength').
--
matchesMiddleLetters _ []
  = False
matchesMiddleLetters s [s']
  | len >= minLen' && take len (drop ((len' - len) `div` 2) s') == s 
    = True
  | len >= minLen && isOneWord s' && len' >= len + 2 
    = isSubstring s mid
  | otherwise 
    = False
  where
    len  = length s
    len' = length s'
    minLen = minMiddleLetterLength len'
    minLen' = minMiddleLetterLength' len'
    (_, mid, _) = extractMiddle s'
matchesMiddleLetters xs ss
  = all isOneWord ss && matchesAll xs (length xs) ss (length ss - 1)
  where
    matchesAll [] _ [] _
      = True
    matchesAll xs len (s : ss) lens
      = or [matchesOneWord xsPre k &&
            matchesAll xsPost (len - k) ss (lens - 1) |
              k <- [1 .. len - lens],
              let (xsPre, xsPost) = splitAt k xs]
      where
        len' = length s
        matchesOneWord xsPre k
          | len' >= k + 2 && (odd k && odd len' || even k && even len')
            = take k (drop ((len' - k) `div` 2) s) == xsPre
          | otherwise
            = False
         
    matchesAll _ _ _ _
      = False

--
-- The take-out is symmetric, although we don't have to take the 
-- same number of letters out from each word. The recursive rule

-- ensures that we reserve at least two letters in every word.
--
matchesEndLetters xs []
  = False  
matchesEndLetters xs [s]
  | isOneWord s && even len && len' > len 
    = take len2 s == xsPre && drop (len' - len2) s == xsPost
  | otherwise 
    = False
  where
    len = length xs
    len2 = len `div` 2
    len' = length s
    (xsPre, xsPost) = splitAt len2 xs 
matchesEndLetters xs ss
  = all isOneWord ss && matchesAll xs (length xs) ss (length ss - 1)
  where
    matchesAll [] _ [] _
      = True
    matchesAll xs len (s : ss) lens
      = or [matchesEndLetters xsPre [s] && 
            matchesAll xsPost (len - k) ss (lens - 1) | 
              k <- [2 .. len - 2 * lens],
              let (xsPre, xsPost) = splitAt k xs]
    matchesAll _ _ _ _
      = False

--
-- These are the concatenation equivalents when the take-out
-- is NOT mapped.
--
matchesFirstLettersC :: String -> [String] -> Bool
matchesFirstLettersC s ss
  = all isOneWord ss && len > len'' && len' > len && take len css == s
  where
    css = concat ss
    len = length s
    len' = length css
    len'' = len' - length (last ss)

matchesLastLettersC :: String -> [String] -> Bool
matchesLastLettersC s []
  = False
matchesLastLettersC s ss
  = all isOneWord ss && len > len'' && len' > len && drop (len' - len) css == s
  where
    css = concat ss
    len = length s
    len' = length css
    len'' = length (concat (tail ss))

--
-- This is essentially the 'hidden word' construction, e.g.
-- matchesMiddleLettersC "nma" ["con","man"] --> True.
-- If there is one word then we delegate to the rule above.
--
matchesMiddleLettersC :: String -> [String] -> Bool
matchesMiddleLettersC s [s']
  = matchesMiddleLetters s [s']
matchesMiddleLettersC s ss@(h : t)
  = all isOneWord ss &&
    or [isPrefixOf s (drop i h ++ concat t) |
        i <- [max 1 (r + 1) .. min (length h - 1) (r + n - 1)]]
  where
    r = length h - length s + sum (map length (init t))
    n = length (last t)

matchesEndLettersC :: String -> [String] -> Bool
matchesEndLettersC xs ys
  | not (all isOneWord ys) = False
  | length xs >= length (concat ys) = False
matchesEndLettersC xs ys@[s]
  = or [matchesFirstLetters pre ys && matchesLastLetters post ys |
         (pre, post) <- split2 xs]
matchesEndLettersC xs [s, s']
  = or [matchesFirstLetters pre [s] && matchesLastLetters post [s'] |
         (pre, post) <- split2 xs]
matchesEndLettersC xs _
  = False

matchesAnyMiddleLetters :: String -> String -> Bool
matchesAnyMiddleLetters xs ys
  | n' < 3    = False
  | otherwise = findMatch (take (n' - 2) (tail ys)) (n' - n)
  where
    n = length xs
    n' = length ys
    findMatch _ 0
      = False
    findMatch s@(_ : s') k
      = take n s == xs || findMatch s' (k - 1)

inits' [_]
  = []
inits' (x : xs)
  = [x] : [x : ys | ys <- inits' xs]
inits' _
  = []

tails' [_]
  = []
tails' (x : xs)
  = xs : tails' xs
tails' _
  = []

tails'' [_]
  = []
tails'' [x, y] 
  = []
tails'' (x : xs)
  = xs : tails'' xs
tails'' _
  = []

odds []
  = []
odds [x]
  = [x]
odds (x : x' : xs)
  = x : odds xs   

evens (x : x' : xs)
  = x' : evens xs   
evens xs
  = []

rotate []
  = []
rotate xs@(x : xs')
  = [(xs' ++ [x]), reverse (rxs ++ [x'])]
  where
    (x' : rxs) = reverse xs
  

-- Plural <-> singular transformer functions...

vowels 
  = "aeiou"

--
-- Need to do this properly, i.e. for noun plurals only.
-- Note: the letter 's' (as in South) may come up and its
-- postfix is null, so we must catch the "" case.
--
isPlural ""
  = False
isPlural s
  = elem s (map snd specialCases) ||
    not (elem '\'' s) && last s == 's'

makePlural s
  = fromMaybe (generalCases s) (lookup s specialCases)
  where
    generalCases s
      | ends "bass" = s
      | ends "deer" = s
      | ends "sheep" = s
      | ends "pox" = s
      | ends "media" = s
      | otherwise = reverse (applyGeneralRule (reverse s))
      where
        srev = reverse s
        ends s' = take (length s') srev == reverse s'
    applyGeneralRule ('f' : s@(c : _))
      | elem c "lr" = "sev" ++ s
    applyGeneralRule ('y' : s@(c : _))
      | not (elem c vowels) = "sei" ++ s
    applyGeneralRule s@('s' : s'@(c : s''))
      | elem c vowels = if take 2 s' == "ui" then 'i' : s'' else "ses" ++ s''
      | otherwise = "se" ++ s
    applyGeneralRule s@('h' : c : _)
      | elem c "cs" = "se" ++ s
    applyGeneralRule s
      = 's' : s  

makeSingular s
  = fromMaybe (generalCases s) (lookup s (map (\(a, b) -> (b, a)) specialCases))
  where
    generalCases s
      | ends "bass" = s
      | ends "deer" = s
      | ends "sheep" = s
      | ends "pox" = s
      | ends "media" = s
      | ends "shoes" = replace 1 "" 
      | ends "oes" = replace 2 "" 
      | ends "xes" = replace 2 ""
      | ends "ches" = replace 2 "" 
      | ends "shes" = replace 2 ""
      | ends "sses" = replace 2 ""
      | ends "tives" = replace 1 "" 
      | ends "hives" = replace 1 "" 
      | otherwise = reverse (applyGeneralRule (reverse s))
      where
        srev = reverse s
        ends s' = take (length s') srev == reverse s'
        replace k s  
          = reverse (s ++ post)
          where
            (pre, post) = splitAt k srev
    -- Note: The lone string "s" is NOT plural, hence the @...
    applyGeneralRule ('s' : 'e' : 'i' : s@(c : _))
      | not (elem c vowels) = "y" ++ s
    applyGeneralRule ('s' : 'e' : 'v' : s@(c : _))
      | elem c "lr" = "f" ++ s
    applyGeneralRule ('s' : (s@(_ : _)))
      = s
    applyGeneralRule s
      = s  
      
specialCases
  = [("alias", "aliases"),
     ("alumnus", "alumni"),
     ("analysis", "analyses"),
     ("amoyese", "amoyese"),
     ("appendix", "appendices"),
     ("axis", "axes"),
     ("atlas", "atlases"),
     ("barracks", "barracks"),
     ("beef", "beefs"),
     ("bison", "bison"),
     ("borghese", "borghese"),
     ("bream", "bream"),
     ("breeches", "breeches"),
     ("britches", "britches"),
     ("brother", "brothers"),
     ("buffalo", "buffalo"),
     ("bus", "buses"),
     ("cactus", "cacti"),
     ("cafe", "cafes"),
     ("cantus", "cantus"),
     ("carp", "carp"),
     ("chassis", "chassis"),
     ("child", "children"),
     ("clippers", "clippers"),
     ("cod", "cod"),
     ("coitus", "coitus"),
     ("congoese", "congoese"),
     ("contretemps", "contretemps"),
     ("cookie", "cookies"),
     ("corps", "corps"),
     ("corpus", "corpuses"),
     ("cow", "cows"),
     ("crisis", "crises"),
     ("criterion", "criteria"),
     ("curve", "curves"),
     ("debris", "debris"),
     ("diabetes", "diabetes"),
     ("diagnonis", "diagnoses"),
     ("djinn", "djinn"),
     ("echo", "echoes"),
     ("eland", "eland"),
     ("elf", "elves"),
     ("elk", "elk"),
     ("embargo", "embargoes"),
     ("equipment", "equipment"),
     ("faroese", "faroese"),
     ("flounder", "flounder"),
     ("focus", "foci"),
     ("foe", "foes"),
     ("foochowese", "foochowese"),
     ("foot", "feet"),
     ("fungus", "fungi"),
     ("gallows", "gallows"),
     ("ganglion", "ganglions"),
     ("genevese", "genevese"),
     ("genie", "genies"),
     ("genoese", "genoese"),
     ("genus", "genera"),
     ("gilbertese", "gilbertese"),
     ("goose", "geese"),
     ("graffiti", "graffiti"),
     ("graffito", "graffiti"),
     ("gulf", "gulfs"),
     ("headquarters", "headquarters"),
     ("hero", "heroes"),
     ("herpes", "herpes"),
     ("hijinks", "hijinks"),
     ("hoof", "hoofs"),
     ("hoof", "hooves"),
     ("hottentotese", "hottentotese"),
     ("index", "indices"),
     ("information", "information"),
     ("innings", "innings"),
     ("jackanapes", "jackanapes"),
     ("kiplingese", "kiplingese"),
     ("knife", "knives"),
     ("kongoese", "kongoese"),
     ("leaf", "leaves"),
     ("life", "lives"),
     ("loaf", "loaves"),
     ("louse", "lice"),
     ("lucchese", "lucchese"),
     ("mackerel", "mackerel"),
     ("maltese", "maltese"),
     ("man", "men"),
     ("matrix", "matrices"),
     ("mews", "mews"),
     ("money", "monies"),
     ("mongoose", "mongooses"),
     ("moose", "moose"),
     ("mouse", "mice"),
     ("move", "moves"),
     ("movies", "movies"),
     ("mumps", "mumps"),
     ("mythos", "mythoi"),
     ("nankingese", "nankingese"),
     ("news", "news"),
     ("nexus", "nexus"),
     ("niasese", "niasese"),
     ("niche", "niches"),
     ("nucleus", "nuclei"),
     ("numen", "numina"),
     ("occiput", "occiputs"),
     ("octopus", "octopuses"),
     ("opus", "opuses"),
     ("ox", "oxen"),
     ("parenthesis", "parentheses"),
     ("pekingese", "pekingese"),
     ("penis", "penises"),
     ("person", "people"),
     ("phenomenon", "phenomena"),
     ("piedmontese", "piedmontese"),
     ("pincers", "pincers"),
     ("pistoiese", "pistoiese"),
     ("pliers", "pliers"),
     ("portuguese", "portuguese"),
     ("potato", "potatoes"),
     ("proceedings", "proceedings"),
     ("prognosis", "prognoses"),
     ("quiz", "quizzes"),
     ("rabies", "rabies"),
     ("rhinoceros", "rhinoceros"),
     ("rice", "rice"),
     ("salmon", "salmon"),
     ("sarawakese", "sarawakese"),
     ("scissors", "scissors"),
     ("self", "selves"),
     ("series", "series"),
     ("sex", "sexes"),
     ("shavese", "shavese"),
     ("shears", "shears"),
     ("siemens", "siemens"),
     ("soliloquy", "soliloquies"),
     ("species", "species"),
     ("staff", "staff"),
     ("status", "statuses"),
     ("svvermontese", "svvermontese"),
     ("swine", "swine"),
     ("syllabus", "syllabi"),
     ("synopsis", "synopses"),
     ("testis", "testes"),
     ("thesis", "theses"),
     ("tomato", "tomatoes"),
     ("tooth", "teeth"),
     ("torpedo", "torpedoes"),
     ("trilby", "trilbys"),
     ("trousers", "trousers"),
     ("trout", "trout"),
     ("tuna", "tuna"),
     ("turf", "turfs"),
     ("vertex", "vertices"),
     ("veto", "vetoes"),
     ("virus", "viruses"),
     ("wave", "waves"),
     ("wenchowese", "swwenchowese"),
     ("whiting", "whiting"),
     ("wife", "wives"),
     ("wildebeest", "wildebeest"),
     ("woman", "women"),
     ("yengeese", "yengeese")]

