module ShowFunctions where

import Data.Char
import Data.List
import Data.Maybe
import Data.Array.IArray
import Debug.Trace

import Types
import CacheFunctions
import Constants

-----------------------------------------------------------------

showP :: (Int, Parse) -> ParserCache -> String
showP (k, (d, _, t)) pCache
  = show k ++ ": [DEF = " ++ getString pCache d ++ "] " ++ showT t pCache

showI :: TreeId -> String
showI (i, j, k)
  = "<" ++ show i ++ "," ++ show j ++ "," ++ show k ++ ">"

--
-- Show Function for Parse Trees...
--
showT :: ParseTree -> ParserCache -> String
showT ((index, _), (Abbreviation _ i, _)) pCache
  = showI index ++ "ABBR " ++ getString pCache i
showT ((index, _), (SpareWord sw _, _)) pCache
  = showI index ++ "SYN " ++ sw 
showT ((index, _), (Synonym i, _)) pCache
  = showI index ++ "SYN " ++ getString pCache i
showT ((index, _), (Anagram _ t@(_, (SpareWord sw i, _)), _)) pCache
  = showI index ++ "ANAG (" ++ showT t pCache ++ ")"
showT ((index, _), (Anagram _ t, _)) pCache
  = showI index ++ "ANAG (" ++ showT t pCache ++ ")"
showT ((index, _), (AnagramInf _ t t', _)) pCache
  = showI index ++ "ANAGINF (" ++ showT t pCache ++ ")" ++ " (" ++ showT t' pCache ++ ")"
showT ((index, _), (Odds _ i, _)) pCache
  = showI index ++ "ODDS " ++ getString pCache i
showT ((index, _), (Evens _ i, _)) pCache
  = showI index ++ "EVENS " ++ getString pCache i
showT ((index, _), (FirstLetters _ t, _)) pCache
  = showI index ++ "FIRST (" ++ showT t pCache ++ ")"
showT ((index, _), (LastLetters _ t, _)) pCache
  = showI index ++ "LAST (" ++ showT t pCache ++ ")"
showT ((index, _), (MiddleLetters _ t, _)) pCache
  = showI index ++ "MID (" ++ showT t pCache ++ ")"
showT ((index, _), (EndLetters _ t, _)) pCache
  = showI index ++ "ENDS (" ++ showT t pCache ++ ")"
showT ((index, _), (Duplicate i s t, _)) pCache
  = showI index ++ "DUP" ++ showInd i pCache ++ "(" ++ showT t pCache ++ ")"
showT ((index, _), (Homophone _ t, _)) pCache
  = showI index ++ "HOM (" ++ showT t pCache ++ ")"
showT ((index, _), (Hyponym _ i, _)) pCache
  = showI index ++ "EG " ++ getString pCache i
showT ((index, _), (Reversal _ t@(_, (SpareWord sw ind, _)), _)) pCache
  = showI index ++ "REV (" ++ showT t pCache ++ ")"
showT ((index, _), (Rotation _ t, _)) pCache
  = showI index ++ "ROT (" ++ showT t pCache ++ ")"
showT ((index, _), (Reversal _ t, _)) pCache
  = showI index ++ "REV (" ++ showT t pCache ++ ")"
showT ((index, _), (Insertion _ _ t t', _)) pCache
  = showI index ++ "INS (" ++ showT t pCache ++ ")" ++ " (" ++ showT t' pCache ++ ")"
showT ((index, _), (Subtraction _ t t', _)) pCache
  = showI index ++ "SUB (" ++ showT t pCache ++ ")" ++ " (" ++ showT t' pCache ++ ")"
showT ((index, _), (Charade i ts, _)) pCache
  = showI index ++ "(" ++ showTs i ts pCache ++ ")"

showTs i [t] pCache
  = showT t pCache
showTs i (t : ts) pCache
  = showT t pCache ++ " +" ++ showInd i pCache ++ showTs i ts pCache

showInd i pCache
  | i == noInd = " "
  | otherwise  = "[" ++ getString pCache i ++ "] "
 
--
-- Show Function for solutions...
--
showSol :: ParserCache -> Solution -> String
showSol pCache (d, i, pt, rt@(R(sol, _))) 
  = showInd d ++ " ->" ++ showLink i ++ showR sol pt rt Nothing
  where
    despace = filter (/=' ')
     
    showLink i 
      = if i == noInd 
        then " " 
        else "[" ++ showInd i ++ "] "

    showTakeout :: SubtextType -> String -> String -> String
    showTakeout con s s'
      | n == 1 
        = fromMaybe err (showTakeout' con s s')
      | otherwise 
        = unwords' (zipWith (showTakeout' con) (frags s) ws)
      where
        unwords' ms 
          | length s `mod` n == 0 &&
            all isJust ms = unwords (map fromJust ms)
          | otherwise = fromMaybe err (showTakeout' con s (filter (/=' ') s'))
        err = "***Error in mid letter takeout***"
        ws = words s'
        n = length ws
        k = length s `div` n
        frags [] = []
        frags s  = take k s : frags (drop k s)

    showTakeout' :: SubtextType -> String -> String -> Maybe String
    showTakeout' First s s'
      = Just (map toUpper s ++ drop (length s) s')
    showTakeout' Last s s'
      = Just (take (m - n) s' ++  map toUpper s)
      where
        m = length s'
        n = length s
    showTakeout' Middle s s'
      = showMiddleTakeout s s'
      where
        showMiddleTakeout :: String -> String -> Maybe String
        showMiddleTakeout s s'@(c : cs)
          | take n s' == s = Just (map toUpper s ++ drop n s')
          | otherwise = do s'' <- showMiddleTakeout s cs
                           return (c : s'')
          where 
            n = length s
        showMiddleTakeout s s'
          = Nothing
    showTakeout' End s s'
      = Just (map toUpper s1 ++ take (length s' - 2 * n) (drop n s') ++
               map toUpper s2)
      where
        n = length s `div` 2
        (s1, s2) = splitAt n s
        
    up Nothing str s
      | otherwise = map toUpper s'
      where
        s' = filter (/=' ') s
    up (Just con) str s
      = showTakeout con str s 

    maybeShow :: String -> Maybe SubtextType -> String
    maybeShow s tout = maybe (map toUpper (despace s)) (const (map toUpper s)) tout

    showR :: String -> ParseTree -> ResultTree -> Maybe SubtextType -> String
    showR str (_, (SpareWord sw _, _)) _ tout
      = map toUpper sw
    showR str (_, (Synonym i, _)) (R (s, _)) tout
      | getString pCache i == s = s
      | otherwise = map toUpper s ++ "=" ++ showInd i
    showR str (_, (Charade i ts, _)) (R (s, rts)) tout
      = maybeShow s tout ++ "(" ++ intercalate plus (zipWith f ts rts) ++ ")"
      where
        plus = " +" ++ if i == noInd 
                       then " " 
                       else "[" ++ showInd i ++ "] "
               
        f t (rt@(R (s, _))) = showR s t rt Nothing
    showR str pt t@(R (s, _)) tout
      = maybeShow s tout ++ "(" ++ showR' pt t ++ ")"
      where
        showR' :: ParseTree -> ResultTree -> String
        showR' (_, (Hyponym i j, _)) (R (s, _))
          = "hyponym[" ++ showInd i ++ "]" ++ map toUpper s ++ "=" ++ showInd j
        showR' (_, (Abbreviation i i', _)) (R (_, _)) 
          = "abbreviation[" ++ showInd i ++ "] " ++ showInd i'
        -- Note the special case, e.g. from hairstyle
        showR' (_, (Anagram _ t@(_, (SpareWord sw ind, _)), _)) (R (s, [rt]))
          = "anagram[" ++ ind ++ "] " ++ showR s t rt Nothing
        showR' (_, (Anagram i t, _)) rt@(R (s, [rt'@(R (s', _))]))
          | (getIndicatorPredicateSet pCache i ! ReversalInd) && 
            (s' == reverse str)
            = showR' (undefined, (Reversal i t, undefined)) rt
          | otherwise = "anagram[" ++ showInd i ++ "] " ++ showR s t rt' Nothing
        showR' (_, (AnagramInf i t t', _)) (R (s, [rt1@(R (s1, _)), rt2@(R (s2, _))]))
          = "anagram[" ++ showInd i ++ "] " ++
            showR s (undefined, (Charade noInd [t, t'], undefined)) (R (s1 ++ s2, [rt1, rt2])) Nothing
        showR' (_, (Odds _ i, _)) _
          = "odd letters[" ++ showInd i ++ "]"
        showR' (_, (Evens _ i, _)) _
          = "even letters[" ++ showInd i ++ "]"
        showR' (_, (FirstLetters i t, _)) (R (s, [rt]))
          = "first letters[" ++ showInd i ++ "] " ++ showR s t rt (Just First)
        showR' (_, (LastLetters i t, _)) (R (s, [rt]))
          = "last letters[" ++ showInd i ++ "] " ++ showR s t rt (Just Last)
        showR' (_, (MiddleLetters i t, _)) (R (s, [rt]))
          = "middle letters[" ++ showInd i ++ "] " ++ showR s t rt (Just Middle)
        showR' (_, (EndLetters i t, _)) (R (s, [rt]))
          = "end letters[" ++ showInd i ++ "] " ++ showR s t rt (Just End)
        showR' (_, (Duplicate i _ t, _)) (R (s, [rt, rt']))
          = "duplicate[" ++ showInd i ++ "] " ++ 
            "(" ++ showR s t rt Nothing ++ " + " ++ showR s t rt' Nothing ++ ")"
        showR' (_, (Homophone i t, _)) (R (s, [rt]))
          = "homophone[" ++ showInd i ++ "] " ++ showR s t rt Nothing
        -- Note the special case, e.g. from set-back 
        showR' (_, (Reversal _ t@(_, (SpareWord sw ind, _)), _)) (R (s, [rt]))
          = "reversal[" ++ ind ++ "] " ++ showR s t rt Nothing
        showR' (_, (Rotation i t, _)) (R (s, [rt]))
          = "rotation[" ++ showInd i ++ "] " ++ showR s t rt Nothing
        showR' (_, (Reversal i t, _)) (R (s, [rt]))
          = "reversal[" ++ showInd i ++ "] " ++ showR s t rt Nothing
        showR' (_, (Insertion i ind t t', _)) (R (s, [rt1, rt2]))
          = "insert[" ++ showInsInd ++ "] " ++
            showR s t rt1 Nothing ++ " into " ++ showR s t' rt2 Nothing
          where
            showInsInd = if ind == ""
                         then showInd i
                         else ind
        showR' (_, (Subtraction i t t', _)) (R (s, [rt1, rt2]))
          = "subtract[" ++ showInd i ++ "] " ++
            showR s t rt1 Nothing ++ " from " ++ showR s t' rt2 Nothing
        showR' t r
          = error (showT pt pCache ++ "\n" ++ "No match " ++ show t ++ "\n" ++ show r)

    showInd i 
      = getString pCache i
    
