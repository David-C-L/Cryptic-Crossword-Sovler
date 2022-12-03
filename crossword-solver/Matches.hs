module Matches where

import Data.Set as Set hiding (map, foldl')
import Data.Hashable
import Data.List

import Types
import Utilities
import Debug.Trace

matches :: [Int] -> [ParseTree] -> Set (TreeId, [Int])
matches s ts
  = foldl' f Set.empty ts
  where
    f set t = matches' s t set 0

inBounds s (m, n)
  = len >= m && len <= n 
  where
    len = length s

matches' :: (Eq a, Show a, Ord a, Hashable a) => [a] -> ParseTree 
          -> Set (TreeId, [a])
          -> Int
          -> Set (TreeId, [a])
matches' _ _ setIn level 
  | level == 3 = setIn
matches' _ t setIn _
  | dontCache t = setIn
matches' s t@((index, _), (_, (bs, _))) setIn level
  | inBounds s bs = matches'' s t setIn' level
  | otherwise     = trace (show s ++ ": " ++ show t) $ error "Matches bounds error" 
  where 
    setIn' = if level == 0 
             then setIn
             else Set.insert (index, s) setIn

dontCache (_, (Synonym _, _))
  = True
dontCache (_, (Hyponym _ _, _))
  = True
dontCache (_, (Odds _ _, _))
  = True
dontCache (_, (Evens _ _, _))
  = True
dontCache _
  = False

matches'' :: (Eq a, Show a, Ord a, Hashable a) => [a] -> ParseTree 
          -> Set (TreeId, [a]) 
          -> Int
          -> Set (TreeId, [a])
matches'' s (_, (Reversal i t, bs)) setIn level
  = matches' (reverse s) t setIn (level + 1)
matches'' s (_, (Rotation i t, bs)) setIn level
  = foldl' f setIn (rotate s)
  where
    f set s' = matches' s' t set (level + 1)
matches'' s (_, (Insertion i i' t t', _)) setIn level
  = foldl' f setIn (split3mn s (fst (snd (snd t)))
                              (fst (snd (snd t'))))
  where
    f set (s1, s2, s3) 
      = matches' (s1 ++ s3) t' (matches' s2 t set (level + 1)) (level + 1)
matches'' s (_, (Charade i ts, _)) setIn level
  = foldl' f setIn (splitNopt s (map (fst . snd . snd) ts))
  where
    f set split 
      = foldl' f' set (zip split ts)
    f' set (s, t)
      = matches' s t set (level + 1)
matches'' _ _ setIn _
  = setIn


