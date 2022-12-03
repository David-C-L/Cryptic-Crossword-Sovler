module Stemmer where

import Debug.Trace
import qualified Data.Set as Set
import Data.List as List
import qualified Utilities

data Target = Unmodified String | Modified String | Final String
  deriving Show

data Rule = Intact String |
      ReplaceAndContinue String String |
      Protect String |
      ReplaceAndStop String String
  deriving Show

-- This mainly comes https://github.com/wooorm/lancaster-stemmer/blob/master/index.js
      
vowels = Set.fromList ['a','e','i','o','u']

isVowel :: Char -> Bool
isVowel c = Set.member c vowels


rules :: [Rule]
rules = [

    (Intact (reverse "ia")),
    (Intact (reverse "a")),

    (ReplaceAndStop (reverse "bb") (reverse "b")),

    (ReplaceAndStop (reverse "ytic") (reverse "ys")),
    (ReplaceAndContinue (reverse "ic") (reverse "")),
    (ReplaceAndContinue (reverse "nc") (reverse "nt")),


    (ReplaceAndStop (reverse "dd") (reverse "d")),
    (ReplaceAndContinue (reverse "ied") (reverse "y")),
    (ReplaceAndStop (reverse "ceed") (reverse "cess")),
    (ReplaceAndStop (reverse "eed") (reverse "ee")),
    (ReplaceAndContinue (reverse "ed") (reverse "")),
    (ReplaceAndContinue (reverse "hood") (reverse "")),

    (ReplaceAndContinue (reverse "e") (reverse "")),

    (ReplaceAndStop (reverse "lief") (reverse "liev")),
    (ReplaceAndContinue (reverse "if") (reverse "")),


    (ReplaceAndContinue (reverse "ying") (reverse "ie")),
    (ReplaceAndContinue (reverse "uing") (reverse "ue")),
    (ReplaceAndContinue (reverse "ing") (reverse "")),
    (ReplaceAndContinue (reverse "sy") (reverse "")),
    (ReplaceAndStop (reverse "iag") (reverse "y")),
    (ReplaceAndContinue (reverse "ag") (reverse "")),
    (ReplaceAndStop (reverse "gg") (reverse "g")),


    (Intact (reverse "th")),
    (ReplaceAndStop (reverse "guish") (reverse "ct")),
    (ReplaceAndContinue (reverse "ish") (reverse "")),


    (Intact (reverse "i")),
    (ReplaceAndContinue (reverse "i") (reverse "y")),


    (ReplaceAndStop (reverse "ij") (reverse "id")),
    (ReplaceAndStop (reverse "fuj") (reverse "fus")),
    (ReplaceAndStop (reverse "uj") (reverse "ud")),
    (ReplaceAndStop (reverse "oj") (reverse "od")),
    (ReplaceAndStop (reverse "hej") (reverse "her")),
    (ReplaceAndStop (reverse "verj") (reverse "vert")),
    (ReplaceAndStop (reverse "misj") (reverse "mit")),
    (ReplaceAndStop (reverse "nj") (reverse "nd")),
    (ReplaceAndStop (reverse "j") (reverse "s")),


    (ReplaceAndStop (reverse "ifiabl") (reverse "")),
    (ReplaceAndStop (reverse "iabl") (reverse "y")),
    (ReplaceAndContinue (reverse "abl") (reverse "")),
    (ReplaceAndStop (reverse "ibl") (reverse "")),
    (ReplaceAndContinue (reverse "bil") (reverse "bl")),
    (ReplaceAndStop (reverse "cl") (reverse "c")),
    (ReplaceAndStop (reverse "iful") (reverse "y")),
    (ReplaceAndContinue (reverse "ful") (reverse "")),
    (ReplaceAndStop (reverse "ul") (reverse "")),
    (ReplaceAndContinue (reverse "ial") (reverse "")),
    (ReplaceAndContinue (reverse "ual") (reverse "")),
    (ReplaceAndContinue (reverse "al") (reverse "")),
    (ReplaceAndStop (reverse "ll") (reverse "l")),


    (ReplaceAndStop (reverse "ium") (reverse "")),
    (Intact (reverse "um")),
    (ReplaceAndContinue (reverse "ism") (reverse "")),
    (ReplaceAndStop (reverse "mm") (reverse "m")),


    (ReplaceAndContinue (reverse "sion") (reverse "j")),
    (ReplaceAndStop (reverse "xion") (reverse "ct")),
    (ReplaceAndContinue (reverse "ion") (reverse "")),
    (ReplaceAndContinue (reverse "ian") (reverse "")),
    (ReplaceAndContinue (reverse "an") (reverse "")),
    (Protect (reverse "een")),
    (ReplaceAndContinue (reverse "en") (reverse "")),
    (ReplaceAndStop (reverse "nn") (reverse "n")),


    (ReplaceAndContinue (reverse "ship") (reverse "")),
    (ReplaceAndStop (reverse "pp") (reverse "p")),


    (ReplaceAndContinue (reverse "er") (reverse "")),
    (Protect (reverse "ear")),
    (ReplaceAndStop (reverse "ar") (reverse "")),
    (ReplaceAndContinue (reverse "ior") (reverse "")),
    (ReplaceAndContinue (reverse "or") (reverse "")),
    (ReplaceAndContinue (reverse "ur") (reverse "")),
    (ReplaceAndStop (reverse "rr") (reverse "r")),
    (ReplaceAndContinue (reverse "tr") (reverse "t")),
    (ReplaceAndContinue (reverse "ier") (reverse "y")),


    (ReplaceAndContinue (reverse "ies") (reverse "y")),
    (ReplaceAndStop (reverse "sis") (reverse "s")),
    (ReplaceAndContinue (reverse "is") (reverse "")),
    (ReplaceAndContinue (reverse "ness") (reverse "")),
    (Protect (reverse "ss")),
    (ReplaceAndContinue (reverse "ous") (reverse "")),
    (Intact (reverse "us")),
    (ReplaceAndContinue (reverse "s") (reverse "")),
    (ReplaceAndStop (reverse "s") (reverse "")),


    (ReplaceAndStop (reverse "plicat") (reverse "ply")),
    (ReplaceAndContinue (reverse "at") (reverse "")),
    (ReplaceAndContinue (reverse "ment") (reverse "")),
    (ReplaceAndContinue (reverse "ent") (reverse "")),
    (ReplaceAndContinue (reverse "ant") (reverse "")),
    (ReplaceAndStop (reverse "ript") (reverse "rib")),
    (ReplaceAndStop (reverse "orpt") (reverse "orb")),
    (ReplaceAndStop (reverse "duct") (reverse "duc")),
    (ReplaceAndStop (reverse "sumpt") (reverse "sum")),
    (ReplaceAndStop (reverse "cept") (reverse "ceiv")),
    (ReplaceAndStop (reverse "olut") (reverse "olv")),
    (Protect (reverse "sist")),
    (ReplaceAndContinue (reverse "ist") (reverse "")),
    (ReplaceAndStop (reverse "tt") (reverse "t")),


    (ReplaceAndStop (reverse "iqu") (reverse "")),
    (ReplaceAndStop (reverse "ogu") (reverse "og")),


    (ReplaceAndContinue (reverse "siv") (reverse "j")),
    (Protect (reverse "eiv")),
    (ReplaceAndContinue (reverse "iv") (reverse "")),

    (ReplaceAndStop (reverse "wn") (reverse "w")),


    (ReplaceAndContinue (reverse "bly") (reverse "bl")),
    (ReplaceAndContinue (reverse "ily") (reverse "y")),
    (Protect (reverse "ply")),
    (ReplaceAndContinue (reverse "ly") (reverse "")),
    (ReplaceAndStop (reverse "ogy") (reverse "og")),
    (ReplaceAndStop (reverse "phy") (reverse "ph")),
    (ReplaceAndStop (reverse "omy") (reverse "om")),
    (ReplaceAndStop (reverse "opy") (reverse "op")),
    (ReplaceAndContinue (reverse "ity") (reverse "")),
    (ReplaceAndContinue (reverse "ety") (reverse "")),
    (ReplaceAndStop (reverse "lty") (reverse "l")),
    (ReplaceAndStop (reverse "istry") (reverse "")),
    (ReplaceAndContinue (reverse "ary") (reverse "")),
    (ReplaceAndContinue (reverse "ory") (reverse "")),
    (ReplaceAndStop (reverse "ify") (reverse "")),
    (ReplaceAndContinue (reverse "ncy") (reverse "nt")),
    (ReplaceAndContinue (reverse "acy") (reverse "")),


    (ReplaceAndContinue (reverse "iz") (reverse "")),
    (ReplaceAndStop (reverse "yz") (reverse "ys"))

  ]


isAcceptable :: String -> Bool
-- Check if we still have a valid string to keep stemming on
-- Simple rule: we stop at 3 (this could be refined, e.g.
-- with different rules for vowels, consonants etc.)
isAcceptable (x1:x2:x3:_) = True
isAcceptable _ = False

tooShortToReturn :: Target -> Bool
tooShortToReturn = not . isAcceptable . reverse . unpack

applyStem :: Target -> Target
-- For Debug...
-- applyStem t | trace ("[applyStem] " ++ show t) False = undefined
applyStem x = do
  let matches = filter (match x) rules
  applyIfHasMatches matches x


stuff x = filter (match x) rules

applyIfHasMatches :: [Rule] -> Target -> Target
applyIfHasMatches [] target = Final . unpack $ target
applyIfHasMatches matches target = applyMatches matches target

applyMatches :: [Rule] -> Target -> Target
-- applyMatches r t | trace ("[applyMatches] " ++  show r ++ " " ++ show t) False = undefined
applyMatches [] target = applyStem target
applyMatches (x:[]) target = do  
    let s = unpack target
    let target' = apply x s 
    if (tooShortToReturn target') then
      Final s
    else
      applyStem target'
applyMatches (x:xs) target = do  
    let s = unpack target
    let target' = apply x s 
    if (tooShortToReturn target') then
      Final s
    else
      applyMatches (filter (match target') xs) target'

apply :: Rule -> String -> Target
apply (Protect s) t = Final t 
apply (Intact s) t = Final $ drop (length s) t
apply (ReplaceAndStop s s') t = Final $ s' ++ (drop (length s) t)
apply (ReplaceAndContinue s s') t = Modified $ s' ++ (drop (length s) t) 

match :: Target -> Rule -> Bool
match (Final _) _ =  False
match (Modified _) (Intact _) =  False
match target rule = startsWith (unpackRule rule) (unpack target)


startsWith = List.isPrefixOf


-- Silly utility functions: there must be an easier way
unpack :: Target -> String
unpack (Modified s) = s
unpack (Unmodified s) = s
unpack (Final s) = s

unpackRule :: Rule -> String
unpackRule (Intact s) =  s
unpackRule (Protect s) =  s
unpackRule (ReplaceAndContinue s _) =  s
unpackRule (ReplaceAndStop s _) =  s

stemList
  = sort . nub . map stem

-- We have have an indicator like "... Norway leader's X" where X
-- is the definition. So we must remove apostrophes.
-- There are a few special (and useful) casese...
stem :: String -> String
stem "doing"
  = "do"
stem "going" 
  = "go"
stem "letters"
  = "lett"
stem s
  | length s <= 3 
     = s
  | otherwise     
     = (reverse . unpack . applyStem . Unmodified . reverse) s'
  where
    s' = Utilities.removeApostrophe s


