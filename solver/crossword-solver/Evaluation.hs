module Evaluation where

import Debug.Trace
import Data.List 
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Array.IArray
import Data.Char

import Utilities
import CacheFunctions
import ParseTreeFilters
import Types
import Databases
import Constants
import BoundsFunctions

-------------------------------------------------------------------
--
-- THE EVALUATOR
-- Only backwards evaluation is used (evalB). Forwards evaluation is avoided
-- by using wildcards: '*' for take-outs and '#' for anagrams -- see the
-- respective rules. The target strings (s) can thus contain wildcards
-- but result strings must not. Furthermore, charade results contain
-- spaces between the components. This can help in the indirect subtext,
-- particularly those involving reversals.
--
-------------------------------------------------------------------
-- 
-- The parser cache (pCache) is passed into the evaluator to get 
-- fast access to text fragments, synonyms etc.
--
-- The solver works backwards by string matching. 
-- The evaluation cache can optionally be used (evalCacheOn).
-- To use the cache, at the topmost level, we index the cached tree 
-- at (i, j, k) using the candidate solution string.
-- Note: if an answer is supplied then the cache won't work, as it's
-- built using a different set of parses (i.e. assuming no answer).
-- So we can only use the cache if null answer. You won't need the 
-- cache anyway in that case (it's very fast) so nothing is lost.
--
-- When picking out the candidate solutions (each of length n)
-- it's important to use synonyms s, rather than the pCache 
-- synonym table, because the definition text could be, e.g.
-- (2,4)="animal for example", which only resolves if we treat
-- the text as a hyponym. In this case s will be "animal". 
-- If an answer is supplied then we just use that as the (single)
-- candidate.
--
-- To speed up evaluation we precompute the candidates for each 
-- parse remove those that do not contain 'must-have' characters,
-- specifically those appearing in anagram arguments and other 
-- raw text constructions. Note: mh is retained but not used 
-- at this top level.
--
evaluate :: [Parse] -> ParserCache -> Clue -> String -> Bool ->
            [Solution]
evaluate ps pCache clue@(clueText, n) answer evalCacheOn
  | null answer = filter isOK orderedSolutions
  | otherwise   = filter isCorrectAndOK orderedSolutions
  where
    orderedSolutions = filter isNonSyn solutions ++ filter isSyn solutions 

    clueLen = length textWords

    textWords = words (clueText)

    ps' = map (processMustHaveLetters pCache n answer textWords) ps

    solutions
      = [(d, ind, t, rt) |
          let ts = zip [0..] ps',
          let nTrees = length ps,
          (tnum, (p@(d, ind, t@(((i, j, k), _), _)), mh, cands)) <- ts,
          let evalCandidate cand = if evalCacheOn && null answer 
                                   then indexEvalCache evCache i j k cand 
                                   else evalB cand t UseAllSyns evalCacheOn nullAnagramText,
          rt <- trace (show tnum ++ " " ++ show nTrees) $
                --
                -- Uncomment one of the following. The first uses the cache;
                -- the second doesn't. The False argument ensures that all
                -- recursive invocations work with the cache turned off.
                -- 
                concat [evalCandidate cand | cand <- cands]]

    isSynTree t = isSynonymTree t || isHyponymTree t
 
    isNonSyn :: Solution -> Bool
    isNonSyn (_, _, pt, _)
      = not (isSynTree pt)

    isSyn :: Solution -> Bool
    isSyn (_, _, pt, _)
      = isSynTree pt

    isOK :: Solution -> Bool
    isOK (d, _, pt, _)
      = not (isSynTree pt) ||
        isSynTree pt &&
          length (activeDefWords defStr) <= maxNoOfActiveWords 
      where
        defStr = getString pCache d

    isCorrectAndOK :: Solution -> Bool
    isCorrectAndOK sol@(_, _, _, R (s, _))
      = filter (/= ' ') s == answer && isOK sol

    nullAnagramText :: AnagramText
    nullAnagramText = array (0, -1) []

    -- 
    -- Build the evaluation cache. This runs evalB with the cache indexing
    -- turned on (cacheOn = True). We pass evalB in, as it's otherwise not
    -- scope.
    -- Lazy evaluation means that the cache won't be built or used if 
    -- caching is turned off.
    -- 
    evCache :: EvalCache
    evCache 
      = makeEvalCache clue answer ps' pCache evalB

    --
    -- getBounds picks out the right bounds when splitting text for
    -- insertions and charades.
    -- Raw text is always included in the text synonyms, so the bounds for
    -- text will always capture the length of the raw text itself.
    -- During parsing the singular synonym and plain synonym bounds are
    -- essentially unioned, so only one bound is needed to cover both cases.
    -- See the Synonym parsing rule.
    -- UseRawText isn't actually used here, but it's available as an option.
    --
    getBounds expander
      = case expander of
          UseRawText      -> snd . snd . snd
          UseTextSyns     -> snd . snd . snd
          UseAllSyns      -> fst . snd . snd
          UseSingularSyns -> fst . snd . snd

    --
    -- This governs how synonyms are expanded during evaluation.
    --
    getExpansionTable pCache i expander
      = case expander of
          UseTextSyns     -> getTextSynonymTable pCache i
          UseAllSyns      -> getSynonymTable pCache i
          UseSingularSyns -> getSingularSynonymTable pCache i

    -------------------------------------------------------------------
    -- Evaluation rules...
    -------------------------------------------------------------------
    --  
    -- Used for recursive evaluation of subtrees. If the cache is turned
    -- on then we index the cache; otherwise we evaluate recursively.
    -- See below for an explanation of the arguments.
    --
    evalBProxy s t@(((i, j, k), _), _) expander cacheOn anagramText
      | cacheOn   = indexEvalCache evCache i j k s
      | otherwise = evalB s t expander cacheOn anagramText

    --
    -- The string is the target string (initially the candidate solution).
    -- This may contain wildcards ('*') if we're evaluating under a take-out
    -- and/or '#' placeholders if we're under an anagram.
    -- Importantly, anagrams are not nested, so we do not need to thread 
    -- anagramText through the code. 
    -- The synonym expansion rule initially expands text into all its synonyms,
    -- but this may be refined, e.g. when evaluating trees that sit under 
    -- anagrams. The bool states whether the evaluation cache is on or off.
    -- The anagramText is used when evaluating under an anagram, in which case
    -- it contains a frequency count of the letters in the anagram string.
    -- These are used to filter out invalid matches (see below).
    -- Note: s, expander, cacheOn, len and lengthCon are in scope everywhere 
    -- in evalB'. This saves carrying round superfluous arguments.
    -- Note: The target string (s) could be null if a charade split happens
    -- with (0, 0) bounds, e.g. middle letters of "my" has (0, 0) bounds, as
    -- its only synonym is itself.
    --
    evalB :: String -> ParseTree -> SynExpansionRule -> Bool -> AnagramText ->
             [ResultTree]
    evalB "" _ _ _ _
      = []
    evalB s (_, t) expander cacheOn anagramText
      = evalB' t
      where
        len = length s
        revs = reverse s
        --
        -- A useful helper function that does all string matching between
        -- a target string and strings generated by text/synonym expansions.
        -- It's important to use s' in the result, as the target string may 
        -- contain wild cards (*) or anagram letter placeholders (#).
        --
        find :: String -> [String] -> [ResultTree]
        find s strs
          = map (\s' -> R (s', [])) (filter (eq s) strs)
 
        --    
        -- Strings may contain wildcards. Note that eq matches words
        -- containing entirely wild cards, e.g. **** and "bake". For subtext
        -- this might lead to noise words being matched but the subtext rules
        -- double check this (see below).
        -- If we find a '#' then this quick filter checks that c' is a member
        -- the anagramText argument to evalB. This may yield false positives,   
        -- but it saves threading an updated frequency table through the eval 
        -- function. False positive are removed by haveSameLetters in the 
        -- anagram rules. 
        --
        eq (c : cs) (c' : cs')
          | c == c'   = eq cs cs'
          | c == '*'  = eq cs cs'
          | c == '#' && isElem c' anagramText = eq cs cs'
          | otherwise = False
        eq [] []
          = True
        eq _ _ 
          = False

        encodeChar :: Char -> Int
        encodeChar c
          | c == ' '  = 26
          | c == '-'  = 27
          | c == '\'' = 28
          | c >= 'a' && c <= 'z' = ord c - ord 'a'
          | otherwise = 0

        -- We just care that the char is in the table, ignoring the actual
        -- frequency count at this point.
        isElem :: Char -> AnagramText -> Bool
        isElem c a
          = a ! (encodeChar c) > 0

        --
        -- Used in the anagram evaluation rules. 31 is a magic number!
        --
        makeAnagramText :: String -> AnagramText
        makeAnagramText s
          = accumArray (+) 0 (0, 31) [(encodeChar c, 1) | c <- s]

        ---------------------------------------------------------------------
        --
        -- Subtext evaluation: this is by far the messiest part! Much of the
        -- code is dedicated to supporting indirect subtext, where a take-out
        -- is applied to a synonym of some fragment of clue text.
        --
        -- The evaluation rules that request text synonyms and
        -- singular synonyms do not allow subtext arguments. Also, UseRawTex
        -- is not currently used, so here the expander will be UseAllSyns,
        -- or UseTextSyns if we're under an anagram. However, in that 
        -- case we behave as if we were asked to UseRawTex as we don't want
        -- anagrams of synonym expansions of any sort.
        -- The UseRawText case is preserved as we may (well) want to 
        -- turn off indirect take-outs.
        -- The matcher depends on the subtext case (first/last/middle/end
        -- letters).
        -- The tree is either a synonym/hyponym, charade tree or a reversal
        -- thereof. 
        -- There are two notions of match: map match (M), e.g. taking the first 
        -- letters of every word, and concat match (C), e.g. concatentating all
        -- words and then discarding the last few letters of the last word
        -- to yield the first letters of the concatenation. Similarly for last,
        -- middle and end cases. Whatever happens, all words must play a part
        -- in the take-out, otherwise we have a spare word in the clue, which is
        -- not permitted.
        -- Matches can apply to raw text or synonym expansions of all words
        -- in the text.
        -- Synonym expansions must consume a 'substantial'
        -- proportion of the text returned, e.g. remain mostly -> RESt, or
        -- bed vacated, case dismissed -> bUNKEMPTy. Otherwise you can get 
        -- stupid solutions involving, e.g. single letter
        -- take-outs of long synonyms, like eradiCation -> c. Some
        -- constants dictate the rules, e.g. if we need a 3-letter word we
        -- may want to generate only 4-letter words for the take-out.
        -- getSubtextSynLengthBounds delivers the required bounds and is
        -- target length dependent.
        -- Warning: Indirect subtext maps may lead to strange solutions, e.g. 
        -- article quill -> the pen -> then (using end letters).
        --
        -- We give up synonym expansions if there are more than a specified 
        -- number of subtrees, as defined by a constant. Two
        -- is about right and allows the 'unkempt' clue to be solved. 
        --
        -- To enable all evaluation to go backwards we insert wildcards. The
        -- rules are messy for the map case where we take a set of words, 
        -- synonym-expand them and then do the take-out. The wildcards MAY
        -- get split up yielding incorrect solutions, so it's important to 
        -- to apply matcherM and matcherC even though they appear not to be 
        -- necessary, e.g. cal ifor nia -> *cal* **ifor** *nia* looks like it
        -- will pick out middle letters of three synonyms, but the charade
        -- split might yield *ca l***if or***nia* and these might all
        -- happen to match and the result would NOT then be a middle-letter
        -- take-out.
        --
        -- Edge case: if we want subtext of a single word with expander ==
        -- UseAllSyns then we must be sure to include the raw text itself.
        -- The trouble is that when you replace text with a synonym we are
        -- only allowed to discard a certain number of letters, depending on the
        -- length of the target string. If the raw text doesn't satisfy this
        -- constraint (look at the number of wildcards added) then we won't
        -- ever use the raw text, e.g. first offspring should be 'o', but to
        -- end up with 'o' we can only start with a two-letter word (say).
        -- For this reason we consider the raw text case separately and it's
        -- useful to do this as it's then easy to turn off synonym expansions
        -- for evaluation purposes (just comment out calls to concatMatches and 
        -- mapMatches). They can be expensive (try the 'trenchant' clue). 
        -- An alternative is to manufacture the right wildcards for
        -- the raw text case to work, but this gets very messy. In particular
        -- we would then need to account for '#' characters universally; this
        -- is OK, but you have to know where you added the '*' wildcards so
        -- you construct the right result string (here we just use s, but we
        -- can't do that if it contains '#'). It's doable, and is best done
        -- by computing wildcard indices and using add/removeWildcards in
        -- Utilities. However it will be super-slow compared to the optimised
        -- code dealing with raw text. 
        --
        -- If s contains '#' characters, i.e. we're sitting under an anagram 
        -- then we resort to *generating* first/last/middle/end letters and then 
        -- checking that each letter is in the anagram text. It's not ideal,
        -- but it's an edge case and the alternative is a much more complicated
        -- set of matching functions that also return bindings for all the '#' 
        -- characters (a sort of unification). Really messy and a lot of work!
        -- '#' characters are not an issue for UseAllSyns as we don't allow
        -- indirect anagrams, let alone anagrams of indirect take-outs.
        --
        -- Note that the text strings of Hyponym is null - hence the rather 
        -- odd null test.
        --  
        -- If there is a single target word for the take-out (synonym and  
        -- example cases) then we want the take-out to be symmetric for middle
        -- and end letters. We could choose either matcher as they do the same
        -- thing for single words. We choose the map matcher. Note that 
        -- middle class -> a or las, but not s.
        --
        -- REMARK...
        -- After *much* experimentation the raw text special case seems a good 
        -- idea, despite the fact that it lengthens the code and introduces the 
        -- nasty '#' edge case.
        --
        -- UPDATE: THe code here will deal with indirect take-outs across
        -- multiple words, e.g. ("Messy bed vacated, case dismissed", 7) 
        -- i.e. UNKEMPT, but these are now restricted via the predicates
        -- in concatMatches and mapMatches to *single-tree*
        -- constructions, e.g. ("endless attempts to capture british...", 5), 
        -- i.e. TRIBE. With these in, we can't solve for UNKEMPT.
        --
        evalSubtextB t matchType matcherM matcherC generator
          | True = rawTextMatches
          | expander == UseTextSyns
            = rawTextMatches
          | isSynonymTree t || isHyponymTree t 
            = rawTextMatches ++ mapMatches 
          | otherwise
            = rawTextMatches ++ concatMatches ++ mapMatches 
          where
            rawTextMatches
              = [R (s'', [rt]) | 
                  let (strings, rt) = evalTextStrings t,
                  let s' = map removeHyphens strings, 
                  not (null s'),
                  s'' <- if elem '#' s
                         then filter allInAnagramText (generator s' (len, len))
                         else if matcherC s s' || matcherM s s'
                              then [s]
                              else []]
              where
                allInAnagramText s 
                  = and (map (flip isElem anagramText) s)
                --
                -- This is essentially a forwards solver, for extracting
                -- the leaves of t as a list of raw text strings.
                --
                -- Edge case: If the string is, e.g. "the milk" then
                -- we want the target text to be "milk", as "the" is 
                -- a noise word. The parser will choose to prefix "the" to
                -- "milk" rather than postfix it to the previous indicator,
                -- which would give a poor explanation, e.g. "axes the
                -- milk", where "the" should be attached to "milk", not 
                -- "axes". So if we spot a noise word prefix we lop 
                -- it off. Note that leaving it to the indirect matcher
                -- might work, but you may get caught out by the 
                -- constraints on the number of wildcards. For example
                -- it so happens that "[top of] [the milk]" does not
                -- resolve to "m" for that reason. 
                --
                evalTextStrings ((_, (SpareWord sw _, _)))
                  = ([sw], R (sw, []))
                evalTextStrings ((_, (Synonym txt, _)))
                  | length ws == 2 && elem w prefixNoiseWords 
                    = ([w'], R (w', []))
                  | otherwise 
                    = ([s'], R (s', []))
                  where
                    s' = getString pCache txt
                    ws = getWords pCache txt
                    (w : w' : _) = ws
                evalTextStrings ((_, (Hyponym _ _, _)))
                  = ([], R ("", []))
                evalTextStrings ((_, (Reversal _ t, _)))
                  = (revStrs, R (reverse (unwords strs), [rt]))
                  where
                    (strs, rt) = evalTextStrings t
                    revStrs = reverse (map reverse strs)
                evalTextStrings ((_, (Charade _ ts, _)))
                  = (s', R (unwords s', map snd rs))
                  where
                    rs = map evalTextStrings ts
                    s' = concatMap fst rs

            subtreeBounds 
              = getBounds' t
              where
                getBounds' (_, (Reversal _ t, _))
                  = getBounds' t
                getBounds' (_, (Charade _ ts, _))
                  = concatMap getBounds' ts
                getBounds' t
                  = [getBounds expander t]
            
            numSubtrees
              = length subtreeBounds

            --
            -- For first and last letters we never do a type-C
            -- takeout from two words, e.g. alpha yield --> ayield.
            -- The updated code actually supersedes this with the
            -- requirement for indirect takeouts to come from a
            -- single (non-charade) tree.
            -- To undo this just do evalMatches always, or 
            -- as an otherwise under the first rule.
            --
            concatMatches 
              | isFirstOrLastLettersTakeout matchType && 
                numSubtrees >= 2
                = []
              | numSubtrees == 1
                = evalMatches concatWildcards matcherC
              | otherwise 
                = []
              where
                isFirstOrLastLettersTakeout First
                  = True
                isFirstOrLastLettersTakeout Last
                  = True
                isFirstOrLastLettersTakeout _
                  = False

            --
            -- Ditto with take-outs across multiple words
            -- They are forbidden by the guard below.
            --
            mapMatches 
              | numSubtrees == 1
                = evalMatches mapWildcards matcherM
              | otherwise 
                = []

            evalMatches wildcardIndices matcher
              = [R (s''', [rt]) |
                  numSubtrees <= maxNumSubtextSubtrees,
                  is <- wildcardIndices,
                  let ws = addWildcards is s,
                  rt@(R (s', _)) <- evalBProxy ws t expander False anagramText,
                  let s'' = removeSpaces s',
                  let s''' = removeWildcards is s'',
                  matcher s''' (map removeHyphens (words s'))]

            --
            -- This implements concat matching, where the target words are
            -- seen as a single string, e.g. first bath circle cat ->
            -- first spa ring lynx -> sparingly. However, all words must 
            -- be used in part.
            -- We know that minLen >= len, e.g. if l = 6 and s = "cat" then 
            -- we need to add at least two wildcards.
            -- nw (no. of wildcards to add) is initially bounded by the length
            -- bounds of the tree, but also taking into account the min and max 
            -- lengths allowed for synonyms expansions. This may later be
            -- refined in cases where either the first or last word generated
            -- from a charade will be excluded from the match. E.g. if the  
            -- leftmost charade subtree can generate up to 4 letters (u1=4)
            -- and nw is 10 then we want at most 3 of the wildcards to match
            -- words generated from it (*spe, **sp, ***s, for Last say, but
            -- not ****).
            -- If there is no charade tree subtreeBounds will be singleton and u1 
            -- is set to maxWordLength. A similar rule applies at the rightmost
            -- end (u2).
            --

            concatWildcards 
              = concat [add nw | 
                          let (minLen, maxLen) = getSubtextSynLengthBounds len,
                          nw <- [max (minLen - len) (l - len) .. 
                                 min (maxLen - len) (u - len)]]
              where
                (l, u) = getBounds expander t
                add nw
                  = case matchType of
                      First  -> [[(len, min nw (u2 - 1))]]
                      Last   -> [[(0, min nw (u1 - 1))]]
                      Middle -> [[(0, min k (u1 - 1)), 
                                  (len, min (nw - k) (u2 - 1))] |
                                   k <- [1 .. nw - 1]]
                      End    -> [[(k, nw)] |
                                  k <- [1 .. min (len - 1) (u1 - 1)]]
                  where
                    (u1, u2) = case subtreeBounds of
                                 [_] -> (maxWordLength, maxWordLength)
                                 ((_, u1) : bs) -> (u1, snd (last bs)) 

            --
            -- This implements map matching, where the match is applied to each
            -- target word separately, if there is more than one, e.g. 
            -- centrally archive pine, for example -> centrally log tree ->
            -- ore, implemented by matching against *o* *re*.
            -- The number of wildcards is carefully controlled by the length 
            -- bounds and the rules about 'substantial' subtext being taken out.
            -- getSubtextSynLengthBounds tells us how many letters we must 
            -- generate for a takeout to be valid.
            -- Recall len is the length of the string we're trying to match
            -- against.
            --
            mapWildcards
              = concat [add lengths subtreeBounds | 
                         let bs' = map (\(l, u) -> (1, u - d)) subtreeBounds,
                         lengths <- getSplitLengths len bs']
              where 
                d = [1, 1, 2, 2] !! (fromEnum matchType)
                add lengths bs
                  = map concat (sequence (zipWith add' lengths bs))
                  where
                    add' len' (l, u) 
                      = case matchType of
                          First  -> [[(len', n)] | n <- [l' .. u']]
                          Last   -> [[(0, n), (len', 0)] | n <- [l' .. u']]
                          Middle -> [[(0, n), (len', n)] | 
                                      n <- [(max 2 l') `div` 2 .. u' `div` 2]]
                          End    -> [[(len'2, n), (len'2, 0)] | even len', n <- [l' .. u']]
                      where
                        --
                        -- l' is the minimum number of wildcards to add to the
                        -- substring (substr).
                        -- E.g. if l=5 and substr="car" then we need at least 2.
                        -- u' is likewise the maximum. If s="cargo" and
                        -- substr="car" and maxLen=9 then we can have a TOTAL
                        -- of 9-5=4 wildcards at most; but we must retain at 
                        -- least d for "go", e.g. d=1 for First. That means u'=3 
                        -- in that case.
                        --
                        l' = max 1 (l - len')
                        u' = min (maxLen - len') (u - len')
                        len'2 = len' `div` 2
                        (minLen, maxLen) = getSubtextSynLengthBounds len'
                    nSubtrees = length subtreeBounds

        ---------------------------------------------------------------------
        -- Compared to the above, the rest is easy!
        ---------------------------------------------------------------------
        -- 
        -- Evaluation rules.
        --
        -- The evalBProxy(uations) are used when we have a recursive subtree.
        -- The call to evalBProxy will either use the cache or will recurse, as
        -- determined by the expander.
        --

        --
        -- An easy one - the target word is verbatim text...
        --
        evalB' (SpareWord w _, _)
          = find s [w]
     
        --
        -- It's here that we use the expander to pick out the required table,
        -- or just use the raw clue text, as needed.
        -- If there are no wildcards then we can just use
        -- Set.member to look for the match. 
        -- Recall: <!> delivers the list of synonyms and
        -- <!!> delivers the list as a Set.
        --
        evalB' (Synonym ix@(i, j), _)
          | expander == UseRawText = find s [text]
          | containsWildcard s     = if (h == '#' || h == '*') && post == "'s"
                                     then find s' (table <!> (len - 1)) ++ rest
                                     else rest
          | otherwise              = res
          where
            h = head revs
            s' = reverse (tail revs)
            rest = find s (table <!> len)
            res = if Set.member s (table <!!> len) ||
                     h == 's' && post == "'s" &&
                     Set.member s' (table <!!> (len - 1))
                  then [R (s, [])]
                  else []
            text = getString pCache ix
            (_, post) = break (== '\'') text
            table = getExpansionTable pCache ix expander
            containsWildcard s = elem '*' s || elem '#' s

        --
        -- Hyponym is just an indirect synonym. We can call evalB' safely
        -- when the cache is on, as the Synonym rule does not index the cache.
        --
        evalB' (Hyponym _ txt, bs)
          = evalB' (Synonym txt, bs)
        --
        -- E.g. short time -> t, small jumper -> roo
        -- This is a backwards version that uses an inverted abbeviations
        -- table to map s (e.g. roo) to its unabbreviated form (e.g. kangaroo).
        -- We then try to find the unabbreviated form in a synonym of the clue
        -- text at index i, e.g. "jumper".
        --
        evalB' t@(Abbreviation _ i, _)
          = [R (s, [R (s', [])]) |
               s' <- fromMaybe [] (Map.lookup s invertedAbbreviationsTable),
               let len' = length s',
               Set.member s' (table <!!> len')]
          where
            table = getExpansionTable pCache i expander

        --
        -- Anagrams are solved by replacing the target string with a string
        -- of '#' characters. The frequency count of the letters of the 
        -- target string are recorded in an array (type AnagramText). This 
        -- could be threaded through the solver to filter rogue solutions,
        -- but we shortcut that here and simply pass it to evalB via evalBProxy.
        -- This is then used (in find) to prune words that contain letters
        -- NOT in the array. This can yield false positives
        -- e.g. if s is "caster" then "starts" would match, so these are
        -- removed by the haveSameLetters check at the end.
        -- Anagrams are not nested, so there is no need to thread the array
        -- through the evalB' function. This would be much easier in a 
        -- non-functional language!
        -- 
        -- Infix anagrams, e.g. Z mixed with Y, may as well use a temporary
        -- charade tree. The index of the temporary tree is unused, so 
        -- undefined is as good as any.
        -- It's safe to return s, as there are no nested anagrams, and anagrams
        -- will never sit under take-outs, so s cannot contain wildcards.
        --
        evalB' (Anagram i t, _)
          = [R (s, [rt]) | 
              len >= minAnagramTextLength,
              let anagText = makeAnagramText s,
              rt@(R (s', _)) 
                <- evalBProxy (replicate len '#') t UseTextSyns False anagText,
              haveSameLetters s s']
        evalB' (AnagramInf i t t', bs)
          = [R (s, [rt, rt']) | 
              len >= minAnagramTextLength,
              let anagText = makeAnagramText s,
              (R (s', [rt, rt'])) 
                <- evalBProxy (replicate len '#') cTree UseTextSyns False anagText,
              haveSameLetters s s']
          where
            cTree = (((0,0,0),0), (Charade noInd [t, t'], bs))
        evalB' (Odds _ i, _)
          = find s [odds (filter (/= '-') $ getMergedString pCache i)]
        evalB' (Evens _ i, _)
          = find s [evens (filter (/= '-') $ getMergedString pCache i)]
        --
        -- The first matcher function is the map version, 
        -- e.g. matcher "cat" ["come", "and", "try"] -> True
        -- The second is the concat version, 
        -- e.g. matcherC "cats" ["cat", "strays"] -> True
        -- The enumeration (First, ...) is passed so we know how to add
        -- wildcards. We could avoid this by passing lots more parameters
        -- but this seems tidier.
        -- Note that hidden words are just instances of middle-letter 
        -- take-outs so a separate rule is not required.
        --
        evalB' (FirstLetters _ t, _)
          = evalSubtextB t First 
              matchesFirstLetters matchesFirstLettersC genFirstLetters
        evalB' (LastLetters _ t, _)
          = evalSubtextB t Last 
              matchesLastLetters matchesLastLettersC genLastLetters
        evalB' (MiddleLetters _ t, _)
          = evalSubtextB t Middle 
              matchesMiddleLetters matchesMiddleLettersC genMiddleLetters
        evalB' (EndLetters _ t, _)
          = evalSubtextB t End 
              matchesEndLetters matchesEndLettersC genEndLetters
        --
        -- Note: This must be evaluated by recursion (no cache lookup), as
        -- we need to use the singular synonym table when evaluating t, 
        -- e.g. two trees => lookup of "tree".
        -- The bounds (bs) is that of the subtree (t) assuming 
        -- singular synonym expansions. The bounds of the duplicate tree 
        -- itself will be double that of bs. We can't use the evaluation
        -- cache as it was built assuming AllSyns expansions, not singular
        -- synonyms.
        -- Again, note that the result string must be made from s' and s''
        -- as s may contain wildcards.
        --
        evalB' (Duplicate i _ t, _)
          = [R (s' ++ s'', [rt, rt']) |
              (s1, s2) <- split2mn s bs bs, 
              rt@(R (s', _))  <- evalBProxy s1 t UseSingularSyns False anagramText, 
              rt'@(R (s'', _)) <- evalBProxy s2 t UseSingularSyns False anagramText]
          where
            bs = getBounds UseSingularSyns t
        --
        -- A homophone subtree is a synonym.
        -- Homophones are symmetric, so we simply generate the
        -- homophones of the target text (s) and proceed backwards.
        -- This only works for single-word homophones. In principle,
        -- beau thai could map to bowtie, but bowtie has no 
        -- homophones, so we can't use symmetry if we were to 
        -- allow charade arguments.
        -- Note: You can't use the cache because the matcher used to build
        -- the cache doesn't do homophone expansions.
        -- It's safe to return s as homophones can't sit under anagrams
        -- or take-outs.
        --
        evalB' (Homophone i t, _)
          = [R (s, [rt]) |
              s' <- homophones s,
              rt <- evalBProxy s' t expander False anagramText]
        --
        -- We have to return the reversal of s', as s itself may
        -- contain wildcards. This is because reversals can sit under
        -- subtext, which is where wildcards are introduced. See the 
        -- subtext evaluation rule above.
        --
        evalB' (Reversal i t, _)
          = [R (reverse s', [rt]) | 
              rt@(R (s', _)) 
                <- evalBProxy (reverse s) t expander cacheOn anagramText,
              not (isPalindrome s'),
              hasTwoLettersOrMore s']
        evalB' (Rotation _ t, _)
          = [R (s, [rt]) |
              s' <- rotate s,
              rt <- evalBProxy s' t expander cacheOn anagramText]
        -- 
        -- The subevaluations will either use the cache or will recurse, as
        -- determined by the expander.
        -- The use of split3mn means that only splits of the right size are 
        -- generated.
        -- It's safe to return s as insertions don't appear under anagrams or
        -- take-outs. This is fortunate, otherwise we would not immediately
        -- know how to insert the rt result into that of rt'.
        --
        evalB' (Insertion i i' t t', _)
          = [R (s, [rt, rt']) |
              (s1, s2, s3) <- split3mn s (getBounds expander t) 
                                         (getBounds expander t'),
              rt  <- evalBProxy s2 t expander cacheOn anagramText, 
              rt' <- evalBProxy (s1 ++ s3) t' expander cacheOn anagramText]
        --
        -- We add wildcards where there might be subtracted text. This is
        -- essentially a union of the take-out rules for contiguous text
        -- but the wildcards can appear in any position, not just in 
        -- pre/post/mid positions.
        -- The length of the text to subtract is governed by a constant,
        -- for efficiency reasons.
        -- E.g. s = cart -> car**t. Match with t' -> carPEt.
        -- Now match PE with t. k and nw indicate where the ** has
        -- been added.
        -- Works also if s contains wildcards, e.g. s = #### -> ###**#. 
        -- Match with t' -> carPEt (anagramText could be 'tracking'
        -- for example so 'cart' matches OK). Now match PE with t and 
        -- return cart if successful. Note that the result has 
        -- to be constructed explicitly from s', rather than simply
        -- returning s, as we can't return text with wildcards.
        -- Note that (similar to take-outs above) wildcards may match
        -- entirely one word, e.g. (clue 22) s = entity -> ****entity
        -- matches the charade 'tang entity', which would be invalid.
        -- The result is valid if it conforms to one of the concat 
        -- matches (C) used in take-outs (see above), as subtractions can
        -- remove any contiguous text from the target (k <- [0 .. len]).
        -- Note: s' may contain spaces, so these need to be removed to form 
        -- the argument to match with t.
        -- Subtractions can appear under anagrams so we have to build the
        -- result string (s'''') explicitly as s may contain wildcards.
        -- Note that subtracted text can appear anywhere in a single word,
        -- e.g. nuTs -> nus, but must span all words in a multi-word target, 
        -- e.g. timE TO Natter. So we use matchesMiddleLettersC if there is
        -- more than one word and matchesAnyMiddleLetters if there's only one.
        --
        evalB' t1@(Subtraction i t t', _)
          = [R (s'''', [rt, rt']) |
              is <- getWildcardIndices (getBounds UseTextSyns t),
              let ws = addWildcards is s,
              rt'@(R (s', _)) <- evalBProxy ws t' expander False anagramText,
              let s'' = removeSpaces s',
              let s''' = extractWildcardText is s'',
              rt <- evalBProxy s''' t UseTextSyns False nullAnagramText,
              isValidSubtraction s''' (map removeHyphens (words s')),
              let s'''' = removeWildcards is s'']
          where
            isValidSubtraction s ss
              = matchesFirstLettersC s ss ||
                matchesLastLettersC s ss  || 
                case ss of 
                  [w] -> matchesAnyMiddleLetters s w
                  ws  -> matchesMiddleLettersC s ws
            (minLen, maxLen) = subtractionLengthLimits
            getWildcardIndices (l, u)
              = [[(k, nw)] |
                  k <- [0 .. len],
                  nw <- [max l minLen .. min (maxWordLength - len) (min u maxLen)]]
        --
        -- The split only generates substrings whose lengths match the
        -- corresponding tree bounds.
        -- We have to concatenate the sub-result strings, as the search
        -- string, s, may contain wildcards. Reversals and synonyms also
        -- have to do likewise. For synonyms, this is done by find.
        --
        evalB' (Charade i ts, _)
          = [R (unwords (map (\(R (s', _)) -> s') rts), rts) | 
              substrs <- splitNopt s (map (getBounds expander) ts),
              let resTrees = zipWith evalBProxy' substrs ts,
              rts <- sequence resTrees]
          where 
            evalBProxy' s t = evalBProxy s t expander cacheOn anagramText


--
-- This starts with the shortest synonyms, e.g. for century
-- the synonyms are c and century, so we begin with c.
-- The function returns the intersection across all synonyms,
-- i.e. "c" in this case.
--
intersectLetters []
  = []
intersectLetters [w]
  = w
intersectLetters (w : ws)
  = process w ws
  where
    process (c : cs) ws
      | all (elem 'c') ws = c : process cs (map (\\[c]) ws)
      | otherwise = process cs ws
    process s _
      = s

letterIntersection :: SynonymTable -> String
letterIntersection table
  = intersectLetters (concatMap (Set.elems . fst)
                         (Data.Array.IArray.elems (snd table)))
--
-- Anagram arguments are evaluated using text expansions only.
-- If there is only one synonyms then those letters must appear
-- in the candidate string. If there's more than one then 
-- we find the intersection. A single function, letterIntersection,
-- handles all cases.
-- Note: we must stope if we're not sure, e.g. we may have an 
-- anagram of a subtraction, in which case all bets are off.
--
processMustHaveLetters pCache n answer textWords p@(d, _, pt)
  = (p, mh, cands)
  where
    mh = mustHaveLetters pt False
    cands = [c | c <- getCandidates pCache d n answer textWords,
                      null (mh \\ c)]

    mustHaveLetters (_, (SpareWord _ w, _)) useSynText
      = w
    mustHaveLetters (_, (Synonym ix, _)) useSynText
      | useSynText = letterIntersection (getTextSynonymTable pCache ix)
      | otherwise  = []
    mustHaveLetters (_, (Hyponym i j, _)) useSynText
      = []
    mustHaveLetters (_, (Anagram i t, _)) useSynText
      = mustHaveLetters t True
    mustHaveLetters (_, (AnagramInf i t1 t2, _)) useSynText
      = mustHaveLetters t1 True ++ mustHaveLetters t2 True
    mustHaveLetters (_, (Abbreviation i j, _)) useSynText
      = []
    mustHaveLetters (_, (Odds i j, _)) useSynText
      = odds (getString pCache j)
    mustHaveLetters (_, (Evens i j, _)) useSynText
      | null s = []
      | otherwise = odds (tail s)
      where
        s = getString pCache j
    mustHaveLetters (_, (FirstLetters i t, _)) useSynText
      = []
    mustHaveLetters (_, (LastLetters i t, _)) useSynText
      = []
    mustHaveLetters (_, (MiddleLetters i t, _)) useSynText
      = []
    mustHaveLetters (_, (EndLetters i t, _)) useSynText
      = []
    mustHaveLetters (_, (Duplicate i s t, _)) useSynText
      = []
    mustHaveLetters (_, (Homophone i t, _)) useSynText
      = []
    mustHaveLetters (_, (Reversal i t, _)) useSynText
      = []
    mustHaveLetters (_, (Rotation i t, _)) useSynText
      = []
    mustHaveLetters (_, (Insertion i i' t t', _)) useSynText
      = mustHaveLetters t useSynText ++ mustHaveLetters t' useSynText
    mustHaveLetters (_, (Subtraction i t t', _)) useSynText
      = []
    mustHaveLetters (_, (Charade i ts, _)) useSynText
      = concatMap (flip mustHaveLetters useSynText) ts

