module Parser where 

import qualified Data.Set as Set
import Data.Array.IArray
import Data.List  
import Data.Maybe
import Debug.Trace

import Types
import ShowFunctions
import IndicatorPredicates
import Utilities
import CacheFunctions
import ParseTreeFilters
import Databases
import Constants
import BoundsFunctions

--
-- The following is useful for debugging.
--
getParseTree :: Parse -> ParseTree
getParseTree (_, _, t)
  = t

-- 
-- Used for filtering out invalid parse trees. (l, u) are the
-- lower and upper bounds of the solution length that the parse
-- tree can generate.
-- Note: This only checks for synonym bounds, not text expansion
-- bounds, but that's OK as the former includes the latter.
--
hasValidLength :: Int -> ParseTree -> Bool
hasValidLength n (_, (_, ((l, u), _)))
  = n >= l && n <= u

--
-- allParses yields the full set of parses, before any 
-- constraints are applied.
--
allParses :: Clue -> ([Parse], ParserCache)
allParses clue@(text, n)
  = parses clue "" NoPruning

allParsesWithAnswer :: Clue -> String -> ([Parse], ParserCache)
allParsesWithAnswer clue@(text, n) answer
  = parses clue answer NoPruning

--
-- Applies just the length constraint (no pruning of
-- equivalent parse trees)
--
lengthPrunedParses :: Clue -> ([Parse], ParserCache)
lengthPrunedParses clue@(text, n)
  = parses clue "" LengthPruning

lengthPrunedParsesWithAnswer :: Clue -> String -> ([Parse], ParserCache)
lengthPrunedParsesWithAnswer clue@(text, n) answer
  = parses clue answer LengthPruning

--
-- Generates the fully pruned set of parses (True). acceptAnyDef
-- tells the parser whether to accept any definition string. 
-- When evaluating with a known answer we may want to override
-- (i.e. ignore) the definition text.
--
prunedParses :: Clue -> ([Parse], ParserCache)
prunedParses clue@(text, n) 
  = parses clue "" FullPruning

prunedParsesWithAnswer :: Clue -> String -> ([Parse], ParserCache)
prunedParsesWithAnswer clue@(text, n) answer
  = parses clue answer FullPruning

--
-- Various show functions.
--

--
-- The complete set, before any pruning
--
showAllParses :: Clue -> IO()
showAllParses clue@(text, n)
  = mapM_ (putStrLn . flip showP pCache) (zip [0..] ps)
  where
    (ps, pCache) = allParses clue

--
-- The length pruned parses 
--
showLengthPrunedParses :: Clue -> IO()
showLengthPrunedParses clue@(text, n)
  = mapM_ (putStrLn . flip showP pCache) (zip [0..] ps)
  where
    (ps, pCache) = lengthPrunedParses clue

showLengthPrunedParsesWithAnswer :: Clue -> String -> IO()
showLengthPrunedParsesWithAnswer clue@(text, n) answer
  = mapM_ (putStrLn . flip showP pCache) (zip [0..] ps)
  where
    (ps, pCache) = lengthPrunedParsesWithAnswer clue answer

--
-- The fully pruned parses 
--
showPrunedParses :: Clue -> IO()
showPrunedParses clue@(text, n)
  = mapM_ (putStrLn . flip showP pCache) (zip [0..] ps)
  where
    (ps, pCache) = prunedParses clue 

showPrunedParsesWithAnswer :: Clue -> String -> IO()
showPrunedParsesWithAnswer clue@(text, n) answer
  = mapM_ (putStrLn . flip showP pCache) (zip [0..] ps)
  where
    (ps, pCache) = prunedParsesWithAnswer clue answer

--
-- Prints one of the above
--
showPrunedParse :: Int -> Clue -> IO()
showPrunedParse parseNum clue@(text, n)
  = putStrLn (showP (parseNum, ps !! parseNum) pCache)
  where
    (ps, pCache) = prunedParses clue 

showPrunedParseWithAnswer :: Int -> Clue -> String -> IO()
showPrunedParseWithAnswer parseNum clue@(text, n) answer
  = putStrLn (showP (parseNum, ps !! parseNum) pCache)
  where
    (ps, pCache) = prunedParsesWithAnswer clue answer

-------------------------------------------------------------------
--
-- The Parser...
--
-- treeFilter will either be id (allParses) or will filter out trees if 
-- their length bounds are wrong (someParses). This removes parses that can
-- solve.
--
-- We then attempt to rank each by the likelihood of them being
-- the correct parse. The 'quality' function is experimental and tries
-- to penalise complex trees, whilst also favouring the more
-- common constructions, e.g. insertions, anagrams etc. This yields
-- sortedParses. *It must not look at indicators, as two trees with the
-- same structure must have the same score.*
--
-- Next we observe that several parses may have the same tree, differing
-- only in the way noise words are scattered around indicators. So we 
-- arrange the sorted list into groups with the same quality score. If two
-- trees are identical in structure then their parses are guaranteed to have the 
-- same score. For each group we then batch together the parses whose trees have
-- identical structure and pick *one* of them. The heuristic (scoreParse)
-- attaches a higher score to parses with a definition separator; likewise
-- longer indicators are favoured over shorter ones. This tends to pick the
-- tree that gives the best explanation, but any will do. This also serves
-- to prune the parses, although not substantially. 
--
-- To get the best explanatin when the parse trees are unequal we promote some
-- parses based on inspection of the definition and definition indicator. To 
-- avoid a bad parse leapfrogging a good one the promotion function
-- (defProperties) also uses the quality score. This means we compute this
-- twice for each tree, but that could be cached (to do, although it seems
-- to make little difference to the solve time).
--
-- The various quality/scoring functions are at the end of the module.
--
-- The parser cache (pCache) is used during evaluation, so is returned along
-- with the list of Parses.
--

parses :: (String, Int) -> String -> Pruner -> ([Parse], ParserCache)
parses clue@(s, n) answer pruner
  = case pruner of
      NoPruning     -> (filteredParses (const True), pCache)
      LengthPruning -> (filteredParses (hasValidLength n), pCache)
      FullPruning   -> (prunedAndSortedParses, pCache)
  where
    acceptAnyDef = not (null answer)

    eqQ (q, _) (q', _) = q == q'

    --
    -- Duplicate trees add about 10% to the number of parses, but
    -- removing them can take a while. To turn it off just 
    -- replace the concatMap... with sortedParses.
    --
    prunedAndSortedParses :: [Parse]
    prunedAndSortedParses
      = map snd $ promoteParses $
          concatMap prune (groupBy eqQ sortedParses)

    --
    -- If the answer is given then we promote parses whose
    -- definition has a synonym that matches the answer. 
    -- Otherwise we may end up with a bogus explanation when
    -- a better one is known, e.g. ("thought almost perfect",4)
    -- which will solve without answer to give IDEA=thought=
    -- FST[almost] ideal. We want to pick this one
    -- rather than IDEA=perfect=FST thought (thought may resolve 
    -- to IDEAte, for example).
    -- 
    promoteParses :: [(Int, Parse)] -> [(Int, Parse)]
    promoteParses qps
      | null answer = qps
      | otherwise   = sortOn defP qps
      where
        defP (q, p) = defProperties pCache answer p q

    sortedParses :: [(Int, Parse)]
    sortedParses = sortOn fst [(quality p, p) | 
                                p <- filteredParses (hasValidLength n)]

    --
    -- Each element of a group has the same quality score
    --
    prune :: [(Int, Parse)] -> [(Int, Parse)]
    prune qps
      | null answer = qps
      | otherwise   = map pickOne (groupBy g (sortBy f qps))
      where
        g (_, p) (_, p') = sameParse pCache p p'
        f (_, p) (_, p') = compareParses pCache p p'
        pickOne = head .  sortOn (scoreParse pCache answer . snd)

    clueText
      = words (cleanUp s)

    filteredParses treePred
      = [parse | parse@(_, _, t) <- topLevelParses, treePred t]

    --
    -- Cache all parse trees, synonyms tables etc.
    -- The make function is defined in CacheFunctions. The
    -- wordplay parser is passed in, as it's otherwise out of scope.
    --
    pCache :: ParserCache
    pCache 
      = makeParseCache clueText parseWordplay

    ---------------------------------------------------------------------------
    --
    -- Indicator predicates are applied to the stemmed version of 
    -- the clue text...
    --
    isIndicator :: IndicatorTypes -> Index -> Bool
    isIndicator indPred index@(i1, i2)
      | i2 - i1 > maxIndicatorWords = False
      | otherwise = predSet ! indPred  
      where
        predSet = getIndicatorPredicateSet pCache index

    --
    -- Parse with and without indicator...
    --
    topLevelParses :: [Parse]
    topLevelParses 
      = [(fromJust i1', noInd, t) | 
          (i1, i2) <- split2Indices' allIndices,
          let i1' = getDef i1,
          isValid i1',
          t <- getAllTrees pCache i2] ++
        [(fromJust i1', i2, t) | 
          (i1, i2, i3) <- split3Indices allIndices,
          isIndicator DefIndicator i2,
          let i1' = getDef i1,
          isValid i1',
          t <- getAllTrees pCache i3] ++
        [(fromJust i1', i2, t) | 
          (i3, i2, i1) <- split3Indices allIndices,
          isIndicator DefRIndicator i2,
          let i1' = getDef i1,
          isValid i1',
          t <- getAllTrees pCache i3]
      where
        isValid (Just index)
          = n == 1 || n > 1 && not (elem (last ws) badEndWords) 
          where
            ws = getWords pCache index
            n = length ws
        isValid Nothing
          = False
        allIndices 
          = (0, length clueText - 1)
        --
        --
        -- We may filter parses also on the basis of whether we can find a
        -- suitable replacement (synonym) for the definition. We therefore
        -- compute a definition string here that will be used during 
        -- evaluation.
        -- Hyponyms are allowed, e.g. type of boat; cat, for example. We use
        -- the existing parsing rule.
        -- Prefixes like a, the, small, former etc. and words ending
        -- "'s" are all handled by the synonyms/hasSynonym functions, 
        -- e.g. a small animal --> mouse; animal's --> cat; an animal --> dog.
        -- The boolean argument (acceptAnyDef) is used to test the ability 
        -- to recognise wordplay, in which case the candidate definition 
        -- is always accepted, subject to a given maximum length.
        --
        getDef index@(i, j)
          | not (null syns)
              = Just (head syns)
          | not (null hyponymSyns)
              = Just (head hyponymSyns)
          | acceptAnyDef && (j - i + 1) <= maxDefLength
              = Just index
          | otherwise
              = Nothing
          where
            syns 
              = [sIndex | 
                  (_, (Synonym sIndex, _)) <- getSynonymTrees pCache index]
            hyponymSyns 
              = [hIndex | 
                  (_, (Hyponym _ hIndex, _)) <- getHyponymTrees pCache index]
            
    ----------------------------------------------------------------------
    --
    -- Some utilities...
    --
    doubleBounds :: LengthBounds -> LengthBounds
    doubleBounds ((l, u), (l', u'))
      = ((2 * l, 2 * u), (2 * l', 2 * u'))

    -- 
    -- When adding bounds we must preserve negative bounds 
    -- (see subtraction and anagram rules)
    --
    addBounds :: Bounds -> Bounds -> Bounds
    addBounds (l, u) (l', u')
      | u < 0 || u' < 0 = (l + l', -1)
      | otherwise       = (l + l', u + u')

    --
    -- All trees are assigned a unique id: the index in the clue text
    -- (i, j) and the index of parse tree (k) in the list of trees 
    -- generated from that text.
    --
    addIndex :: Index -> Int -> UnindexedTree -> ParseTree
    addIndex (i, j) k (d, t)
      = (((i, j, k), d), t)

    --
    -- These are used to compute bounds when parsing subtext.
    -- Note: If there is a reversal in the argument there will 
    -- be exactly one, at the topmost level.
    --
    foldCharade :: Int -> Int -> ParseTree -> Bounds
    foldCharade m n (_, (SpareWord s _, _))
      = (m, len - n)
      where
        len = length s
    foldCharade m n (_, (Synonym index, _))
      = makeBounds m n index
    foldCharade m n (_, (Hyponym _ index, _))
      = makeBounds m n index
    foldCharade m n (_, (Charade _ ts, _))
      = foldr1 addBounds (map (foldCharade m n) ts)
    foldCharade m n (_, (Reversal _ t, _))
      = foldCharade m n t

    --
    -- The bounds in the four subtext cases correspond to the 
    -- synonym expansion of each word. This also bounds the raw text
    -- case as the synonym of a word includes the word itself.
    -- Synonyms are allowed, subject to the constraints detailed
    -- in the evaluator.
    --
    makeBounds m n index
      | m + n > maxSynLength
        = nullBounds
      | otherwise          
        = (m, maxSynLength - n)
      where
        maxSynLength = snd (fst (getSynonymTable pCache index))

    ----------------------------------------------------------------------
    --
    -- The wordplay parser...
    --
    parseWordplay index@(m, n) 
      = zipWith (addIndex index) [0..] allTrees
      where
        pairs = split2Indices' index
        triples = split3Indices index
        allTrees 
          = parseUnindicatedCharades index ++
            parseUnarySpecialCases index ++
            parseBinarySpecialCases pairs ++
            parseSynonym index ++
            parseHyponym pairs ++
            parseIndicatedAbbreviation pairs ++
            parseAnagram pairs ++
            parseAnagramInf triples ++
            parseOdds pairs ++
            parseEvens pairs ++
            parseFirstLetters pairs ++
            parseLastLetters pairs ++
            parseMiddleLetters pairs ++
            parseEndLetters pairs ++
            parseReversal pairs ++
            parseRotation pairs ++
            parseDuplicate pairs ++  
            parseHomophone pairs ++
            parseInsertion triples ++
            parseInsertion' index ++
            parseSubtraction triples ++
            parseIndicatedCharade triples 

    --
    -- All parse trees have a unique index, which is useful if we want to cache
    -- evaluations later on. Each segment of clue text has a list of associated 
    -- parse trees, indexed from 0. Unindicated charade indices start at 0 and the
    -- remainder start from the next available index (k below).
    --
    -- The depth of each tree is tracked (first element of UnindexedTree
    -- tuple) so deep trees can be pruned before they are formed. Pruning
    -- afterwards is not computationally tractable. Base cases have depth 1.
    --
    parseUnindicatedCharades :: Index -> [UnindexedTree]
    parseUnindicatedCharades index
      = concatMap parseUnindicatedCharades' validPartitions
      where
        validPartitions = filter ((<= maxNumCharadeSubtrees) . length) (partitionsIndices index)
        getArgTrees 
          = getNonCharadeTrees pCache
        parseUnindicatedCharades' p 
          = concatMap (buildCharade index) (sequence (map getArgTrees p))

        buildCharade index ts
          | depthsOK  = [(maximum (map (snd . fst) ts) + 1, 
                         (Charade noInd ts, (bs, bs')))]
          | otherwise = []
          where
            depthsOK = all ((< maxTreeDepth) . (snd . fst)) ts
            bs = foldr addBounds (0, 0) (map getSynBounds ts)
            bs' = foldr addBounds (0, 0) (map getTextBounds ts)
            getSynBounds 
              = fst . snd . snd
            getTextBounds 
              = snd . snd . snd

    --
    -- All synonyms are single words (see makeSynonymTable).
    -- If there is more than one word and no synonym, e.g. "noisy kettle",  
    -- then the charade builder above will build the union of the individual
    -- words, so we just return nothing. If there are synonyms, e.g. "rich
    -- people", then we retain the synonym.
    -- All single words have at least one synonym, as an unknown word is its
    -- own synonym, e.g. "bgxyah" -> "bgxyah". 
    -- Note that, e.g. "rich people" will NOT map to "richpeople" here
    -- because multi-word text has no self-synonym; the text "richpeople"
    -- will be built by the corresponding charade tree "rich" + "people".
    -- Note that, in this case, "rich people" does have at least one
    -- synonym.
    -- The synonym bounds are computed by considering both normal synonym
    -- expansion and the singular equivalent. This saves carrying round a
    -- third set of bounds and will make little material difference to the
    -- efficency of the solver.
    -- For words ending 's we add one to the upper bound, as the synonyms
    -- function (hence table) only gives synonyms for the bit before the 's.
    -- The evaluation essentially adds "s" to the end of each synonym as
    -- a candidate, but it does this by removing any "s" at the *end* of the
    -- search string, e.g. when matching "courts" with "court's" we match 
    -- "court" with synonyms of "court" instead.
    -- Note that if there is no synonym for the index then we generate no
    -- parses: an instant, and universal, prune.
    --
    parseSynonym :: Index -> [UnindexedTree]
    parseSynonym i@(m, n)
      | m == n    = synTree 
      | isEmpty t = []
      | otherwise = synTree
      where
        text = getString pCache i
        (pre, post)   = break (== '\'') text
        correction    = if post == "\'s" then 1 else 0
        synTree       = [(1, (Synonym i, ((min l l', max u u' + correction), bs)))]
        t@((l, u), _) = getSynonymTable pCache i
        ((l', u'), _) = getSingularSynonymTable pCache i
        (bs, _)       = getTextSynonymTable pCache i

    parseUnarySpecialCases :: Index -> [UnindexedTree]
    parseUnarySpecialCases i@(m, n)
      | m == n    = parseSpecialCases1 (getString pCache i)
      | otherwise = []
        
    parseBinarySpecialCases :: Pairs -> [UnindexedTree]
    parseBinarySpecialCases pairs
      = [(d + 1, (Insertion noInd ind t t', (addBounds bs1 bs, addBounds bs1' bs))) | 
           (i1, i2) <- pairs,
           let str = getString pCache i1,
           (ind, suffix) <- splitOnPrefixes insertionPrefixes str,
           t@((_, d), (_, (bs1, bs1'))) <- getInsertionArg1Trees pCache i2,
           d < maxTreeDepth,
           let len = length suffix,
           len >= 2,
           let bs = (len, len),
           let t' = (((0,0,0), 1), (SpareWord suffix ind, (bs, bs)))] ++
        [(d + 1, (Insertion noInd ind t t', (addBounds bs1 bs, addBounds bs1' bs))) |
           (i1, i2) <- pairs,
           let str = getString pCache i2,
           (ind, suffix) <- splitOnPrefixes insertionPrefixes str,
           t@((_, d), (_, (bs1, bs1'))) <- getInsertionArg1Trees pCache i1,
           d < maxTreeDepth,
           let len = length suffix,
           len >= 2,
           let bs = (len, len),
           let t' = (((0,0,0), 1), (SpareWord suffix ind, (bs, bs)))] 
      where
        insertionPrefixes = ["in","into"]
        
    --
    -- These capture embedded indicators for anagrams and reversals, 
    -- hairstyle -> anagram of hair, gadabout -> reversal of gad, 
    -- turncoat -> reversal of coat etc. Because the indicator and
    -- target (text only) don't have indicators we use a special
    -- SpareWord constructor to house both strings as text.
    -- IMPORTANT: SpareWords can't be cached in the evaluation
    -- cache
    --
    parseSpecialCases1 :: String -> [UnindexedTree]
    parseSpecialCases1 s
      = concat [fh, bh, ft, bt]
      where
        n = length s
        fh = map (make Anagram) (concatMap extractHeadString anagrams)
        bh = map (make Anagram) (concatMap extractTailString anagrams)
        ft = map (make Reversal) (concatMap extractHeadString reversals)
        bt = map (make Reversal) (concatMap extractTailString reversals)
        anagrams = ["form", "kind", "mark", "style", "type"]
        reversals = ["about", "around", "back", "round",
                     "turn", "up"]
        extractHeadString :: String -> [(String, String)]
        extractHeadString w
          | n > k && w' == w = [(w, skipHyphen $ drop k s)]
          | otherwise        = []
          where
            w' = take k s
            k = length w
        extractTailString :: String -> [(String, String)]
        extractTailString w
          | n > k && w' == w = [(w, skipHyphen' $ take (n - k) s)]
          | otherwise        = []
          where
            w' = drop (n - k) s
            k = length w
        skipHyphen' s
          = reverse (skipHyphen (reverse s))
        skipHyphen ('-' : s)
          = s
        skipHyphen s
          = s
        make con (ind, s)
          = (2, (con noInd (((0,0,0), 1), (SpareWord s ind, (bs, bs))), (bs, bs))) 
          where
            bs = (len, len)
            len = length s
      
    --
    -- These are just labelled proxies for synonyms.
    -- Note that it doesn't make sense to ask for an example of raw text,
    -- e.g. "Hilary, for example" does not need the raw text case, just
    -- the synonyms of hilary.
    --
    parseHyponym :: Pairs -> [UnindexedTree]
    parseHyponym pairs 
      = [(1, (Hyponym i1 i2, (bs1, bs1))) | 
           (i1, i2) <- pairs, 
           isIndicator HyponymInd i1,
           (_, (_, (bs1, _))) <- getSynonymTrees pCache i2]

    --
    -- The bounds are for the text expansion case (anagrams do not
    -- expand synonyms). Note that if u is negative then there is
    -- a subtraction argument somewhere that cannot be resolved,
    -- e.g. subtract "mistake" "a" -> u = -6. See the subtraction
    -- parsing rule. l is guaranteed to be at least 1.
    --
    parseAnagram :: Pairs -> [UnindexedTree]
    parseAnagram pairs 
      = [(d + 1, (Anagram i1 t, (bs, bs))) | 
           (i1, i2) <- pairs, 
           isIndicator AnagramInd i1,
           t@((_, d), (_, (_, (l, u)))) <- getAnagramArgTrees pCache i2,
           d < maxTreeDepth,
           u > 0,
           let bs = (max l minAnagramTextLength, u)]

    --
    -- Supports infix indicators, e.g. X mixed with Y.
    --
    parseAnagramInf :: Triples -> [UnindexedTree]
    parseAnagramInf triples
      = [(max d d' + 1, (AnagramInf i1 t t', (bs, bs))) |
           (i2, i1, i3) <- triples, 
           isIndicator AnagramInfInd i1,
           t@((_, d), (_, (_, (l, u)))) <- getAnagramArgTrees pCache i2, 
           d < maxTreeDepth,
           u > 0,
           t'@((_, d'), (_, (_, (l', u')))) <- getAnagramArgTrees pCache i3,
           d' < maxTreeDepth,
           u' > 0,
           let bs = (max (l + l') minAnagramTextLength, u + u')]
 
    --
    -- Odds and evens only work on raw text. We skip hyphens.
    --
    -- E.g. odd coupe -> cue, uneven long-term -> lntr
    --
    parseOdds :: Pairs -> [UnindexedTree]
    parseOdds pairs 
      = [(1, (Odds i1 i2, (bs, bs))) | 
           (i1, i2) <- pairs, 
           isIndicator OddsInd i1,
           let s = getMergedString pCache i2,
           let s' = filter (/= '-') s,
           let n = length s',
           let len = (n + 1) `div` 2,
           let bs = (len, len)]

    --
    -- E.g. even castle -> ate
    --
    parseEvens :: Pairs -> [UnindexedTree]
    parseEvens pairs 
      = [(1, (Evens i1 i2, (bs, bs))) | 
           (i1, i2) <- pairs, 
           isIndicator EvensInd i1,
           let s = getMergedString pCache i2,
           let s' = filter (/= '-') s,
           let n = length s',
           let len = n `div` 2,
           let bs = (len, len)]

    --
    -- Subtext rules...
    --
    -- The subtext function may be applied to concatenated
    -- text or to individual words during evaluation.
    --
    -- If the subtree is a reversal then the individual words (wrapped
    -- in a Synonym or a Charade thereof) will be reversed during 
    -- evaluation.
    --
    parseSubtext pairs textCon textInd getSubtextBounds
      = [(d + 1, (textCon i1 t, (bs, bs))) |
           (i1, i2) <- pairs, 
           isIndicator textInd i1,
           t@((_, d), _) <- getSubtextArgTrees pCache i2,
           d < maxTreeDepth,
           let bs = getSubtextBounds t]
    --
    -- E.g. carthage -> cart, star child -> starch, bare seat -> base
    --
    parseFirstLetters :: Pairs -> [UnindexedTree]
    parseFirstLetters pairs 
      = parseSubtext pairs FirstLetters FirstLettersInd firstLettersBounds

    --
    -- E.g. stage -> age, cast each -> teach, bare ends -> reds
    --
    parseLastLetters :: Pairs -> [UnindexedTree]
    parseLastLetters pairs 
      = parseSubtext pairs LastLetters LastLettersInd lastLettersBounds

    --
    -- E.g. silly -> ill, oklahoma terminal -> mate, bare styx -> arty
    --
    parseMiddleLetters :: Pairs -> [UnindexedTree]
    parseMiddleLetters pairs 
      = parseSubtext pairs MiddleLetters MiddleLettersInd middleLettersBounds

    --
    -- E.g. carthage -> cage, stun loop -> stop, bare ends -> bees
    --
    parseEndLetters :: Pairs -> [UnindexedTree]
    parseEndLetters pairs 
      = parseSubtext pairs EndLetters EndLettersInd endLettersBounds

    firstLettersBounds :: ParseTree -> Bounds
    firstLettersBounds 
      = foldCharade 1 1 

    lastLettersBounds :: ParseTree -> Bounds
    lastLettersBounds 
      = firstLettersBounds

    endLettersBounds :: ParseTree -> Bounds
    endLettersBounds 
      = foldCharade 2 1

    middleLettersBounds 
      = foldCharade 1 2

    --
    -- E.g. macs back to front -> scam
    -- 
    parseReversal :: Pairs -> [UnindexedTree]
    parseReversal pairs 
      = [(d + 1, (Reversal i1 t, (bs1, bs2))) | 
           (i1, i2) <- pairs, 
           isIndicator ReversalInd i1,
           t@((_, d), (_, (bs1, bs2))) <- getReversalArgTrees pCache i2,
           d < maxTreeDepth]

    --
    -- Rotations only work on raw text. We skip hyphens.
    --
    -- E.g. iowa circling -> owai, or aiow
    --
    parseRotation :: Pairs -> [UnindexedTree]
    parseRotation pairs 
      = [(d + 1, (Rotation i1 t, (bs1, bs2))) | 
           (i1, i2) <- pairs, 
           isIndicator RotationInd i1,
           t@((_, d), (_, (bs1, bs2))) <- getRotationArgTrees pCache i2,
           d < maxTreeDepth]
    --
    -- E.g. short time -> t, short times -> x
    -- 
    parseIndicatedAbbreviation :: Pairs -> [UnindexedTree]
    parseIndicatedAbbreviation pairs
      = [(1, (Abbreviation i1 i2, (bounds, bounds))) |
           (i1, i2) <- pairs,
           isIndicator AbbreviationsInd i1,
           let s = getString pCache i2,
           let lengths = map length (concatMap abbreviations (synonyms s)),
           not (null lengths),
           let bounds = (minimum lengths, maximum lengths)]

    --
    -- E.g. double gin -> gingin, two circles -> cello etc.
    --
    parseDuplicate :: Pairs -> [UnindexedTree]
    parseDuplicate pairs 
      = [(d + 1, (Duplicate i1 s t, doubleBounds bs)) |
           (i1, i2) <- pairs,
           isIndicator DuplicateInd i1,
           let s = getString pCache i2,
           t@((_, d), (_, bs)) <- getDuplicateArgTrees pCache i2,
           d < maxTreeDepth]

    --
    -- E.g. reported rough -> ruff.
    -- The rule only applies to single synonyms.
    -- e.g. Beau Thai on the radio -> bowtie is not valid.
    --
    -- NOTE: 1 and 11 are hacks. We could instead use the lengths
    -- of the actual homophones, but we would have to allow
    -- for both text and synonym expansions of t to get the 
    -- bounds right.
    --
    parseHomophone :: Pairs -> [UnindexedTree]
    parseHomophone pairs 
      = [(d + 1, (Homophone i1 t, (bs, bs))) | 
           (i1, i2@(m, n)) <- pairs, 
           isIndicator HomophoneInd i1,
           let bs = (1, 11),
           t@((_, d), _) <- getHomophoneArgTrees pCache i2,
           d < maxTreeDepth]

    --
    -- E.g. hover around and -> handover
    --
    makeInsertion :: Index -> Index -> Index -> Index -> [UnindexedTree]
    makeInsertion i1 i1' i2 i3
      = [(max d d' + 1, (Insertion i1 ind t t', (addBounds bs1 bs2, addBounds bs1' bs2'))) |
           t@((_, d), (_, (bs1, bs1')))  <- getInsertionArg1Trees pCache i2,
           d < maxTreeDepth,
           t'@((_, d'), (_, (bs2, bs2'))) <- getInsertionArg2Trees pCache i3,
           d' < maxTreeDepth]
      where
        ind = if i1' == noInd
              then getString pCache i1
              else getString pCache i1 ++ "..." ++ getString pCache i1'

    --
    -- E.g. insert X into Y, into Y insert X
    --
    parseInsertion' :: Index -> [UnindexedTree]
    parseInsertion' index@(i, j) 
        = concat $ 
            -- E.g. inject X into Y
            [makeInsertion i1 i2 i3 i4 |
               (i1, i3, i2, i4) <- quadruples, 
               isIndicator Inject i1,
               isIndicator Into i2] ++
            -- E.g. locate X in Y
            [makeInsertion i1 i2 i3 i4 |
               (i1, i3, i2, i4) <- quadruples, 
               isIndicator Locate i1,
               isIndicator Into i2] ++
            -- E.g. put X around Y
            [makeInsertion i1 i2 i3 i4 |
               (i1, i4, i2, i3) <- quadruples, 
               isIndicator Locate i1,
               isIndicator Around i2] ++
            -- E.g. X and/with/has Y around
            [makeInsertion i1 i2 i3 i4 |
               (i3, i1, i4, i2) <- quadruples, 
               isIndicator Locate i1,
               isIndicator Around i2] ++
            -- E.g. X allowing Y inside
            [makeInsertion i1 i2 i3 i4 |
               (i4, i1, i3, i2) <- quadruples, 
               isIndicator Locate i1,
               isIndicator Into i2] ++
            -- E.g. X with Y injected
            [makeInsertion i1 i2 i3 i4 |
               (i4, i1, i3, i2) <- quadruples, 
               isIndicator Locate i1,
               isIndicator Inject i2] ++
            -- E.g. X having Y around
            -- E.g. X with Y surrounding
            [makeInsertion i1 i2 i3 i4 |
               tup@(i3, i1, i4, i2) <- quadruples, 
               isIndicator Locate i1,
               isIndicator Around i2 || isIndicator Surround i2] ++
            -- E.g. inject X with Y
            [makeInsertion i1 i2 i3 i4 |
               (i1, i4, i2, i3) <- quadruples, 
               isIndicator Inject i1,
               isIndicator With i2] ++
            -- E.g. surround X with Y
            [makeInsertion i1 i2 i3 i4 |
               (i1, i3, i2, i4) <- quadruples, 
               isIndicator Surround i1,
               isIndicator With i2] ++
            -- E.g. around X put Y
            [makeInsertion i1 i2 i3 i4 |
               (i1, i3, i2, i4) <- quadruples, 
               isIndicator Around i1,
               isIndicator Locate i2] ++
            -- E.g. inside X put Y
            [makeInsertion i1 i2 i3 i4 |
               (i1, i4, i2, i3) <- quadruples, 
               isIndicator Into i1,
               isIndicator Locate i2] 
      where
        quadruples = split4Indices index

    parseInsertion :: Triples -> [UnindexedTree]
    parseInsertion triples 
      = concat $ 
          [makeInsertion i1 noInd i2 i3 |
             (i1, i2, i3) <- triples, 
             isIndicator InsertionIndL1 i1] ++
          [makeInsertion i1 noInd i2 i3 |
             (i1, i3, i2) <- triples, 
             isIndicator InsertionIndL2 i1] ++
          [makeInsertion i1 noInd i2 i3 |
             (i2, i1, i3) <- triples, 
             isIndicator InsertionIndC1 i1] ++
          [makeInsertion i1 noInd i2 i3 |
             (i3, i1, i2) <- triples, 
             isIndicator InsertionIndC2 i1] ++
          [makeInsertion i1 noInd i2 i3 |
             (i2, i3, i1) <- triples, 
             isIndicator InsertionIndR1 i1] ++
          [makeInsertion i1 noInd i2 i3 |
             (i3, i2, i1) <- triples, 
             isIndicator InsertionIndR2 i1] 

    --
    -- E.g. ink from linking -> ling
    -- Important note: The upper bounds can be negative if no valid 
    -- subtractions are possible, e.g. for the text expansion case
    -- subtract "mistake" "a" gives bounds (1, -6). We must
    -- preserve the negative bound so that a node higher up
    -- the tree can prune it out.
    --
    makeSubtraction :: Index -> Index -> Index -> [UnindexedTree]
    makeSubtraction i1 i2 i3
      = [(max d d' + 1, (Subtraction i1 t t', (subBounds bs1 bs2, subBounds bs1' bs2'))) |
           t@((_, d), (_, (bs1, bs1')))  <- getSubtractionArg1Trees pCache i2,
           d < maxTreeDepth,
           t'@((_, d'), (_, (bs2, bs2'))) <- getSubtractionArg2Trees pCache i3,
           d' < maxTreeDepth]
        where
          subBounds (l, u) (l', u')
            = (max (l' - u) 1, u' - l)

    parseSubtraction :: Triples -> [UnindexedTree]
    parseSubtraction triples 
      = concat $ 
          [makeSubtraction i1 i2 i3 |
             (i1, i2, i3) <- triples, 
             isIndicator SubtractionIndL1 i1] ++
          [makeSubtraction i1 i2 i3 |
             (i1, i3, i2) <- triples, 
             isIndicator SubtractionIndL2 i1] ++
          [makeSubtraction i1 i2 i3 |
             (i2, i1, i3) <- triples, 
             isIndicator SubtractionIndC1 i1] ++
          [makeSubtraction i1 i2 i3 |
             (i3, i1, i2) <- triples, 
             isIndicator SubtractionIndC2 i1] ++
          [makeSubtraction i1 i2 i3 |
             (i2, i3, i1) <- triples, 
             isIndicator SubtractionIndR1 i1] ++
          [makeSubtraction i1 i2 i3 |
             (i3, i2, i1) <- triples, 
             isIndicator SubtractionIndR2 i1] 

    --
    -- E.g. man with kind -> mankind, before man hit -> hitman
    --
    -- Note: this parses *indicated* charades only. 
    --
    makeCharade :: Index -> Index -> Index -> [UnindexedTree]
    makeCharade i1 i2 i3
      = [(max d d' + 1, (Charade i1 [t, t'], (addBounds bs1 bs2, addBounds bs1' bs2'))) |
           t@((_, d), (_, (bs1, bs1')))  <- getIndicatedCharadeArg1Trees pCache i2,
           d < maxTreeDepth,
           t'@((_, d'), (_, (bs2, bs2'))) <- getIndicatedCharadeArg2Trees pCache i3,
           d' < maxTreeDepth]

    parseIndicatedCharade :: Triples -> [UnindexedTree]
    parseIndicatedCharade triples 
      = concat $ 
          [makeCharade i1 i2 i3 |
             (i1, i2, i3) <- triples, 
             isIndicator CharadeIndL1 i1] ++
          [makeCharade i1 i2 i3 |
             (i1, i3, i2) <- triples, 
             isIndicator CharadeIndL2 i1] ++
          [makeCharade i1 i2 i3 |
             (i2, i1, i3) <- triples, 
             isIndicator CharadeIndC1 i1] ++
          [makeCharade i1 i2 i3 |
             (i3, i1, i2) <- triples, 
             isIndicator CharadeIndC2 i1] ++
          [makeCharade i1 i2 i3 |
             (i2, i3, i1) <- triples, 
             isIndicator CharadeIndR1 i1] ++
          [makeCharade i1 i2 i3 |
             (i3, i2, i1) <- triples, 
             isIndicator CharadeIndR2 i1] 

-------------- QUALITY EVALUATION -------------

--
-- q is the previously-computed quality score.
-- The idea is to promote parses that we would solve anyway, as 
-- we have a synonym for the definition. It doesn't always give the
-- best parse, e.g. "provide food ..." may resolve correctly to 
-- cater but treat "food" as part of the wordplay, giving a more
-- complex explanation. In these rare cases it would be better
-- to pick "provide food" as the definition. Alas...
-- We may end up with the same quality score, same def and same
-- link indicator, in which case we prefer trees where the 
-- indicator words are spread out evenly, rather than, e.g. those
-- where one indicator hogs them all. Example: "called up head of"
-- as a reversal indicator, which looks suspicious (should be called up).
--
defProperties :: ParserCache -> String -> Parse -> Int -> Int
defProperties pCache answer p@(d, _, t) q
  | Set.member answer (table <!!> length answer) = 0
  | hasQuestionablePrefix def = q + 4
  | otherwise = q + length def + maxIndLength t
  where
    def = getWords pCache d
    table = getSynonymTable pCache d

hasQuestionablePrefix (w : _ : _)
  = elem w questionablePrefixes
hasQuestionablePrefix _
  = False

maxIndLength :: ParseTree -> Int
maxIndLength (_, (SpareWord _ _, _))
  = 0
maxIndLength (_, (Synonym _, _))
  = 0
maxIndLength (_, (Hyponym i _, _))
  = indLength i
maxIndLength (_, (Anagram i _, _))
  = indLength i
maxIndLength (_, (AnagramInf i _ _, _))
  = indLength i
maxIndLength (_, (Abbreviation i _, _))
  = indLength i
maxIndLength (_, (Odds i _, _))
  = indLength i
maxIndLength (_, (Evens i _, _))
  = indLength i
maxIndLength (_, (FirstLetters i t, _))
  = max (indLength i) (maxIndLength t)
maxIndLength (_, (LastLetters i t, _))
  = max (indLength i) (maxIndLength t)
maxIndLength (_, (MiddleLetters i t, _))
  = max (indLength i) (maxIndLength t)
maxIndLength (_, (EndLetters i t, _))
  = max (indLength i) (maxIndLength t)
maxIndLength (_, (Duplicate i _ t, _))
  = max (indLength i) (maxIndLength t)
maxIndLength (_, (Homophone i t, _))
  = max (indLength i) (maxIndLength t)
maxIndLength (_, (Reversal i t, _))
  = max (indLength i) (maxIndLength t)
maxIndLength (_, (Rotation i t, _))
  = max (indLength i) (maxIndLength t)
maxIndLength (_, (Insertion i _ t t', _))
  = maximum [indLength i, maxIndLength t, maxIndLength t']
maxIndLength (_, (Subtraction i t t', _))
  = maximum [indLength i, maxIndLength t, maxIndLength t']
maxIndLength (_, (Charade i ts, _))
  = maximum (indLength i : map maxIndLength ts)

--
-- Low scores are better.
--
quality :: Parse -> Int
quality (_, _, pt)
  = quality' pt

quality' :: ParseTree -> Int
quality' (_, (SpareWord _ _, _))
  = 1
quality' (_, (Synonym _, _))
  = 1
quality' (_, (Hyponym _ _, _))
  = 2
quality' (_, (Anagram _ t, _))
  = 1
quality' (_, (AnagramInf _ t t', _))
  = 1
quality' (_, (Abbreviation _ _, _))
  = 1
quality' (_, (Odds _ _, _))
  = 1
quality' (_, (Evens _ _, _))
  = 1
--
-- Take-outs across charades are very rare, except for 
-- middle letters (e.g. hidden words). The weighting
-- is designed to give preference to middle letters across
-- charades (the classic hidden word) over middle letters
-- of single words, which are less common.
--
-- In some cases the tree depth is fixed; if not then
-- we penalise deep trees via the factor 2* in the
-- recursion. Reversals are not penalised in this way, as 
-- we want to pick them over take-outs. Two-way charades 
-- should be on par with insertions.
--
quality' (_, (FirstLetters _ t@(_, (Charade _ _, _)), _))
  = 10 + quality' t
quality' (_, (FirstLetters _ t, _))
  = 1 + 2 * quality' t
quality' (_, (LastLetters _ t@(_, (Charade _ _, _)), _))
  = 10 + 2 * quality' t
quality' (_, (LastLetters _ t, _))
  = 5 + 2 * quality' t
-- quality' (_, (MiddleLetters _ t@(_, (Charade _ _, _)), _))
--   = 10 + 2 * quality' t
quality' (_, (MiddleLetters _ t@(_, (Synonym _, _)), _))
  = 2 + 2 * quality' t
quality' (_, (MiddleLetters _ t, _))
  = 1 + 2 * quality' t
quality' (_, (EndLetters _ t@(_, (Charade _ _, _)), _))
  = 10 + 2 * quality' t
quality' (_, (EndLetters _ t, _))
  = 5 + 2 * quality' t
quality' (_, (Duplicate _ _ t, _))
  = 1 + 2 * quality' t
quality' (_, (Homophone _ t, _))
  = 1 + 2 * quality' t
quality' (_, (Reversal _ t, _))
  = 1 + 2 * quality' t
quality' (_, (Rotation _ t, _))
  = 1 + 2 * quality' t 
quality' (_, (Insertion _ _ t t', _))
  = 1 + 2 * max (quality' t) (quality' t')
quality' (_, (Subtraction _ t t', _))
  = 8 + 2 * max (quality' t) (quality' t')
-- We want X + a + Y to be preferred over X + Y if it happens
-- that X ends in a. E.g. (start to Harvest) A crop >> (start to
-- HArvest) (a crop). However, it's hard to manage this at 
-- parse time. So we instead penalise longer charades, e.g 
-- we want UTAHAN(UTA(rotation[cycling] TAU=character) +[with] 
-- HAN=chinese) rather than (U=character + TA(rotation[cycling] 
-- AT=with) + HAN=chinese)
-- Also, a simple 2-way charade should be better than an 
-- insertion, say. 
quality' (_, (Charade _ ts, _))
  = 2 * (n - 2) + 2 * maximum (map quality' ts)
  where
    n = length ts

--
-- Used to define an ordering on trees.
--
conIndex :: ParseTree -> Int
conIndex (_, (SpareWord _ _, _))
  = 17
conIndex (_, (Rotation _ _, _))
  = 18
conIndex (_, (Synonym _, _))
  = 0
conIndex (_, (Hyponym _ _, _))
  = 1
conIndex (_, (Anagram _ _, _))
  = 2
conIndex (_, (AnagramInf _ _ _, _))
  = 3
conIndex (_, (Abbreviation _ _, _))
  = 4
conIndex (_, (Odds _ _, _))
  = 5
conIndex (_, (Evens _ _, _))
  = 6
conIndex (_, (FirstLetters _ _, _))
  = 7
conIndex (_, (LastLetters _ _, _))
  = 8
conIndex (_, (MiddleLetters _ _, _))
  = 9
conIndex (_, (EndLetters _ _, _))
  = 10
conIndex (_, (Duplicate _ _ _, _))
  = 11
conIndex (_, (Homophone _ _, _))
  = 12
conIndex (_, (Reversal _ _, _))
  = 13
conIndex (_, (Insertion _ _ t _, _))
  = 14
conIndex (_, (Subtraction _ t _, _))
  = 15
conIndex (_, (Charade _ _, _))
  = 16

--
-- Note: the indicator may be (-1, -1)
--
indLength (i, j)
  | i > 0     = j - i + 1
  | otherwise = 0

hasNoisePrefix pCache index@(i, _)
  = indLength index > 1 &&
    elem (getString pCache (i, i)) prefixNoiseWords

--
-- We ignore noise prefixes when comparing definitions, 
-- e.g. "animal" and "an animal" are taken to be the same.
--
equalSyn pCache i i' 
  | i == i'             = True
  | strip i == strip i' = True
  | otherwise           = False
  where
    strip index@(i, j)
      | hasNoisePrefix pCache index 
        = (i + 1, j)
      | otherwise 
        = index 

--
-- The ordering function on parses
--
compareParses :: ParserCache -> Parse -> Parse -> Ordering
compareParses pCache (i, _, t) (i', _, t')
  = compareTrees t t'
  where
    --
    -- The ordering function on parse trees. Synonyms may be preceded
    -- by a prefix like a, an, the, ... in which case the comparison
    -- is based on the text with the prefix removed, e.g. a friend
    -- and friend are equal.
    --
    compareTrees' :: ParseTree -> ParseTree -> ParseTree -> ParseTree ->
                    Ordering
    compareTrees' t1 t2 t1' t2'
      | c == LT   = LT
      | c == GT   = GT
      | otherwise = compareTrees t2 t2'
      where
        c = compareTrees t1 t1'

    compareTrees :: ParseTree -> ParseTree -> Ordering
    compareTrees t t'
      | index < index' = LT
      | index > index' = GT
      where 
        index  = conIndex t 
        index' = conIndex t'
    compareTrees (_, (SpareWord s i, _)) (_, (SpareWord s' i', _))
      = compare (s, i) (s', i')
    compareTrees (_, (Synonym j, _)) (_, (Synonym j', _))
      | equalSyn pCache j j' = EQ
      | otherwise            = compare j j'
    compareTrees (_, (Hyponym _ j, _)) (_, (Hyponym _ j', _))
      = compare j j'
    compareTrees (_, (Anagram _ t, _)) (_, (Anagram _ t', _))
      = compareTrees t t'
    compareTrees (_, (AnagramInf _ t1 t2, _)) (_, (AnagramInf _ t1' t2', _))
      = compareTrees' t1 t2 t1' t2'
    compareTrees (_, (Abbreviation _ j, _)) (_, (Abbreviation _ j', _))
      = compare j j'
    compareTrees (_, (Odds _ j, _)) (_, (Odds _ j', _))
      = compare j j'
    compareTrees (_, (Evens _ j, _)) (_, (Evens _ j', _))
      = compare j j'
    compareTrees (_, (FirstLetters _ t, _)) (_, (FirstLetters _ t', _))
      = compareTrees t t'
    compareTrees (_, (LastLetters _ t, _)) (_, (LastLetters _ t', _))
      = compareTrees t t'
    compareTrees (_, (MiddleLetters _ t, _)) (_, (MiddleLetters _ t', _))
      = compareTrees t t'
    compareTrees (_, (EndLetters _ t, _)) (_, (EndLetters _ t', _))
      = compareTrees t t'
    compareTrees (_, (Duplicate _ _ t, _)) (_, (Duplicate _ _ t', _))
      = compareTrees t t'
    compareTrees (_, (Homophone _ t, _)) (_, (Homophone _ t', _))
      = compareTrees t t'
    compareTrees (_, (Reversal _ t, _)) (_, (Reversal _ t', _))
      = compareTrees t t'
    compareTrees (_, (Rotation _ t, _)) (_, (Rotation _ t', _))
      = compareTrees t t'
    compareTrees (_, (Insertion i1 i2 t1 t2, _)) (_, (Insertion i1' i2' t1' t2', _))
      = compareTrees' t1 t2 t1' t2'
    compareTrees (_, (Subtraction _ t1 t2, _)) (_, (Subtraction _ t1' t2', _))
      = compareTrees' t1 t2 t1' t2'
    compareTrees (_, (Charade _ ts, _)) (_, (Charade _ ts', _))
      = compareTrees'' ts ts'

    compareTrees'' (t : ts) (t' : ts') 
      | compareTrees t t' == LT = LT
      | compareTrees t t' == GT = GT
      | otherwise               = compareTrees'' ts ts'
    compareTrees'' [] []
      = EQ
    compareTrees'' [] _
      = LT
    compareTrees'' _ []
      = GT

sameParse :: ParserCache -> Parse -> Parse -> Bool
sameParse pCache p p'
  = compareParses pCache p p' == EQ

--
-- The scoring function - used to pick a winner among trees
-- with the same computational structure, but with different 
-- indicators. Small scores are better.
--
-- If we have a synonym for the answer then stop. Otherwise, 
-- score the def and indicator; note that the trees are the 
-- same by construction.
-- Note: the quality metric is still attached, as it's needed
-- later.
--
scoreParse pCache answer (d, i, t)
  | Set.member answer (table <!!> length answer) = score - 10
  | otherwise = score
  where
    score = scoreInd i + scoreDefStr ws
    table = getSynonymTable pCache d

    ws = getWords pCache d
    --
    -- Short definitions are better. Definitions preceded by, e.g.
    -- a, and, the etc. are tops. We want these to win over other
    -- parses where, e.g. the 'a' is at the end of an indicator, 
    -- e.g. X Y inside an animal -> ind = inside, def = an animal,
    -- not ind = inside an, def = animal
    --
    scoreDefStr (w : _ : _)
      | elem w prefixNoiseWords = -5
    scoreDefStr s
      = length s

    --
    -- The 0 case only applies to definition links
    -- This penalises short link indicators over long ones.
    -- Should it?!
    --
    scoreInd i
      | len == 0
        = 5
      | len > 1 && elem (last ind) prefixNoiseWords
        = 10
      | otherwise
        = -len
      where
        len = indLength i
        ind = getWords pCache i

    scoreTree :: ParseTree -> Int
    scoreTree (_, (SpareWord _ _, _)) 
      = 0
    scoreTree (_, (Synonym _, _)) 
      = 0
    scoreTree (_, (Hyponym i j, _)) 
      = scoreInd i
    scoreTree (_, (Anagram i t, _)) 
      = scoreInd i + scoreTree t
    scoreTree (_, (AnagramInf i t1 t2, _)) 
      = scoreInd i + scoreTree t1 + scoreTree t2
    scoreTree (_, (Abbreviation i j, _)) 
      = scoreInd i
    scoreTree (_, (Odds i j, _)) 
      = scoreInd i
    scoreTree (_, (Evens i j, _))
      = scoreInd i
    scoreTree (_, (FirstLetters i t, _))
      = scoreInd i + scoreTree t
    scoreTree (_, (LastLetters i t, _))
      = scoreInd i + scoreTree t
    scoreTree (_, (MiddleLetters i t, _))
      = scoreInd i + scoreTree t
    scoreTree (_, (EndLetters i t, _)) 
      = scoreInd i + scoreTree t
    scoreTree (_, (Duplicate i s t, _))
      = scoreInd i + scoreTree t
    scoreTree (_, (Homophone i t, _)) 
      = scoreInd i + scoreTree t
    scoreTree (_, (Reversal i t, _))
      = scoreInd i + scoreTree t
    scoreTree (_, (Rotation i t, _))
      = scoreInd i + scoreTree t
    -- We don't score the second indictor...
    scoreTree (_, (Insertion i i' t t', _))
      = scoreInd i + scoreTree t + scoreTree t'
    scoreTree (_, (Subtraction i t t', _))
      = scoreInd i + scoreTree t + scoreTree t'
    scoreTree (_, (Charade i ts, _))
      = sum (map scoreTree ts)

