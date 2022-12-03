import Data.Char
import Parser
import Evaluation
import Types
import System.IO
import Debug.Trace
import Benchmarks.Everyman
import Benchmarks.EverymanSolved
--import Benchmarks.Guardian
--import Benchmarks.GuardianSolved

head' []
  = []
head' sols
  = [head sols]

analyseAll :: [(_, Clue, String)] -> Int -> Int -> String -> IO()
analyseAll clues m n filename
  = do
      h <- openFile filename WriteMode
      vs <- mapM (analyse h) (zip [0..] (take (n - m + 1) (drop m clues')))
      hPutStrLn h (show (sumVecs vs))
      hClose h
  where
    clues' = map (\ (_, c, s) -> (c, s)) clues

analyse :: Handle -> (Int, (Clue, String)) -> IO [Int]
analyse h (n, (clue, answer))
  | len <= 12
    = do
        putStrLn (show n ++ ": ")
        analyseSols
  | otherwise
    = do
        return zeroVec
  where
    len = length (words (fst (clue)))
    analyseSols
      | null sols = do 
                      return zeroVec
      | otherwise = do 
                      return (makeVec (head sols)) -- (sumVecs (map makeVec sols))
      where
        answer' = map toLower answer
        (ps, pCache) = someParses clue False
        sols = evaluate Backwards ps pCache clue ""
    
addVecs :: [Int] -> [Int] -> [Int]
addVecs 
  = zipWith (+) 

sumVecs :: [[Int]] -> [Int]
sumVecs
  = foldr addVecs zeroVec

zeroVec :: [Int]
zeroVec 
  = replicate nCons 0

makeVec :: Solution -> [Int]
makeVec (_, _, _, t, _)
  = res
  where
    res = replicate k 0 ++ [1] ++ replicate (nCons - k) 0
    k = getConsIndex t

main 
  = do
      analyseAll everyman everymanSolved "results"

----------

nCons :: Int
nCons
  = 17

getConsIndex :: ParseTree -> Int
getConsIndex (_, (Synonym _, _))
  = 0
getConsIndex (_, (ExampleOf _ _, _))
  = 1
getConsIndex (_, (Abbreviation _ _, _))
  = 2
getConsIndex (_, (Anagram _ _, _))
  = 3
getConsIndex (_, (AnagramInf _ _ _, _))
  = 4
getConsIndex (_, (Odds _ _, _))
  = 5
getConsIndex (_, (Evens _ _, _))
  = 6
getConsIndex (_, (FirstLetters _ _, _))
  = 7
getConsIndex (_, (LastLetters _ _, _))
  = 8
getConsIndex (_, (MiddleLetters _ _, _))
  = 9
getConsIndex (_, (EndLetters _ _, _))
  = 10
getConsIndex (_, (Duplicate _ _ _, _))
  = 11
getConsIndex (_, (Homophone _ _, _))
  = 12
getConsIndex (_, (Reversal _ _, _))
  = 13
getConsIndex (_, (Insertion _ _ _, _))
  = 14
getConsIndex (_, (Subtraction _ _ _, _))
  = 15
getConsIndex (_, (Charade _ _, _))
  = 16

