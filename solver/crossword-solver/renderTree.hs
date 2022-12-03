import Types
import Parser
import CacheFunctions
import Clues
import Solver
import System.IO
import Debug.Trace

graph :: ParseTree -> ParserCache -> Int -> String -> IO ()
graph t cache depth f
  = do
      h <- openFile f WriteMode
      hPutStrLn h "digraph BST {"
      hPutStrLn h "  graph [center=true, margin=0.01, width=1,ratio=.67];"
      hPutStrLn h "  node [fontname=\"Arial\",shape=plaintext];"
      hPutStrLn h "  node [margin=0.01,fillcolor=\"white\"];"
      hPutStrLn h "  edge [dir=none];"
      printTree t depth cache h
      hPutStrLn h "}"
      hClose h

showTreeId (id, _)
  = "N" ++ showId id

showId (i, j, k)
  = show i ++ show j ++ show k

-- Number of invisible nodes to add given the tree depth and level.
invNodes 2 1
  = 3
invNodes 2 2
  = 0
invNodes 3 1
  = 5
invNodes 3 2
  = 3
invNodes 3 3
  = 0
invNodes 4 1
  = 7
invNodes 4 2
  = 5
invNodes 4 3
  = 3
invNodes 4 4
  = 0
invNodes d k
  = error ("invNodes " ++ show d ++ " " ++ show k)  

addInvNodes :: Int -> TreeId -> [ParseTree] -> Int ->
               [Either String (ParseTree, Int)]
addInvNodes n id ts level
  = add ts 
  where
    n' = n - length ts
    invNodes = map (Left . (("I" ++ showId id) ++)) (map show [1..n'])
    (is, is') = splitAt (n' `div` 2) invNodes
    addLevel t = Right (t, level)
    add []
      = []
    add [t]
      = is ++ [addLevel t] ++ is'
    add [t, t']
      = [addLevel t] ++ invNodes ++ [addLevel t']
    add [t1, t2, t3]
      = [addLevel t1] ++ is ++ [addLevel t2] ++ is' ++ [addLevel t3]

printTree :: ParseTree -> Int -> ParserCache -> Handle -> IO ()
printTree t depth cache h
  = do
      edges <- printNodes [Right (t, 0)]
      mapM_ printOneEdge edges 
  where
    printOneEdge (id, id'@('I' : _))
      = do 
          hPutStrLn h ("  " ++ id ++ " -> " ++ id' ++ 
                    "[weight=100 style=invis];")
    printOneEdge (id, id')
      = do
          hPutStrLn h ("  " ++ id ++ " -> " ++ id' ++ ";")

    printNodes :: [Either String (ParseTree, Int)] -> IO [(String, String)]
    printNodes []
      = do
          return []
    printNodes (Left s : ts)
      = do
          hPutStrLn h ("  " ++ s ++ " [style=invis];")
          printNodes ts
    printNodes ((Right (t@(id, _), level)) : ts)
      = do
          ts' <- printNode t
          let n = invNodes depth (level + 1) 
          let ts'' = addInvNodes n id ts' (level + 1)
          es' <- printNodes (ts ++ ts'')
          return (map (makeEdge (showTreeId t)) ts'' ++ es')
      where
        makeEdge id (Left id')
          = (id, id')
        makeEdge id (Right (t, _))
          = (id, showTreeId t)

        indString ind
          = if ind == noInd
            then ""
            else "\\n[" ++ getString cache ind ++ "]"

        makeLabel label ind
          = " [label = \"" ++ label ++ ind ++ "\", fontsize=50];"

        printTerminal t ind txt label
          = printTerminal' t ind (getString cache txt) label

        printTerminal' t ind s label
          = do
              hPutStrLn h ("  " ++ showTreeId t ++ 
                           makeLabel label (indString ind))
              hPutStrLn h ("  " ++ showTreeId t ++ "1 " ++
                           makeLabel s "")
              hPutStrLn h ("  " ++ showTreeId t ++ " -> " ++ 
                           showTreeId t ++ "1;")
        
        printInternalNode id ind label
          = do
              hPutStrLn h ("  " ++ showTreeId t ++ 
                           makeLabel label (indString ind))

        printNode t@(id, (Synonym txt, _))
          = do
              printTerminal t noInd txt "Synonym"
              return []
        printNode t@(id, (ExampleOf ind txt, _))
          = do
              printTerminal t ind txt "Example"
              return []
        printNode t@(id, (Abbreviation ind s, _))
          = do
              printTerminal' t ind s "Abbreviation"
              return []
        printNode (id, (Anagram ind t, _))
          = do
              printInternalNode id ind "Anagram" 
              return [t]
        printNode (id, (AnagramInf ind t t', _))
          = do
              printInternalNode id ind "Anagram"
              return [t, t']
        printNode t@(id, (Odds ind txt, _))
          = do
              printTerminal t ind txt "Odds"
              return []
        printNode t@(id, (Evens ind txt, _))
          = do
              printTerminal t ind txt "Evens"
              return []
        printNode (id, (FirstLetters ind t, _))
          = do
              printInternalNode id ind "FirstLetters"
              return [t]
        printNode (id, (LastLetters ind t, _))
          = do
              printInternalNode id ind "LastLetters"
              return [t]
        printNode (id, (MiddleLetters ind t, _))
          = do
              printInternalNode id ind "MiddleLetters"
              return [t]
        printNode (id, (EndLetters ind t, _))
          = do
              printInternalNode id ind "EndLetters"
              return [t]
        printNode (id, (HiddenWord ind t, _))
          = do
              printInternalNode id ind "HiddenWord"
              return [t]
        printNode (id, (Duplicate ind _ t, _))
          = do
              printInternalNode id ind "Duplicate"
              return [t]
        printNode (id, (Homophone ind t, _))
          = do
              printInternalNode id ind "Homophone"
              return [t]
        printNode (id, (Reversal ind t, _))
          = do
              printInternalNode id ind "Reversal"
              return [t]
        printNode (id, (Insertion ind t t', _))
          = do
              printInternalNode id ind "Insertion"
              return [t, t']
        printNode (id, (Subtraction ind t t', _))
          = do
              printInternalNode id ind "Subtraction"
              return [t, t']
        printNode (id, (Charade ind ts, _))
          = do
              printInternalNode id ind "Charade"
              return ts
