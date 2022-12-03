import Data.Char
import System.IO
import JSON
import Control.Monad

text c = reverse (drop 2 (dropWhile (/='(') (reverse c)))
find s = unstring . snd . head . (filter ((==s).fst))
unstring (JSString s) = s

parse file file'
  = do
      h <- openFile file' WriteMode
      contents <- readFile file 
      let ls = lines contents
      hPutStrLn h "clues = ["
      mapM_ (parseLine h) ls
      hPutStrLn h " ]"

isRight (Right _) = True
isRight _         = False

quote '\"' = "\\\""
quote c    = [c]

parseLine h s 
  = do 
      let p = parseJSON s
      let Right (JSObject ps) = parseJSON s
      let c = concatMap quote (text (find "clue" ps))
      let en = find "enumeration" ps
      let ans = find "answer" ps
      let noRefs = null (filter isDigit c)
      when (isRight p && noRefs && not (elem ',' en))
        (hPutStrLn h (" (0, (\"" ++ c ++ "\"," ++
                  filter (/= ')') (tail en) ++ "), \"" ++
                  ans ++ "\"),"))
