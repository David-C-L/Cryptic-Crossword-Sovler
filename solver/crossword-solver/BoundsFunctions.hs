module BoundsFunctions where

import Debug.Trace
import Databases
import Types

nullBounds :: Bounds
nullBounds = (0, 0)

boundsOverlap (l, u) (l', u')
  = not (l' > u || u' < l)

inBounds k (l, u)
  = k >= l && k <= u

addBounds :: Bounds -> Bounds -> Bounds
addBounds (x, y) (x', y')
  = (x + x', y + y')

infixl 6 <<< 
(l, u) <<< n = (max 0 (l - n), max 0 (u - n))

infixl 6 >>> 
(l, u) >>> n = (l + n, u + n)

infixl 6 <<+ 
(l, u) <<+ n = (l + n, u)

infixl 6 >>+ 
(l, u) >>+ n = (l, u + n)

infixl 6 <<- 
(l, u) <<- n = (max 0 (l - n), u)

infixl 6 >>- 
(l, u) >>- n = (l, max 0 (u - n))

geqMin :: Bounds -> Int -> Bool
geqMin (l, u) n 
  = n >= l

leqMax :: Bounds -> Int -> Bool
leqMax (l, u) n 
  = n <= u

satisfiesLen :: Bounds -> Int -> Bool
satisfiesLen (l, u) n
  = n >= l && n <= u

setMin :: Int -> Bounds -> Bounds
setMin n (l, u)
  = (n, u)

-- Sets min to the maximum of n and the current value
adjustMin :: Int -> Bounds -> Bounds
adjustMin n c@(l, u) 
  | n > l = (n, u)
  | otherwise = c


