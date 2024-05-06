{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}
module Main where
  
import AATree

--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- getContents

  -- split the data into words and build an AA tree
  -- use foldl
  undefined

  -- calculate and print statistics
  -- use fromIntegral/ceiling/logBase
  undefined

--------------------------------------------------------------------------------

